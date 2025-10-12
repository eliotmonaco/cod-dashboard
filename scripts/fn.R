# Function to turn strings into ICD code sequences
get_icd_seq <- function(x) {
  x2 <- unlist(strsplit(x, ",\\s?")) # split on comma

  x2 <- gsub("\\*", "", x2) # remove "*"

  x2 <- lapply(x2, \(y) {
    if (grepl("-", y)) {
      y <- unlist(strsplit(y, "-")) # split on hyphen

      if (!all(grepl("^[[:upper:]]\\d{2}(\\.\\d)?$", y))) {
        print(x)
        stop("unexpected ICD pattern")
      }

      if (length(y) != 2) { # check for only 2 values
        stop("wrong number of values in initial sequence")
      }

      if (nchar(y[1]) != nchar(y[2])) { # check for same character count
        stop("different character count")
      }

      if (sum(grepl("\\.", y)) == 2) { # check for decimal
        decimal <- TRUE

        if (!all(grepl("^[[:upper:]]\\d{2}\\.\\d$", y))) {
          stop("different decimal placement")
        }
      } else if (sum(grepl("\\.", y)) == 0) {
        decimal <- FALSE
      } else {
        stop("only one decimal in sequence")
      }

      y <- sub("\\.", "", y) # remove decimal

      l <- unique(str_extract(y, "^[:upper:]")) # extract letter

      n <- str_extract(y, "\\d+") # extract number

      nchr <- nchar(n[1]) # get character count

      n <- as.numeric(n)

      if (length(l) == 1) { # if the initial letter is the same...
        s <- seq(n[1], n[2])

        s <- sprintf(paste0("%0", nchr, "d"), s)

        s <- paste0(l, s)
      } else { # if the initial letters are different...
        ltr <- LETTERS[which(LETTERS == l[1]):which(LETTERS == l[2])]

        s <- list()

        for (i in 1:length(ltr)) {
          if (i == 1) {
            s[[i]] <- seq(n[1], 10^nchr - 1)
          } else if (i != length(ltr)) {
            s[[i]] <- seq(0, 10^nchr - 1)
          } else if (i == length(ltr)) {
            s[[i]] <- seq(0, n[2])
          }

          s[[i]] <- sprintf(paste0("%0", nchr, "d"), s[[i]])

          s[[i]] <- paste0(ltr[i], s[[i]])
        }

        s <- unlist(s)
      }

      if (decimal) { # replace decimal
        sub("(^[[:upper:]]\\d{2})(\\d$)", "\\1.\\2", s)
      } else {
        s
      }
    } else {
      y
    }
  })

  unlist(x2)
}

fix_vital_icd <- function(df, row_id) {
  # Check uniqueness of row ID variable
  if (any(duplicated(df[[row_id]]))) {
    stop("`row_id` is not unique")
  }

  # Filter `df` for invalid ICD codes
  df2 <- df |>
    dplyr::filter(dplyr::if_any(
      c(cod, tidyselect::starts_with("mult")),
      ~ !stringr::str_detect(.x, "^[:alpha:]\\d{2,4}$")
    ))

  # Variables with ICD codes
  var <- stringr::str_which(colnames(df), "(?i)^cod$|^mult\\d{2}$")

  # Roll across columns with ICD codes and fix
  for (i in 1:length(var)) {
    # Split string at space
    pt1 <- apply(df2[, var[i]], 1, \(x) strsplit(x, "\\s")[[1]][1])
    pt2 <- apply(df2[, var[i]], 1, \(x) strsplit(x, "\\s")[[1]][2])

    # For all but the last column...
    if (i != length(var)) {
      df3 <- data.frame(
        x = pt2, # Code part 2 from column `var[i]`
        y = df2[[var[i + 1]]] # Whole value from column `var[i + 1]`
      )

      nm <- colnames(df2)[var[i + 1]]

      # Unite code parts from columns `var[i]` & `var[i + 1]`
      df2[, var[i + 1]] <- tidyr::unite(
        df3,
        col = nm,
        tidyselect::everything(),
        sep = "",
        remove = TRUE,
        na.rm = TRUE
      )
    }

    # Replace code part 1 from column `var[i]`
    df2[[var[i]]] <- pt1
  }

  df |>
    dplyr::rows_update(df2, by = row_id)
}

match_icd <- function(x, ls) {
  cod <- purrr::imap(ls, \(p, i) {
    if (grepl(p, x)) {
      i
    }
  })

  cod <- unlist(cod)

  if (!is.null(cod)) {
    cod
  } else {
    NA
  }
}

rank_cod <- function(x) {
  r <- rank(x, ties.method = "min")

  as.numeric(factor(r))
}

config_vrd <- function(
    df,
    # nranks_input,
    years_input,
    age_input,
    sex_input,
    race_input,
    hispanic_input,
    education_input,
    pregnancy_input,
    palette) {
  requireNamespace("tidyverse")

  df <- df |>
    filter(
      age %in% age_input,
      yod >= years_input[1],
      yod <= years_input[2],
      sex %in% sex_input
    )

  if (race_input != "all") {
    df <- df |>
      filter(.data[[race_input]] == "Y")
  }

  if (hispanic_input != "all") {
    df <- df |>
      filter(.data[[hispanic_input]] == "H")
  }

  if (education_input != "all") {
    df <- df |>
      filter(ed2010 == education_input)
  }

  if (pregnancy_input != "all") {
    df <- df |>
      filter(pregnancy == pregnancy_input)
  }

  df <- df |>
    group_by(yod, cod_rankable) |>
    summarize(n = n()) |>
    ungroup() |>
    drop_na(cod_rankable)

  maxranks <- 52

  ls <- lapply(unique(df$yod), \(x) {
    df |>
      filter(yod == x) |>
      mutate(rank = rank_cod(-n)) |>
      # mutate(yrank = nranks_input - rank) |>
      # filter(rank %in% 1:nranks_input) |>
      mutate(yrank = maxranks - rank) |>
      complete(yod) |>
      arrange(rank)
  })

  list_rbind(ls) |>
    left_join(palette, by = c("cod_rankable" = "cod"))
}

cod_bump_chart <- function(df, xvals, nranks) {
  requireNamespace("tidyverse")

  # x-axis scale
  xseq <- xvals[1]:xvals[2]

  xexp <- 3

  xbrk <- seq(xseq[1], xseq[length(xseq)] + xexp)

  xlab <- c(xseq, rep("", xexp))

  # y-axis scale
  yseq <- df |>
    filter(rank %in% 1:nranks) |>
    distinct(rank, .keep_all = TRUE) |>
    select(rank, yrank)

  # x-axis title position
  xpos <- .5 - (.95 / (length(xbrk) - 1) * xexp / 2)

  # Filter data for right side labels
  dftxt <- df |>
    filter(yod == max(xseq), rank %in% 1:nranks)

  ties <- unique(dftxt$yrank[duplicated(dftxt$yrank)])

  dftxt1 <- dftxt |> # untied labels
    filter(!yrank %in% ties)

  dftxt2 <- dftxt |> # tied labels
    filter(yrank %in% ties)

  # Filter data for plot labels
  dftxt3 <- df |>
    filter(
      yod != max(xseq),
      rank %in% 1:nranks,
      !cod_rankable %in% dftxt$cod_rankable
    ) |>
    arrange(desc(yod)) |>
    distinct(cod_rankable, .keep_all = TRUE)

  # Base text size
  size <- 20

  # Responsive label size
  texp <- ifelse(nranks > 10, nranks - 10, 0)

  label_size <- (size - (10 / nranks * texp)) / 3

  # Plot
  df |>
    ggplot(aes(
      x = yod,
      y = yrank,
      color = colors,
      group = cod_rankable
    )) +
    ggbump::geom_bump(
      linewidth = 6,
      smooth = 8
    ) +
    geom_point(size = 10) +
    geom_point(size = 4, color = "white") +
    ggrepel::geom_label_repel( # right side labels (tied)
      aes(label = str_wrap(cod_rankable, 30)),
      size = label_size,
      lineheight = .8,
      fontface = "bold",
      data = dftxt,
      hjust = 0,
      direction = "y",
      xlim = c(xvals[2] + .25, xvals[2] + 5),
      label.padding = .5,
      point.padding = 2,
      min.segment.length = unit(.4, "in")
    ) +
    # geom_text( # right side labels (untied)
    #   aes(label = str_wrap(cod_rankable, 30)),
    #   size = label_size,
    #   lineheight = .8,
    #   fontface = "bold",
    #   data = dftxt1,
    #   x = xvals[2] + .25,
    #   hjust = 0
    # ) +
    # ggrepel::geom_label_repel( # right side labels (tied)
    #   aes(label = str_wrap(cod_rankable, 30)),
    #   size = label_size,
    #   lineheight = .8,
    #   fontface = "bold",
    #   data = dftxt2,
    #   hjust = 0,
    #   direction = "y",
    #   xlim = c(xvals[2] + .25, xvals[2] + 5),
    #   label.padding = .5
    # ) +
    ggrepel::geom_label_repel( # plot labels
      aes(label = str_wrap(cod_rankable, 30)),
      size = 5,
      fill = "#ffffffdd",
      lineheight = .8,
      fontface = "bold",
      data = dftxt3,
      hjust = .5,
      direction = "both",
      min.segment.length = Inf
    ) +
    coord_cartesian(
      xlim = c(min(xbrk), max(xbrk)),
      ylim = c(min(yseq$yrank), max(yseq$yrank))#,
      # clip = "off"
    ) +
    scale_x_continuous(
      breaks = xbrk,
      labels = xlab,
      expand = expansion(mult = c(.025, .025))
    ) +
    scale_y_continuous(
      # breaks = sort(unique(df$yrank)),
      # labels = rev(sort(unique(df$rank)))
      breaks = yseq$yrank,
      labels = yseq$rank
    ) +
    scale_color_identity() +
    labs(
      x = "\nYear",
      y = "Rank\n"
    ) +
    theme_minimal(base_size = size) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(hjust = xpos),
      panel.grid = element_blank(),
      margins = margin(r = 0)
    )
}

