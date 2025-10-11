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

config_vrd <- function(df, nranks, years, ages, sex, race, palette) {
  requireNamespace("tidyverse")

  df <- df |>
    filter(
      age %in% ages,
      yod >= years[1], yod <= years[2],
      sex %in% sex
    ) |>
    mutate(yod = factor(yod, levels = years[1]:years[2]))

  if (race != "all") {
    df <- df |>
      filter(.data[[race]] == "Y")
  }

  df <- df |>
    group_by(yod, cod_rankable) |>
    summarize(n = n()) |>
    ungroup() |>
    drop_na(cod_rankable)

  ls <- lapply(unique(df$yod), \(x) {
    df |>
      filter(yod == x) |>
      mutate(rank = rank_cod(-n)) |>
      mutate(yrank = nranks - rank) |>
      filter(rank %in% 1:nranks) |>
      complete(yod) |>
      arrange(rank)
  })

  list_rbind(ls) |>
    mutate(yod = as.numeric(as.character(yod))) |>
    left_join(palette, by = c("cod_rankable" = "cod"))
}

cod_bump_chart <- function(df) {
  requireNamespace("tidyverse")

  # Extend x-axis to make room for labels
  yrs <- sort(unique(df$yod))

  xbrk <- c(yrs, max(yrs):(max(yrs) + 2))

  # Filter data for labels
  dftxt <- df |>
    filter(yod == max(yrs))

  ties <- unique(dftxt$yrank[duplicated(dftxt$yrank)])

  dftxt1 <- dftxt |> # untied labels
    filter(!yrank %in% ties)

  dftxt2 <- dftxt |> # tied labels
    filter(yrank %in% ties)

  # Base text size
  size <- 20

  # Responsive label text size
  maxrank <- max(df$rank)

  exp <- ifelse(maxrank > 10, maxrank - 10, 0)

  label_size <- (size - (10 / maxrank * exp)) / 3

  # Plot
  df |>
    ggplot(aes(
      x = yod,
      y = yrank,
      color = colors,
      group = cod_rankable
    )) +
    ggbump::geom_bump(linewidth = 6) +
    geom_point(size = 10) +
    geom_point(size = 4, color = "white") +
    geom_text(
      aes(label = str_wrap(cod_rankable, 40)),
      size = label_size,
      lineheight = .8,
      fontface = "bold",
      data = dftxt1,
      x = max(as.numeric(df$yod)) + .2,
      hjust = 0
    ) +
    ggrepel::geom_label_repel(
      aes(label = str_wrap(cod_rankable, 40)),
      size = label_size,
      lineheight = .8,
      fontface = "bold",
      data = dftxt2,
      hjust = 0,
      direction = "y",
      nudge_x = 2,
      label.padding = .5
    ) +
    coord_cartesian(xlim = c(min(xbrk), max(xbrk))) +
    scale_x_continuous(
      breaks = xbrk,
      labels = c(yrs, "", "", "")
    ) +
    scale_y_continuous(
      breaks = sort(unique(df$yrank)),
      labels = rev(sort(unique(df$rank)))
    ) +
    scale_color_identity() +
    labs(
      x = "\nYear",
      y = "Rank\n"
    ) +
    theme_minimal(base_size = size) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      margins = margin(r = 50)
    )
}

