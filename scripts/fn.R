# Fix problems in VR ICD code variables
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

# Match CDC ICD categories to dataset
match_cdc_icd <- function(df, icd) {
  icd <- icd |>
    filter(!grepl("(?i)^deleted|^mc only", status)) |>
    mutate(code_clean = gsub("[[:punct:]]", "", code)) |>
    select(-status) |>
    rename(icd_code = code)

  cod1 <- df |>
    select(rowid, cod) |>
    left_join(icd, by = c("cod" = "code_clean"))

  cod2 <- cod1 |>
    filter(is.na(icd_title)) |>
    select(-icd_code, -icd_title) |>
    mutate(cod = substr(cod, 1, 3)) |>
    left_join(icd, by = c("cod" = "code_clean"))

  cod_full <- cod1 |>
    drop_na(icd_title) |>
    bind_rows(cod2) |>
    rename(cod_matched_icd = cod)

  df |>
    left_join(cod_full, by = "rowid") |>
    relocate(cod_matched_icd, icd_code, icd_title, .after = cod)
}

# Match rankable ICD categories to dataset
match_rankable_icd <- function(x, ls) {
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

# Filter VR dataset based on Shiny inputs
filter_vrd <- function(
    df,
    cod_set,
    years_input,
    age_input,
    sex_input,
    race_input,
    hispanic_input,
    education_input,
    pregnancy_input,
    district_input) {
  df <- df |>
    filter(
      yod >= years_input[1],
      yod <= years_input[2]
    )

  df <- df |>
    filter(age %in% age_input)

  if (sex_input != "all") {
    df <- df |>
      filter(sex == sex_input)
  }

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

  if (length(pregnancy_input) != 1 || pregnancy_input != "all") {
    df <- df |>
      filter(pregnancy %in% pregnancy_input)
  }

  if (district_input != "all") {
    df <- df |>
      filter(council_district == district_input)
  }

  # If filters result in empty dataframe, return NULL
  if (is.null(pregnancy_input) || nrow(df) == 0) {
    return(NULL)
  }

  if (cod_set == "cdc") {
    df |>
      mutate(cod_var = cod_rankable_cdc)
  } else if (cod_set == "mod") {
    df |>
      mutate(cod_var = cod_rankable_mod)
  } else {
    stop("unexpected value for `cod_set`")
  }
}

# Configure filtered dataset for plot display
config_bump_data <- function(df, colors) {
  df <- df |>
    group_by(yod, cod_var) |>
    summarize(n = n(), .groups = "keep") |>
    ungroup() |>
    drop_na(cod_var)

  maxranks <- 52

  ls <- lapply(unique(df$yod), \(x) {
    df |>
      filter(yod == x) |>
      mutate(rank = rank(-n, ties.method = "first")) |>
      mutate(yrank = maxranks - rank) |>
      complete(yod) |>
      arrange(rank)
  })

  list_rbind(ls) |>
    left_join(colors, by = c("cod_var" = "cod"))
}

# Create leading COD bump chart
cod_bump_chart <- function(df, xvals, nranks, caption) {
  # x-axis scale
  xseq <- xvals[1]:xvals[2]

  xexp <- 4

  xbrk <- seq(xseq[1], xseq[length(xseq)] + xexp)

  xlab <- c(xseq, rep("", xexp))

  # y-axis scale
  yseq <- df |>
    filter(rank %in% 1:nranks) |>
    distinct(rank, .keep_all = TRUE) |>
    select(rank, yrank)

  # x-axis title position
  xpos <- .5 - (.97 / (length(xbrk) - 1) * xexp / 2)

  # Filter data for right side labels
  dflabr <- df |>
    filter(yod == max(xseq), rank %in% 1:nranks)

  ties <- unique(dflabr$n[duplicated(dflabr$n)])

  dflabr <- dflabr |>
    mutate(label = if_else(
      n %in% ties,
      paste0(cod_var, " (", n, ")*"),
      paste0(cod_var, " (", n, ")")
    ))

  # Filter data for plot labels
  dflabp <- df |>
    filter(
      yod != max(xseq),
      rank %in% 1:nranks,
      !cod_var %in% dflabr$cod_var
    ) |>
    arrange(desc(yod)) |>
    distinct(cod_var, .keep_all = TRUE)

  # Base text size
  size <- 20

  # Responsive labels (right side)
  maxranks <- sapply(unique(df$yod), \(x) {
    length(unique(df$cod_var[df$yod == x]))
  }) |>
    max(na.rm = TRUE)

  addranks <- case_when(
    maxranks < 10 ~ 0,
    all(maxranks < nranks & maxranks > 10) ~ maxranks - 10,
    .default = nranks - 10
  )

  label_size <- (size - .35 * addranks) / 3

  # Responsive lines and points
  line_size <- 6; pt_size1 <- 10; pt_size2 <- 4

  if (addranks > 10) {
    line_size <- 4; pt_size1 <- 6; pt_size2 <- 2
  }

  # Text
  title <- "Ranked leading causes of death in Kansas City, MO"

  # Plot
  df |>
    ggplot(aes(
      x = yod,
      y = yrank,
      color = colors,
      group = cod_var
    )) +
    geom_linerange(
      aes(xmin = min(yod), xmax = max(yod), y = yrank),
      linewidth = .25,
      color = "#ccc"
    ) +
    ggbump::geom_bump(
      linewidth = line_size,
      smooth = 8
    ) +
    geom_point(size = pt_size1) +
    geom_point(size = pt_size2, color = "white") +
    geom_textbox( # right side labels
      aes(label = label),
      size = label_size,
      width = unit(xexp / length(xbrk), "npc"),
      lineheight = .8,
      fontface = "bold",
      data = dflabr,
      x = xvals[2] + .25,
      hjust = 0,
      box.color = NA,
      fill = NA
    ) +
    ggrepel::geom_label_repel( # plot labels
      aes(label = str_wrap(cod_var, 30)),
      size = 5,
      fill = "#ffffffdd",
      lineheight = .8,
      fontface = "bold",
      data = dflabp,
      hjust = .5,
      direction = "y",
      min.segment.length = Inf
    ) +
    coord_cartesian(
      xlim = c(min(xbrk), max(xbrk)),
      ylim = c(min(yseq$yrank), max(yseq$yrank))
    ) +
    scale_x_continuous(
      breaks = xbrk,
      labels = xlab,
      expand = expansion(mult = .015)
    ) +
    scale_y_continuous(
      breaks = yseq$yrank,
      labels = yseq$rank,
      expand = expansion(mult = c(.05 - .002 * addranks, .05))
    ) +
    scale_color_identity() +
    labs(
      x = "Year",
      y = "Rank",
      title = title,
      caption = caption
    ) +
    theme_minimal(base_size = size) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(
        hjust = xpos,
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        margin = margin(r = 10)
      ),
      panel.grid = element_blank(),
      plot.caption = element_textbox_simple(
        color = "#555",
        size = 14,
        hjust = 0,
        margin = margin(t = 10, b = 5)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

# Create bump chart caption
cod_bump_caption <- function(names, inputs, filters, ages) {
  # Combine elements of names, inputs, and filters lists
  ls <- lapply(names(filters), \(i) {
    if (length(inputs[[i]]) == 1) {
      paste0(names[[i]], ": ", names(which(inputs[[i]] == filters[[i]])))
    } else {
      paste0(names[[i]], ": ", paste(
        sapply(inputs[[i]], \(x) names(which(x == filters[[i]]))),
        collapse = "; "
      ))
    }
  })

  selections <- paste(unlist(ls), collapse = " | ")

  selections <- paste0(
    "**Categories selected:** ",
    "Age: ", ages[1], " to ", ages[length(ages)], " | ",
    selections
  )

  note <- "**Note:** Labels on the right side of the plot name the rankable causes of death (CODs) and the number of deaths in that category for the final year displayed. Tied counts are ranked by their first appearance in the data and are denoted by an asterisk (*). Labels over points in the plot name rankable CODs that are not in the top ranked CODs in the final year displayed."

  src <- "**Data source:** Missouri Department of Health and Senior Services Vital Records."

  paste(
    note,
    selections,
    src,
    sep = "<br>"
  )
}

# Summarize filtered dataset by rankable COD for table display
rankable_cod_summary <- function(df, year, cod_name, cod_list) {
  df |>
    filter(
      cod_var == cod_name,
      yod == year
    ) |>
    group_by(icd_code, icd_title) |>
    summarize(n = n(), .groups = "keep") |>
    arrange(desc(n))
}

# Create COD breakdown table
cod_table <- function(df, year, cod_name) {
  title <- paste0(cod_name, ", ", year)

  df |>
    datatable(
      colnames = c(
        "ICD-10 code" = "icd_code",
        "Cause of death" = "icd_title",
        "Count" = "n"
      ),
      filter = "top",
      caption = htmltools::tags$caption(
        style = "
          caption-side: top;
          text-align: left;
          color: black;
          font-size: 150%;
        ",
        title
      ),
      options = list(
        dom = "t",
        paging = FALSE
      )
    )
}

# Plot annual deaths
plot_ann_deaths <- function(df, type = c("n", "pct")) {
  df <- df |>
    pivot_longer(
      cols = !year,
      names_to = "type",
      values_to = "n"
    )

  fontsize1 <- "1.2em !important"
  fontsize2 <- "1em !important"

  p <- df |>
    hchart("column", hcaes(
      x = factor(year),
      y = n,
      group = type
    )) |>
    hc_xAxis(
      title = list(
        text = "Year",
        style = list(fontSize = fontsize1)
      ),
      labels = list(
        style = list(fontSize = fontsize2)
      )
    ) |>
    hc_legend(itemStyle = list(fontSize = fontsize1))

  if (type == "n") {
    jsfn <- JS(
      "function() {
        const num = this.y;
        return num.toLocaleString('en-US');
      }"
    )

    p |>
      hc_yAxis(
        title = list(
          text = "Count",
          style = list(fontSize = fontsize1)
        ),
        labels = list(
          style = list(fontSize = fontsize2)
        )
      ) |>
      hc_tooltip(formatter = jsfn) #|>
      # hc_title(
      #   text = "Annual deaths in Kansas City, MO",
      #   align = "left"
      # )
  } else if (type == "pct") {
    jsfn <- JS(
      "function() {
        const num = this.y;
        return num.toLocaleString('en-US') + '%';
      }"
    )

    p |>
      hc_yAxis(
        title = list(
          text = "Percentage",
          style = list(fontSize = fontsize1)
        ),
        labels = list(
          style = list(fontSize = fontsize2)
        )
      ) |>
      hc_tooltip(formatter = jsfn) #|>
      # hc_title(
      #   text = paste(
      #     "Annual deaths in Kansas City, MO,",
      #     "as a percentage of population"
      #   ),
      #   align = "left"
      # )
  }
}

