
source("scripts/setup.R")

input <- list(
  ranks = 10,
  years = c(2014, 2024),
  rcodset = rcodlist[[2]],
  agebin = agelist[[1]],
  # agerng = 0:25,
  sex = sexlist[[1]],
  race = racelist[[1]],
  hispanic = hispaniclist[[1]],
  education = edlist[[1]],
  pregnancy = preglist[[1]],
  # pregnancy = c(preglist[[3]], preglist[[4]]),
  district = distlist[[1]]
)

if ("agerng" %in% names(input)) {
  age <- input$agerng
} else if (input$agebin == "all") {
  age <- 0:maxage
} else {
  age <- as.numeric(unlist(strsplit(input$agebin, ";")))
}

# Configure `vrd` based on Shiny inputs
vrd_fltr <- filter_vrd(
  vrd,
  rcod_set = input$rcodset,
  years_input = input$years,
  age_input = age,
  # age_input = unlist(strsplit(input$agebin, ";")),
  sex_input = input$sex,
  race_input = input$race,
  hispanic_input = input$hispanic,
  education_input = input$education,
  pregnancy_input = input$pregnancy,
  district_input = input$district,
  use_alt_names = TRUE
)

# Leading COD bump chart
vrd_plot <- config_bump_data(vrd_fltr, colors = clrs)

cap <- cod_bump_caption(
  names = filternames,
  inputs = input,
  filters = datafilters,
  max_age = maxage,
  ages = age
)

cod_bump_chart(
  vrd_plot,
  xvals = input$years,
  nranks = 10,
  caption = cap
)

# COD breakdown table
vrd_tbl <- vrd_fltr |>
  rankable_cod_summary(
    year = input$years[2],
    rcod_name = rcod$mod_alt$name2[45],
    cod_list = rcod$mod
  )

vrd_tbl |>
  cod_table(
    year = input$years[2],
    rcod_name = rcod$mod_alt$name2[45]
  )

# Annual deaths plots
plot_ann_deaths(
  annual_deaths |>
    select(year, "Deaths" = n, "Rankable deaths" = n_rankable),
  type = "n"
)

plot_ann_deaths(
  annual_deaths |>
    select(year, "Deaths" = pct, "Rankable deaths" = pct_rankable),
  type = "pct"
)


