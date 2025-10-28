
source("scripts/setup.R")

yrs <- c(2014, 2024)

# Configure `vrd` based on Shiny inputs
df <- filter_vrd(
  vrd,
  cod_set = catlist[[2]],
  years_input = yrs,
  age_input = agelist[[1]],
  sex_input = sexlist[[1]],
  race_input = racelist[[1]],
  hispanic_input = hispaniclist[[1]],
  education_input = edlist[[1]],
  pregnancy_input = preglist[[1]],
  district_input = distlist[[1]]
)

# Leading COD bump chart
df |>
  config_bump_data(colors = cod_colors) |>
  cod_bump_chart(xvals = yrs, nranks = 10)

# COD breakdown table
dftbl <- df |>
  rankable_cod_summary(
    year = yrs[2],
    cod_name = names(cod_mod)[52],
    cod_list = cod_mod
  )

dftbl |>
  cod_table(
    year = yrs[2],
    cod_name = names(cod_mod)[52]
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




# How much does each COD contribute to the total deaths per year as a percentage?

