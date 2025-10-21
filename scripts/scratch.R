
source("scripts/setup.R")

yrs <- c(2014, 2023)

df <- filter_vrd(
  vrd,
  years_input = yrs,
  age_input = agelist[[1]],
  sex_input = sexlist[[1]],
  race_input = racelist[[1]],
  hispanic_input = hispaniclist[[1]],
  education_input = edlist[[1]],
  pregnancy_input = preglist[[1]],
  district_input = distlist[[1]]
)

# df |>
#   config_bump_data(colors = cod_colors) |>
#   cod_bump_chart(xvals = yrs, nranks = 10)

df <- df |>
  rankable_cod_summary(
    cod_name = names(cod)[47],
    cod_list = cod
  )

