
library(tidyverse)

source("scripts/setup.R")

yrs <- c(2014, 2023)

df <- config_vrd(
  vrd,
  # nranks_input = 10,
  years_input = yrs,
  age_input = 1:120,
  sex_input = c("F", "M", "U"),
  # sex_input = "F",
  race_input = racelist[[1]],
  hispanic_input = hispaniclist[[1]],
  education_input = edlist[[1]],
  pregnancy_input = preglist[[1]],
  palette = cod_colors
)

df |>
  cod_bump_chart(xvals = yrs, nranks = 10)


