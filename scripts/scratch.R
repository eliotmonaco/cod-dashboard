
library(tidyverse)

source("scripts/setup.R")

df <- config_vrd(
  vrd,
  nranks_input = 10,
  years_input = c(2014, 2023),
  age_input = 1:120,
  # sex_input = c("F", "M", "U"),
  sex_input = "F",
  race_input = racelist[[1]],
  hispanic_input = hispaniclist[[1]],
  palette = cod_colors
)

df |>
  cod_bump_chart(xvals = c(2014, 2023))


