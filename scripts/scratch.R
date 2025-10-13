
library(tidyverse)

source("scripts/setup.R")

yrs <- c(2014, 2023)

df <- config_vrd(
  vrd,
  years_input = yrs,
  age_input = unlist(strsplit(agelist[[1]], ";")),
  sex_input = unlist(strsplit(sexlist[[1]], ";")),
  # sex_input = "F",
  race_input = racelist[[1]],
  hispanic_input = hispaniclist[[1]],
  education_input = edlist[[1]],
  pregnancy_input = preglist[[3]],
  palette = cod_colors
)

df |>
  cod_bump_chart(xvals = yrs, nranks = 10)

