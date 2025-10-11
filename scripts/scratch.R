
library(tidyverse)

source("scripts/setup.R")

df <- config_vrd(
  vrd,
  nranks = 10,
  years = c(2014, 2023),
  ages = 1:120,
  sex = c("F", "M", "U"),
  race = racelist[[1]],
  palette = cod_colors
)

df |>
  cod_bump_chart(xvals = c(2014, 2023))


