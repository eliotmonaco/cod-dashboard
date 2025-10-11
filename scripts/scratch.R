
library(tidyverse)

source("scripts/fn.R")

vrd <- readRDS("data/vr_deaths_2014_2023.rds")
cod_colors <- readRDS("data/cod_colors.rds")

df <- config_vrd(
  vrd,
  nranks = 10,
  years = c(2014, 2023),
  ages = 1:115,
  sex = c("F", "M", "U"),
  race = "racechin",
  palette = cod_colors
)

df |>
  cod_bump_chart()


