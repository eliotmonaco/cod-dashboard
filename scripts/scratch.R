
library(tidyverse)
library(ggbump)

source("scripts/fn.R")

vrd <- readRDS("data/vr_deaths_2014_2023.rds")

nranks <- 10

df <- vrd |>
  # filter(age %in% 0:4) |>
  group_by(yod, cod_rankable) |>
  summarize(n = n()) |>
  ungroup() |>
  drop_na(cod_rankable)

ls <- lapply(unique(df$yod), \(x) {
  df |>
    filter(yod == x) |>
    mutate(rank = rank_cod(-n),) |>
    mutate(yrank = nranks - rank) |>
    filter(rank %in% 1:nranks) |>
    arrange(rank)
})

df <- list_rbind(ls)

cod_bump_chart(df)













