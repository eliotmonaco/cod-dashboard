library(tidyverse)
library(ggtext)

source("scripts/fn.R")

vrd <- readRDS("data/vr_deaths_2014_2023.rds")
cod <- readRDS("data/cod_rankable.rds")
cod_colors <- readRDS("data/cod_colors.rds")

# Selector lists for Shiny inputs

## Years

yrsrng <- c(
  min(vrd$yod, na.rm = TRUE),
  max(vrd$yod, na.rm = TRUE)
)

## Age

maxage <- max(vrd$age, na.rm = TRUE)

ngroups <- ceiling(maxage / 5)

agegroups <- list()

age <- 0

for (i in 1:ngroups) {
  agegroups[[i]] <- paste(age:(age + 4), collapse = ";")

  names(agegroups)[i] <- paste(age, "to", age + 4)

  age <- age + 5
}

agelist <- c(
  list("All" = paste(0:maxage, collapse = ";")),
  agegroups
)

## Sex

sexlist <- list(
  "All" = "F;M;U",
  "Female" = "F",
  "Male" = "M"
)

## Race

racelist <- list(
  "all",
  "racewht",
  "raceblk",
  "raceind",
  "raceasind",
  "racechin",
  "raceflip",
  "racejap",
  "racekor",
  "raceviet",
  "raceaoth",
  "racehaw",
  "raceguam",
  "racesamoa",
  "raceioth",
  "raceoth"
)

names(racelist) <- c(
  "All",
  "White",
  "Black",
  "American Indian or Alaska Native",
  "Asian Indian",
  "Chinese",
  "Filipino",
  "Japanese",
  "Korean",
  "Vietnamese",
  "Other Asian",
  "Native Hawaiian",
  "Guamanian or Chamorro",
  "Samoan",
  "Other Pacific Islander",
  "Other"
)

# Hispanic origin

hispaniclist <- list(
  "all",
  "mexican",
  "puer",
  "cuban",
  "hother"
)

names(hispaniclist) <- c(
  "All",
  "Mexican",
  "Puerto Rican",
  "Cuban",
  "Other"
)

# Education level

edlist <- as.list(as.character(1:9))

names(edlist) <- c(
  "8th Grade or Less",
  "9th through 12th",
  "High School Grade or GED",
  "Some College, No Degree",
  "Associate Degree",
  "Bachelors Degree",
  "Masters Degree",
  "Doctorate Degree or Professional Degree",
  "Unknown"
)

edlist <- c(
  list("All" = "all"),
  edlist
)

# Pregnancy status

preglist <- as.list(as.character(c(1:4, 7:9)))

names(preglist) <- c(
  "Not pregnant within past year",
  "Pregnant at the time of death",
  "Not pregnant, but pregnant within 42 days of death",
  "Not pregnant, but pregnant 43 days to 1 year before death",
  "Unknown if prengnant within last year",
  "Not applicable",
  "Not on certificate"
)

preglist <- c(
  list("All" = "all"),
  preglist
)

