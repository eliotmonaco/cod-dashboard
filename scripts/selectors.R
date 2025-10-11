# Selector lists for Shiny inputs


# Age

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
  list("All ages" = paste(0:maxage, collapse = ";")),
  agegroups
)


# Sex

sexlist <- list(
  "Both" = "F;M;U",
  "Female" = "F",
  "Male" = "M"
)


# Race

racelist <- list(
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

racelist <- c(
  list("All" = "all"),
  racelist[sort(names(racelist))]
)

