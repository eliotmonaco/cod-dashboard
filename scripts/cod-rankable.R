# Create a list of rankable causes of death (CODs). Source data is Table B from
# the NVSS Instruction Manual, Part 9, ICD-10 Cause-of-Death Lists for
# Tabulating Mortality Statistics. This table contains the rankable CODs used to
# determine leading CODs which are indicated by a "#" before the name. Lines
# from the original PDF were copied into an Excel file, which is imported below.
# https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf

library(tidyverse)

source("scripts/fn.R")

icd <- readxl::read_excel(
  "data/icd-rankable-cod-raw.xlsx",
  sheet = 2,
  col_names = FALSE
)

colnames(icd) <- "name"

# Find the rows that belong together and paste
p <- "^\\.{2}|^[[:lower:]]|^[[:upper:]]\\d{2}"

icd$first <- !grepl(p, icd[[1]])

for (i in rev(1:nrow(icd))) {
  if (!icd[i, "first"]) {
    icd[i - 1, 1] <- paste(icd[i - 1, 1], icd[i, 1])
  }
}

icd <- icd[icd$first, "name"]

codes <- strsplit(icd$name, "\\.{2,}")

icd$name <- unlist(lapply(codes, `[[`, 1))
icd$code <- unlist(lapply(codes, `[[`, 2))

# # Test `get_icd_seq()`
# get_icd_seq("A01, A02")
#
# get_icd_seq("A07-A09")
#
# get_icd_seq("A04, A09-A21")
#
# get_icd_seq("A54.2")
#
# get_icd_seq("A54.2-A63.3")
#
# get_icd_seq("A89-B11")
#
# get_icd_seq("A89-C11")
#
# x <- get_icd_seq("A99.9-C01.0")
# head(x)
# tail(x, n = 12)
#
# get_icd_seq("*U01-*U02,X85-Y09,Y87.1")

# Correct rows with codes starting with "I"
p <- "(I\\d{2}-)1(\\d{2})"

for (i in 1:nrow(icd)) {
  s <- icd[i, "code", drop = TRUE]

  if (grepl(p, s)) {
    print(s)

    s <- gsub(p, "\\1I\\2", s)

    icd[i, "code"] <- s

    print(s)
  }
}

# Correct rows with codes containing "l"
p <- "l"

for (i in 1:nrow(icd)) {
  s <- icd[i, "code", drop = TRUE]

  if (grepl(p, s)) {
    print(s)

    s <- gsub(p, "1", s)

    icd[i, "code"] <- s

    print(s)
  }
}

# Get all sequences from `icd`
codes <- lapply(icd$code, get_icd_seq)

# Create list of rankable CODs
r <- which(grepl("#", icd$name))

cod_rankable <- codes[r]

names(cod_rankable) <- str_squish(sub("^(\\d{1,3})?#\\s", "", icd$name[r]))

saveRDS(cod_rankable, "data/cod_rankable.rds")

