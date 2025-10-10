# Process deaths dataset

library(tidyverse)

source("scripts/get-deaths.R")
source("scripts/fn.R")

cod <- readRDS("data/cod_rankable.rds")

fmt <- paste0("ID%0", nchar(nrow(vrd)), "d")

vrd <- vrd |>
  mutate(
    yod = as.numeric(yod),
    age = as.numeric(age),
    rowid = sprintf(fmt, row_number()), .before = 1
  ) |>
  filter(citylimit == "Y")

# Clean ICD codes
vrd <- fix_vital_icd(vrd, "rowid")

# Validate ICD codes
vrd |>
  select(cod) |>
  filter(!grepl("^[[:upper:]]\\d{2,3}$", cod))

# Create regex patterns
cod <- lapply(cod, \(x) {
  paste(paste0("^", sub("\\.", "", x)), collapse = "|")
})

# Get rankable COD categories
cod_ranked <- sapply(vrd$cod, match_icd, ls = cod)

# Check if any ICD code was matched to > 1 COD
any(sapply(cod_ranked, length) > 1)

vrd$cod_rankable <- cod_ranked

saveRDS(vrd, "data/vr_deaths_2014_2023.rds")

