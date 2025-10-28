# Get deaths data from Birth and Death (\\hd3)

library(tidyverse)

yrs <- 2014:2024

# Find vital stats directories for the years of interest
p <- paste0("(", paste(yrs, collapse = "|"), ") Vital Stats Files")
dirs <- dir("W:\\", pattern = p, full.names = TRUE)

# Get file names containing "death" but not "birth" or "fetal" for each year
files <- lapply(yrs, \(x) {
  dir <- dirs[grepl(x, dirs)]
  file <- list.files(dir, pattern = "(?i)death", full.names = TRUE)
  file <- file[!grepl("(?i)birth|fetal", file)]

  if (any(grepl("sav$", file)) && !grepl("2019", x)) {
    file[grepl("sav$", file)][1]
  } else if (any(grepl("sas7bdat$", file)) && !grepl("2019", x)) {
    file[grepl("sas7bdat$", file)][1]
  } else if (any(grepl("csv$", file))) {
    file[grepl("csv$", file)][1]
  } else if (any(grepl("xlsx$", file))) {
    file[grepl("xlsx$", file)][1]
  } else {
    NULL
  }
})

print(files)

# Import files
vrd <- lapply(files, \(x) {
  if (grepl("sav$", x)) {
    haven::read_sav(x[grepl("sav$", x)]) |>
      mutate(across(everything(), as.character))
  } else if (grepl("sas7bdat$", x)) {
    haven::read_sas(x[grepl("sas7bdat$", x)])
  } else if (grepl("csv$", x)) {
    read_csv(x[grepl("csv$", x)], col_types = cols(.default = col_character()))
  } else if (grepl("xlsx$", x)) {
    readxl::read_excel(x[grepl("xlsx$", x)], col_types = "text")
  } else {
    NULL
  }
})

names(vrd) <- paste0("deaths_", yrs)

# Column names to lower case
vrd <- lapply(vrd, \(x) {
  colnames(x) <- tolower(colnames(x))
  x
})

# Check variable names
df <- setmeup::batch_compare(lapply(vrd, colnames))

# Find the variable names all files have in common
find_common_colnames <- function(ls) {
  z <- lapply(ls[[1]], \(x) {
    all(unlist(lapply(ls[2:length(ls)], \(y) all(x %in% y))))
  })
  ls[[1]][unlist(z)]
}

cnms <- find_common_colnames(lapply(vrd, \(x) colnames(x)))

print(cnms)

# Find the variable names not shared by all files
cols <- lapply(vrd, colnames)
names(cols) <- names(vrd)

nm_missing <- lapply(cols, \(x) sort(x[!x %in% cnms]))
names(nm_missing) <- names(vrd)

print(nm_missing)

# In the 2023-2024 files, `bridgrace` was renamed `multirace`
vrd <- lapply(vrd, \(x) {
  rename_vars <- c("multirace" = "bridgrace")
  rename(x, any_of(rename_vars))
})

# Check variable names
df <- setmeup::batch_compare(lapply(vrd, colnames))

# Combine into a single dataframe
vrd <- list_rbind(vrd)

vrd[vrd == ""] <- NA

# Save to `data/1-source/`
nm <- paste0("vr_deaths_", min(yrs), "_", max(yrs), ".rds")

saveRDS(vrd, paste0("data/", nm))

