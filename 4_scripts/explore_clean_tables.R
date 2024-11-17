#' Explore clean tables
#' 2024-09-18

#' Poking around with list of cleabn tables


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

source('3_functions/get_str.R')

# Load clean tables
names <- list.files('6_outputs/') %>%
  str_split_i('\\.', 1) %>%
  str_split_i('_', 2)
paths <- list.files('6_outputs/', full.names = TRUE)
# dat <- map(paths, read.csv) %>%
  # setNames(c(names))

walk2(paths, names, \(path, name) {
  assign(name, read.csv(path), envir = .GlobalEnv)
})


# Explore -----------------------------------------------------------------


names(dat)

str(dat$admins)
map(dat, str)

# Start with datasets
get_str(datasets)
get_str(collections)
get_str(licenses)
get_str(software)
get_str(produce)
get_str(publications)
