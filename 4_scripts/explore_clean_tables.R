#' Explore clean tables
#' 2024-09-18

#' Poking around with list of cleabn tables


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr
)

# Load clean tables
dat <- readRDS('2_clean/jhu_dfs.rds')



# Explore -----------------------------------------------------------------


names(dat)
get_str(dat)
