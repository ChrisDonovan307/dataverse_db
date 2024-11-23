#' Explore clean tables
#' 2024-09-18

#' Poking around with list of cleabn tables


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

source('r/3_functions/get_str.R')
source('r/4_scripts/housekeeping.R')

# Load all datasets
dat <- readRDS('r/2_clean/jhu_dfs.Rds')



# Explore -----------------------------------------------------------------


sort(names(dat))
get_str(dat)


# Do user and reg user join properly
test <- inner_join(dat$user, dat$registered_user)


# Check pubdate for file and dataset
get_str(dat$dataset)
get_str(dat$file)
get_str(dat$affiliation)

# Check first dataset
(id <- dat$dataset$ds_id[1])

# Date of dataset
dat$dataset %>%
  filter(ds_id == id) %>%
  pull(pub_date)

# Get associated files
dat$file %>%
  filter(ds_id == id) %>%
  pull(published_at)

