#' Pull Johns Hopkins
#' 2024-09-15
#'
#' Pulling metadata from Johns Hopkins Dataverse Installation
#' https://archive.data.jhu.edu/dataverse/root
#'
#' Steps
#'  1. Search API to get most metadata
#'  2. Metrics API to get download counts
#'  3. Join the two together
#'  4. Get clean-ish csvs
#'  5. ??
#'  6. Profit



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  stringr
)

base_url <- 'https://archive.data.jhu.edu/'
results <- list()

# Load helper functions
paths <- list.files('3_functions/', full.names = TRUE)
lapply(paths, source)
rm(paths)


# Search API --------------------------------------------------------------
## Test with ASU -----------------------------------------------------------


# Using function for files at ASU
test <- pull_metadata(
  base_url = 'https://dataverse.asu.edu',
  type = 'file',
  per_page = 10,
  page_limit = 3
)
get_str(test)



## Files -------------------------------------------------------------------


# Now do it for real with JHU.
# It has 8666 files, so we are setting backup stop at 10 pages
results$jhu_files <- pull_metadata(
  base_url = 'archive.data.jhu.edu',
  type = 'file',
  per_page = 1000,
  page_limit = 10
)
get_str(results$jhu_files)



## Datasets ----------------------------------------------------------------


# Only 251 datasets at JHU
results$jhu_datasets <- pull_metadata(
  base_url = 'archive.data.jhu.edu',
  type = 'dataset',
  per_page = 1000,
  page_limit = 1
)
get_str(results$jhu_datasets)



## Dataverses --------------------------------------------------------------


# Only 62 here
results$jhu_dataverses <- pull_metadata(
  base_url = 'archive.data.jhu.edu',
  type = 'dataverse',
  per_page = 1000,
  page_limit = 1
)
get_str(results$jhu_dataverses)



# Metrics API -------------------------------------------------------------


# Now use metrics API to get downloads by file
results$jhu_downloads <- get(
  "https://archive.data.jhu.edu/api/info/metrics/filedownloads"
) %>%
  .$data

get_str(results$jhu_downloads)
head(results$jhu_downloads)
# These are download counts by file DOI. Use this to link back to datasets



# Native API --------------------------------------------------------------


# Use this to get software. Need to map over list of dataset DOIs
dataset_ids <- results$jhu_datasets$global_id
base_url <- "http://archive.data.jhu.edu"

# Dataverse API endpoint for datasets
ds_dat <- map(dataset_ids,\(id) {
  url <- paste0(
    base_url,
    "/api/datasets/:persistentId/?persistentId=",
    id
  )
  print(url)
  out <- GET(url) %>%
    content(as = 'text') %>%
    fromJSON()
})

ds_dat <- map(ds_dat, ~ .x$data)
get_str(ds_dat)
get_str(ds_dat, 3)

results$jhu_datasets_native <- ds_dat



# Save and Clear ----------------------------------------------------------


saveRDS(results, '1_raw/jhu_metadata.RDS')
clear_data()
