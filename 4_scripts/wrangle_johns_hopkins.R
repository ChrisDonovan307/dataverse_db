#' Wrangle Johns Hopkins
#' 2024-09-15
#'
#' Wrangling, cleaning, exploring Johns Hopkins data. Making csv datasets



# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  purrr
)

dat <- readRDS('1_raw/jhu_metadata.RDS')
results <- list()



# Files -------------------------------------------------------------------


get_str(dat)

# See if the file id can connect files from search API to files in downloads
intersect(dat$jhu_files$file_id, dat$jhu_downloads$id) %>%
  length()
# Yes, this works

# Are any IDs from downloads NOT included in files?
any(! dat$jhu_downloads$id %in% dat$jhu_files$file_id)
# Yes, there are

# How many
length(which(! dat$jhu_downloads$id %in% dat$jhu_files$file_id))
# 57. Eh. Let's just lose em for now.

# Use left join to keep all files from the files API search, add downloads
# But first make file_id numeric to match
dat$jhu_files$file_id <- as.numeric(dat$jhu_files$file_id)
files_df <- left_join(
  dat$jhu_files,
  dat$jhu_downloads,
  by = join_by(file_id == id)
) %>%
  rename(downloads = count)

get_str(files_df)

# Files are ready to go now
results$files <- files_df



# Datasets ----------------------------------------------------------------


get_str(dat$jhu_datasets, 1)
get_str(dat$jhu_datasets_native[[1]])
# url links to persistentUrl

datasets_df <- dat$jhu_datasets %>%
  select(name:storageIdentifier)
get_str(datasets_df)

results$datasets <- datasets_df



# Authors -----------------------------------------------------------------


get_str(dat$jhu_datasets, 1)
get_str(dat$jhu_datasets$authors)

# Pull out authors from lists, keep unique authors only
authors <- dat$jhu_datasets$authors %>%
  unlist() %>%
  unique() %>%
  sort()
# Need some cleaning with string similarity, but looks okay otherwise

# Add author ID
authors_df <- data.frame(
  author = authors,
  author_id = 1:length(authors)
)
head(authors_df)

# Save this to results list
results$authors <- authors_df



# Publications ------------------------------------------------------------


get_str(dat$jhu_datasets)
get_str(dat$jhu_datasets$publications)

# Get the global id from dataset into it
pubs <- dat$jhu_datasets %>%
  select(global_id, publications)
get_str(pubs)

pubs_df <- map(1:nrow(pubs), \(row) {
  df <- data.frame(
    citation = ifelse(
      is.null(pubs$publications[[row]]$citation),
      NA_character_,
      pubs$publications[[row]]$citation
    ),
    url = ifelse(
      is.null(pubs$publications[[row]]$url),
      NA_character_,
      pubs$publications[[row]]$url
    ),
    dataset_doi = pubs$global_id[[row]]
  )
}) %>%
  list_rbind() %>%
  mutate(pub_id = as.numeric(factor(citation)))

get_str(pubs_df)
# looks good

# Save it to results list
results$publications <- pubs_df



# Subjects ----------------------------------------------------------------


get_str(dat$jhu_datasets)
get_str(dat$jhu_datasets$subjects)

subjects <- dat$jhu_datasets$subjects %>%
  unlist() %>%
  unique() %>%
  sort()

subjects_df <- data.frame(
  subject = subjects,
  subject_id = 1:length(subjects)
)
subjects_df
# Looks good

results$subjects <- subjects_df



# Keywords ----------------------------------------------------------------


get_str(dat$jhu_datasets)
get_str(dat$jhu_datasets$keywords)

keywords <- dat$jhu_datasets$keywords %>%
  unlist() %>%
  tolower() %>%
  unique() %>%
  sort()
keywords

keywords_df <- data.frame(
  keyword = keywords,
  keyword_id = 1:length(keywords)
)
keywords_df
# Might need some string similarity cleaning here, but whatever

results$keywords <- keywords_df



# License -----------------------------------------------------------------


get_str(dat$jhu_datasets_native)
get_str(dat$jhu_datasets_native[[1]]$latestVersion)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$license)

licenses <- map(dat$jhu_datasets_native, ~ .x$latestVersion$license)
get_str(licenses)
licenses <- map(licenses, as.data.frame)
get_str(licenses)

# Function to check if a data frame is empty or NULL
is_valid_df <- function(df) {
  !is.null(df) && nrow(df) > 0
}

# Filter the list, keeping only valid (non-empty, non-NULL) data frames
licenses_filter <- Filter(is_valid_df, licenses)

licenses_df <- list_rbind(licenses_filter) %>%
  unique() %>%
  # arrange(name) %>%
  mutate(license_id = 1:nrow(.))
get_str(licenses_df)

results$licenses <- licenses_df



# Grant -------------------------------------------------------------------


# Check grant numbers
# get_str(dat$jhu_datasets_native)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value)

# Pull just grant number and grant agency for all datasets
out <- imap(dat$jhu_datasets_native, \(dataset, index) {

  # Names are inconsistent. Figure out which index in fields$value is grants
  grant_index <- which(dataset$latestVersion$metadataBlocks$citation$fields$typeName == 'grantNumber')

  # If there is no grant info, just return NA for that dataset
  if(length(grant_index) == 0) {
    return(NA)
  } else {

    # Assign object based on grant metadata for that dataset
    grant_metadata <- dataset$latestVersion$metadataBlocks$citation$fields$value[[grant_index]]

    # Pull grant number and agency
    grant_numbers <- grant_metadata$grantNumberValue$value
    grant_agency <- grant_metadata$grantNumberAgency$value

    # Make DF with number, agencym and dataset url.
    # If no number or agency, just give NA
    grant_df <- data.frame(
      grant_number = ifelse(is.null(grant_numbers), NA, grant_numbers),
      grant_agency = ifelse(is.null(grant_agency), NA, grant_agency),
      dataset_url = dataset$persistentUrl
    )

    return(grant_df)
  }
})

grants_df <- Filter(Negate(is.logical), out) %>%
  list_rbind()
get_str(grants_df)
# This is messy but it's okay - that's what it is.

results$grants <- grants_df



# Software ----------------------------------------------------------------


# get_str(dat$jhu_datasets_native)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields, 3)
#
# # Names here
# (sw_fields <- dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$typeName)
#
# # All values
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$value)
#
# # SW Title and SW License
# dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$value[[1]]
# dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$value[[2]]
#
# # SW dependencies
# get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$value[11], 3)
#
# # Let's just take top level SW data for now.
# # that is ...$value, columns 1:9
# test <- imap(dat$jhu_datasets_native, \(x, i) {
#   x$latestVersion$metadataBlocks$software$fields$value[1:8]
# })
# get_str(test)
# results$datasets$url
#
# test2 <- imap(test, \(x, i) {
#   if (is.list(x)) {
#     x %>%
#       c(results$datasets$url[[i]])
#   }
# })
#
# get_str(test2)
# test3 <- Filter(Negate(is.null), test2)
# as.data.frame(test3[[1]])
# as.data.frame(test3[[14]])
# test3[[14]] %>% get_str
#
# test4 <- map(test3, ~ as.data.frame(.x))
#   map(~ as.data.frame(.x))
#
# get_str(test3)
#
#
# test3[[1]]
#
# process_vector <- function(vec) {
#   if (is.null(vec)) {
#     return(NA_character_)
#   } else if (is.character(vec)) {
#     return(paste(vec, collapse = ", "))
#   } else {
#     return(vec)
#   }
# }
#
# # Apply the function to each element in the list
# processed_list <- map(test3, ~ {
#   .x <- map(.x, process_vector)
# })
# get_str(processed_list)
#
# df <- map(processed_list, ~ {
#   as.data.frame(.x) %>%
#     setNames(c(sw_fields[1:8], 'dataset_url')) %>%
#     unnest()
# })
# get_str(df)
#
# dfs <- rbind(df)
# get_str(dfs)



# Software Round 2 --------------------------------------------------------


# Names
dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$typeName
# We want swTitle, swLicense, consider swDependency

# This is the worst code. Holy moly. But it works for now.
out <- imap(dat$jhu_datasets_native, \(dataset, index) {

  # Names are inconsistent. Figure out which index in fields$value is grants
  metadataBlocks_names <- dataset$latestVersion$metadataBlocks %>%
    names()
  software_index <- which(metadataBlocks_names == 'software')

  # If there is no grant info, just return NA for that dataset
  if(length(software_index) == 0) {
    return(NA)
  } else {

    # Assign object based on grant metadata for that dataset
    software_metadata <- dataset$latestVersion$metadataBlocks[[software_index]]

    # Exit early if there is no software info
    if (length(software_metadata$fields) == 0) {

      return(NA)

    } else {

      # Pull swTitle and License indices
      license_index <- which(software_metadata$fields$typeName == 'swLicense')
      title_index <- which(software_metadata$fields$typeName == 'swTitle')

      # Pull the actual licenses and titles base don the indices
      if (length(license_index) > 0) {
        sw_licenses <- software_metadata$fields$value[[license_index]]
      } else {
        sw_licenses <- NA
      }

      if (length(title_index) > 0) {
        sw_titles <- software_metadata$fields$value[[title_index]]
      } else {
        sw_titles <- NA
      }

      # Make DF with license, title, and dataset url.
      # If no number or agency, just give NA
      grant_df <- data.frame(
        software_title = ifelse(is.na(sw_titles), NA, sw_titles),
        software_license = ifelse(is.na(sw_licenses), NA, sw_licenses),
        dataset_url = dataset$persistentUrl
      )

      return(grant_df)

    }
  }
})

get_str(out)
sw_licenses_df <- Filter(Negate(is.logical), out) %>%
  list_rbind()
get_str(sw_licenses_df)
# This is messy but it's okay - that's what it is.

results$software_licenses <- sw_licenses_df



# Save and Clear ----------------------------------------------------------


# Save as list of DFs
saveRDS(results, '2_clean/jhu_dfs.Rds')

# Also save as separate CSVs
iwalk(results, \(df, name) {
  write.csv(df, paste0('6_outputs/jhu_', name, '.csv'))
})

# Clear
clear_data()
