pacman::p_load(
  httr,
  jsonlite,
  dplyr
)



# Native API --------------------------------------------------------------


# Dataverse API endpoint for datasets
base_url <- "http://dataverse.asu.edu/api/datasets/:persistentId/"
doi <- "doi:10.48349/ASU/WW9UMD"

# Construct API URL
api_url <- paste0(base_url, "?persistentId=", doi)

# Make request
dat <- GET(api_url) %>%
  content(as = 'text') %>%
  fromJSON()

# Explore
get_str(dat)
get_str(dat, 5)
get_str(dat$data$latestVersion$metadataBlocks, 5)

# within citation, not obviously named, but named in type fields
dat$data$latestVersion$metadataBlocks$citation$fields$typeName
get_str(dat$data$latestVersion$metadataBlocks$citation$fields, 5)
get_str(dat$data$latestVersion$metadataBlocks$citation$fields$value, 5)
# This is more comprehensive than the OAI metadata.

# No download info though?


# Another Native API ------------------------------------------------------


base_url <- "http://dataverse.asu.edu/api/datasets/:persistentId/"
doi <- 'doi:10.48349/ASU/F8EQ0Y'

# Construct API URL
api_url <- paste0(base_url, "?persistentId=", doi)

# Make request
dat <- GET(api_url) %>%
  content(as = 'text') %>%
  fromJSON()

# Explore
get_str(dat)
get_str(dat, 5)
get_str(dat$data$latestVersion$metadataBlocks, 5)

# within citation, not obviously named, but named in type fields
dat$data$latestVersion$metadataBlocks$citation$fields$typeName
get_str(dat$data$latestVersion$metadataBlocks$citation$fields, 5)
get_str(dat$data$latestVersion$metadataBlocks$citation$fields$value, 5)



# Search by Files ---------------------------------------------------------


# Get 1000 (limit) files at ASU
# url <- 'https://dataverse.asu.edu/api/search?q=*&type=file&per_page=1000'
#
# dat <- GET(url) %>%
#   content(as = 'text') %>%
#   fromJSON()
#
# get_str(dat)
# get_str(dat$data, 3)
# get_str(dat$data$items, 3)


# Play around with only 10 files, add metadata blocks?
url <- 'https://dataverse.asu.edu/api/search?q=*&type=file&per_page=10'
dat <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(dat)
get_str(dat$data, 3)

# First 1000 are here
get_str(dat$data$items, 3)



# Search by Dataset -------------------------------------------------------


# Search by datasets instead of files
url <- 'https://dataverse.asu.edu/api/search?q=*&type=dataset&per_page=10'
dat <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(dat)
get_str(dat$data, 3)

get_str(dat$data$items$publications, 3)
get_str(dat$data$items$authors, 3)
get_str(dat$data$items$contacts, 3)
dat$data$items$publisher
dat$data$items$name_of_dataverse

# What are contacts
get_str(dat$data$items$contacts, 4)



# Search by Dataverse -----------------------------------------------------


# Search by datasets instead of files
url <- 'https://dataverse.asu.edu/api/search?q=*&type=dataverse&per_page=10'
dat <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(dat)
get_str(dat$data, 3)
dat$data$items
# Checks out, but not much here



# Dataset Metrics ---------------------------------------------------------


# Using Native API, but instructions for curl with shell, trying in R though
base_url <- "http://dataverse.asu.edu/api/datasets/:persistentId/makeDataCount/downloadsTotal"
doi <- "doi:10.48349/ASU/WW9UMD"
api_url <- paste0(base_url, "?persistentId=", doi)

# Make request
dat <- GET(api_url) %>%
  content(as = 'text') %>%
  fromJSON()

dat
# Not working. Try again with a Harvard dataset?

base_url <- "http://dataverse.harvard.edu/api/datasets/:persistentId/makeDataCount/downloadsTotal"
doi <- 'doi:10.7910/DVN/ZT3KDM'
api_url <- paste0(base_url, "?persistentId=", doi)

# Make request
dat <- GET(api_url) %>%
  content(as = 'text') %>%
  fromJSON()

dat
# Not working
# Could they not have make data count turned on?


# Metrics API -------------------------------------------------------------


base_url <- "http://dataverse.asu.edu/api/info/metrics/filedownloads"

dat <- GET(base_url) %>%
  content(as = 'text') %>%
  fromJSON()

dat
get_str(dat)
head(dat$data, 50)
# WAIT THIS IS IT - we just have to link it back using DOI
# the first series of letters is the dataset, second series is the file


