#' Explore APIs
#' 2024-09-10
#'
#' Testing out Dataverse APIs for pulling metadata. User guide at:
#' https://guides.dataverse.org/en/latest/api/index.html
#'
#' Few APIs here:
#'  1. Native API - This does all the admin things, creating and editing, just
#'    about everything you can do as as admin on website. API key required for
#'    consequential actions. Tried this below but it is not of much use to us.
#'  2. Search API - This pulls almost all of the metadata we need. Can search
#'    at dataverse, dataset, file level.
#'  3. Metrics API - This is where we can download counts by file. It also has
#'    endpoints for tons of other useful things like time series of downloads,
#'    downloads up to a certain date, tree structure of dataverse collections
#'
#' Actual script for pulling our data is in pull_john_hopkins.R



# Housekeeping ------------------------------------------------------------

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

# Test flattening
test <- flatten(dat)
test <- prettify(dat)
dat <- GET(api_url) %>%
  content(as = 'text') %>%
  fromJSON(flatten = TRUE)
get_str(dat, 5)
# Not doing much different



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


# Now with Johns Hopkins
url <- 'https://archive.data.jhu.edu/api/search?q=*&type=file&per_page=10'
dat <- get(url)
get_str(dat)



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




# Metrics API Testing ------------------------------------------------------


## Total files
get("http://dataverse.asu.edu/api/info/metrics/files")

## Total datasets
get("http://dataverse.asu.edu/api/info/metrics/datasets")

## Total files in last 100 days
get("http://dataverse.asu.edu/api/info/metrics/files/pastDays/100")

## Try it with Johns Hopkins too - make sure their metrics are working
get("https://archive.data.jhu.edu/api/info/metrics/files/pastDays/100")

## By Subject
get("https://archive.data.jhu.edu/api/info/metrics/datasets/bySubject")

## Combine queries
get("https://archive.data.jhu.edu/api/info/metrics/datasets/bySubject/pastDays/100")
# Nope, this doesn't work

## Try downloads
get("https://archive.data.jhu.edu/api/info/metrics/downloads")
# This is total downloads, presumably every file?
# Matches total shown at to of webpage in widget

## Downloads in past days
get("https://archive.data.jhu.edu/api/info/metrics/downloads")

## Downloads on monthly basis - time series
get("https://archive.data.jhu.edu/api/info/metrics/downloads/monthly")
# That's pretty neat!

## Monthly downloads by file ID
get("https://archive.data.jhu.edu/api/info/metrics/filedownloads/monthly")

## Unique downloads
get("https://archive.data.jhu.edu/api/info/metrics/uniquedownloads")
# Not a lot of unique people downloading things!
# Can also do this monthly or up to a certain date
# This is less biased (otherwise, large datasets get inflated download stats)

## Check out the tree
dat <- get("https://archive.data.jhu.edu/api/info/metrics/tree")
get_str(dat$data, 3)
# This is structure of dataverses, might come in handy too

## Accounts
get("https://archive.data.jhu.edu/api/info/metrics/accounts")
# This one is not working.



# Metrics API Downloads ---------------------------------------------------


# This is the one we care about to get download counts by file.
dat <- get("https://archive.data.jhu.edu/api/info/metrics/filedownloads")

dat
get_str(dat)
head(dat$data, 50)
# the first series of letters is the dataset, second series is the file
# Doesn't quite match files listed on website - I don't think it is counting
# files with 0 downloads.

get_table(dat$data$count)
# No files have 0 downloads


