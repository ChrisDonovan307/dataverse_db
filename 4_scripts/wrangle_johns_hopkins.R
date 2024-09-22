#' Wrangle Johns Hopkins
#' 2024-09-15
#'
#' Wrangling, cleaning, exploring Johns Hopkins data. Making csv datasets



# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  purrr,
  stringr,
  tidyr,
  httr,
  jsonlite
)

dat <- readRDS('1_raw/jhu_metadata.RDS')
source('3_functions/wrangling_utilities.R')
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
  mutate(downloads = ifelse(is.na(count), 0, count),
         .keep = 'unused')
get_str(files_df)

# Clean up names to be consistent with other tables
# Get rid of native file_id and make the persistent file id the file_id
files_df <- files_df %>%
  select(-c(
    file_id,
    dataset_id,
    file_persistent_id,
    unf,
    dataset_name,
    dataset_citation
  )) %>%
  select(
    file_id = pid,
    dataset_id = dataset_persistent_id,
    everything()
  )
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

#' To get download counts for datasets, lets add up the file counts, because the
#' metrics API for directly getting download counts doesn't work.
get_str(files_df)
get_str(datasets_df)
# files_df.dataset_persistent_id == datasets_df.global_id

# Get download counts
counts <- files_df %>%
  group_by(dataset_id) %>%
  summarize(downloads = sum(downloads)) %>%
  arrange(desc(downloads))
counts
get_str(counts)
get_table(counts$downloads)

# Join this into the datasets
datasets_df <- left_join(datasets_df,
                         counts,
                         join_by(global_id == dataset_id))
get_str(datasets_df)
get_table(datasets_df$downloads)

# Harmonize names
datasets_df <- datasets_df %>%
  select(-storageIdentifier, -type, -name_of_dataverse) %>%
  select(
    dataset_id = global_id,
    collection_id = identifier_of_dataverse,
    name,
    url,
    description,
    citation_html = citationHtml,
    everything()
  )

# Save it
results$datasets <- datasets_df



# Collections -------------------------------------------------------------


get_str(dat$jhu_dataverses)
head(dat$jhu_dataverses)
# This is easy, just get rid of the type column and leave else as is
# also add the root dataverse
collections_df <- dat$jhu_dataverses %>%
  select(collection_id = identifier, everything(), -type) %>%
  mutate(root_id = 'jhu')
get_str(collections_df)

results$collections <- collections_df



# Contacts ----------------------------------------------------------------


get_str(dat$jhu_datasets, 3)
get_str(dat$jhu_datasets$contacts)

# Save dataset ids, will have to join them with contacts
global_ids <- dat$jhu_datasets$global_id

# Pull out just contacts
contacts <- dat$jhu_datasets$contacts

get_str(contacts)
contacts[[1]]
contacts[[2]]
# The second row in each dataframe seems to always be for access, and leads to
# Johns Hopkinds University Data Services. Probably just get rid of those.

# Add the global IDs here before we move on and muck things up
# Then combine into a single DF
contacts_df <- map2(contacts, global_ids, \(x, y) {
  x %>%
    mutate(global_id = y)
}) %>%
  bind_rows()
get_str(contacts_df)

# Get rid of anything that includes "access". we just want data contacts
# Also rename name to message
contacts_df <- contacts_df %>%
  filter(!str_detect(affiliation, 'Data Services'),
         str_length(affiliation) > 0,
         !str_detect(name, 'dataservices@jhu.edu|datamanagement@jhu.edu')) %>%
  rename(message = name)
get_str(contacts_df)

# Split out message into all the contact names
# First by contact. take last piece
# If there is an OR, split by OR
contacts_df$message %>% head(50)

# WE HAVE 241 GOING INTO THIS
# Split up message into chunks, pull out names and emails
# Also remove prefixes, dr and prof and such
contacts_df <- contacts_df %>%
  mutate(
    split = str_split_i(contacts_df$message, 'contact ', 2) %>%
      str_split(' or | or or ') %>%
      map(~ str_split(.x, ' via | v.ia | vis | at ') %>% extract_pairs)) %>%
  unnest(split) %>%
  mutate(email = str_split_i(email, ' ', 1) %>% str_remove('\\.$'),
         name = str_remove(name, 'Dr\\. |Prof\\. |Mr. |or '))

# Remove any rows where email or name are NA or empty string
contacts_df <- contacts_df %>%
  filter(!is.na(name), !is.na(email), length(email) > 0, length(name) > 0)

# now just clean it up so names match properly
# Saving this to a different object so I can come back here later
# because we will need to pull out these emails.
# for now, just keeping the things we only need for this relationship entity
contacts_table <- contacts_df %>%
  select(
    dataset_id = global_id,
    author_id = name
  )
get_str(contacts_table)

results$contacts <- contacts_table



# Explore -----------------------------------------------------------------


get_str(dat$jhu_datasets, 3)
get_str(dat$jhu_datasets_native)
get_str(dat$jhu_datasets_native[[1]])
get_str(dat$jhu_datasets_native[[1]]$latestVersion)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks)

### Explore citations
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields)

dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$typeName
# contact 3 "datasetContact
# author 2 "author"

# Explore contacts
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value[[3]])
dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value[[3]]



# Authors -----------------------------------------------------------------


get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value[[2]])
# Nice, this has name, affiliation, and an identifier, usually orcid ID

# First get global IDs
global_ids <- dat$jhu_datasets$global_id
authors_dfs <- imap(dat$jhu_datasets_native, \(author_chunk, index) {
  author_chunk <- author_chunk$latestVersion$metadataBlocks$citation$fields$value[[2]]
  if (is.data.frame(author_chunk)) {
    out <- author_chunk %>%
      unnest(cols = everything(), names_sep = '.') %>%
      mutate(global_id = global_ids[[index]])
  } else {
    out <- NA
  }
  return(out)
})
get_str(authors_dfs)

# Remove logical NAs and bind
big_author_df <- Filter(Negate(is.logical), authors_dfs) %>%
  bind_rows()
get_str(big_author_df)

# This is basically the author/dataset relationship entity
# we can pull that out, as well as a pure author dataset.
# and also use this to get institutions!

### First get pure author dataset
get_str(big_author_df)
just_authors <- big_author_df %>%
  select(
    name = authorName.value,
    # institution_id = authorAffiliation.value,
    author_id_type = authorIdentifierScheme.value,
    author_id = authorIdentifier.value
  ) %>%
  unique() %>%
  mutate(author_id = str_sub(author_id, start = -19)) %>%
  arrange(name)

# How many NAs
n_na <- sum(is.na(just_authors$author_id))

# Assign the ones missing an ID a 1:n_na id
just_authors$author_id[is.na(just_authors$author_id)] <- seq(1:n_na)
just_authors$author_id_type <- ifelse(is.na(just_authors$author_id_type),
                                      'dataverse_id',
                                      just_authors$author_id_type)
get_str(just_authors)

# Put their names in order instead of it being last, first
just_authors <- just_authors %>%
  mutate(
    split = str_split(name, ', '),
    name = map_chr(split, ~ paste(.x[2], .x[1])) %>% str_trim()
  ) %>%
  select(-split) %>%
  arrange(name)
get_str(just_authors)

# Bind to emails from contacts (maybe not worth doing this)
# test <- contacts_df %>%
#   select(name, email) %>%
#   unique() %>%
#   right_join(just_authors, by = 'name') %>%
#   unique()
# get_str(test)

results$authors <- just_authors



# Institutions ------------------------------------------------------------


get_str(big_author_df)
# Could split by commas, pull out the chunk that contains University or Institute
# get us most of the way there
# University, Institut, Johns Hopkins, then stragglers...

institution_df <- big_author_df %>%
  select(full_affiliation = authorAffiliation.value)

# Split by commas to get lists
splits <- str_split(institution_df$full_affiliation, ', ')
pattern <- 'University|Johns Hopkins|Institut'

clean_inst <- map_chr(splits, \(x) {
  out <- x[which(str_detect(x, pattern))]
  if (length(out) == 0) {
    return(NA_character_)
  } else if (length(out) == 1) {
    return(out)
  } else {
    return(out[[1]])
  }
})

clean_inst

institution_df$clean_name <- clean_inst
institution_df <- institution_df %>%
  mutate(clean_name = case_when(
    str_detect(clean_name, 'Johns Hopkins') ~ 'Johns Hopkins University',
    str_detect(clean_name, 'Maryland') ~ 'University of Maryland',
    .default = clean_name
  ))
get_str(institution_df)



## Get Websites ------------------------------------------------------------


# NOTE: daily limit of 100 queries, and we have 130 institutions to search.

# List of institutions
inst_list <- institution_df %>%
  filter(!is.na(clean_name)) %>%
  .$clean_name %>%
  unique

api_key <- Sys.getenv("API_KEY")
cse_id <- Sys.getenv("CSE_ID")

# # CAREFUL WITH THIS - limit of 100 queries per day
# test <- map(inst_list[130], ~ get_website(.x, api_key, cse_id))
# test



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

# Clean up names
pubs_df <- pubs_df %>%
  select(
    publication_id = pub_id,
    dataset_id = dataset_doi,
    everything()
  )
get_str(pubs_df)

# Save it to results list
results$publications <- pubs_df



# Produce -----------------------------------------------------------------


# Relation entity between authors and publications
get_str(just_authors)
get_str(pubs_df)

# Need to pull from big author DF which has dataset_ids
get_str(big_author_df)

produce_df <- big_author_df %>%
  select(
    name = authorName.value,
    author_id = authorIdentifier.value,
    dataset_id = global_id
  ) %>%
  mutate(
    split = str_split(name, ', '),
    name = map_chr(split, ~ paste(.x[2], .x[1]))
  ) %>%
  select(-split) %>%
  unique()

get_str(produce_df)

results$produce <- produce_df



# Subjects ----------------------------------------------------------------


get_str(dat$jhu_datasets)
get_str(dat$jhu_datasets$subjects)

subjects <- dat$jhu_datasets$subjects %>%
  unlist() %>%
  unique() %>%
  sort()

subjects_df <- data.frame(
  name = subjects,
  subject_id = 1:length(subjects)
)
get_str(subjects_df)
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

# Filter the list, keeping only valid (non-empty, non-NULL) data frames
licenses_filter <- Filter(is_valid_df, licenses)

licenses_df <- list_rbind(licenses_filter) %>%
  unique() %>%
  mutate(license_id = 1:nrow(.)) %>%
  rename(icon_uri = iconUri)
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
