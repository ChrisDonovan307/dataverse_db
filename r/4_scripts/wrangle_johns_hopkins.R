#' Wrangle Johns Hopkins
#' 2024-09-15

#' Wrangling, cleaning, exploring Johns Hopkins data. Making csv datasets


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  purrr,
  stringr,
  tidyr,
  httr,
  jsonlite,
  lorem,
  lubridate,
  stringdist
)

# Load data from API
dat <- readRDS('r/1_raw/jhu_metadata.RDS')

# Source scripts,
source('r/3_functions/wrangling_utilities.R')
source('r/3_functions/get_str.R')
source('r/4_scripts/housekeeping.R')

# Initialize results list
results <- list()



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
    ds_id = dataset_persistent_id,
    filetype = file_type,
    filesize = size_in_bytes,
    title = name,
    everything(),
    -type,
    -url,
    -file_content_type
  ) %>%
  mutate(pub_date = as_datetime(published_at), .keep = 'unused')
get_str(files_df)

# Fill in the file_id if one is ever missing.
# Just adding row number to the dataset_id
files_df <- files_df %>%
  mutate(file_id = case_when(
    is.na(file_id) ~ paste0(ds_id, '/', row_number()),
    .default = file_id
  ))
get_str(files_df)

# Files are ready to go now
results$file <- files_df



# Datasets ----------------------------------------------------------------
## Main API ----------------------------------------------------------------


get_str(dat$jhu_datasets, 1)
get_str(dat$jhu_datasets_native[[1]])
# url links to persistentUrl

datasets_df <- dat$jhu_datasets %>%
  select(name:storageIdentifier, subjects)
get_str(datasets_df)

#' To get download counts for datasets, lets add up the file counts, because the
#' metrics API for directly getting download counts doesn't work.
get_str(files_df)
get_str(datasets_df)
# files_df.dataset_persistent_id == datasets_df.global_id

# Get download counts
counts <- files_df %>%
  group_by(ds_id) %>%
  summarize(file_downloads = sum(downloads)) %>%
  arrange(desc(file_downloads))
counts
get_str(counts)
table(counts$file_downloads)

# Join this into the datasets
datasets_df <- left_join(datasets_df,
                         counts,
                         join_by(global_id == ds_id))
get_str(datasets_df)
table(datasets_df$file_downloads)

# Harmonize names
datasets_df_clean <- datasets_df %>%
  select(-storageIdentifier, -type, -name_of_dataverse) %>%
  select(
    ds_id = global_id,
    col_id = identifier_of_dataverse,
    title = name,
    url,
    description,
    pub_date = published_at,
    subjects,
    file_downloads
  ) %>%
  mutate(pub_date = as_datetime(pub_date))
get_str(datasets_df_clean)



## Subjects ----------------------------------------------------------------


# Pulling out subject info here - multi value attribute
# Datasets can have more than one subject
get_str(datasets_df_clean)

subjects <- datasets_df_clean %>%
  select(ds_id, subjects) %>%
  unnest(cols = c(subjects)) %>%
  rename(subject = subjects)
get_str(subjects)

# That's it, just dataset IDs and the subject names
results$subject <- subjects

# Now we can remove subjects from the datasets
datasets_df_clean <- datasets_df_clean %>%
  select(-subjects)



## Add Native Info --------------------------------------------------------


## Get license information from native API and join it
get_str(dat$jhu_datasets_native[[1]])
get_str(dat$jhu_datasets_native[[1]]$latestVersion$license)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks)
get_str(dat$jhu_datasets_native[[1]]$latestVersion)

# For a single file, get persistent id (ds_id) and license
dat$jhu_datasets_native[[1]]$latestVersion$datasetPersistentId
dat$jhu_datasets_native[[1]]$latestVersion$license[[1]]
nrow(dat$jhu_datasets_native[[1]]$latestVersion$files)

# Now pull them all
ids_licenses <- map(dat$jhu_datasets_native, ~ {
  lic_id <- ifelse(is.null(.x$latestVersion$license[[1]]), NA, .x$latestVersion$license[[1]])
  ds_id <- .x$latestVersion$datasetPersistentId
  n_files <- nrow(.x$latestVersion$files)
  data.frame(ds_id, lic_id, n_files)
}) %>%
  bind_rows()

# Join to datasets, rename col_id back to identifier cause I'm silly
datasets_df_clean <- datasets_df_clean %>%
  left_join(ids_licenses) %>%
  rename(identifier = col_id)
get_str(datasets_df_clean)

# Save it
results$dataset <- datasets_df_clean
# But we will have to save it again after collections - need numeric col_id



# Collections -------------------------------------------------------------


get_str(dat$jhu_dataverses)
head(dat$jhu_dataverses)
# 62 dataverses (collections)

# This is easy, just get rid of the type column and leave else as is
# also add the root dataverse
collections_df <- dat$jhu_dataverses %>%
  select(
    col_id = identifier,
    title = name,
    pub_date = published_at,
    everything(),
    -type,
    -url
  ) %>%
  mutate(
    root_id = 1,
    pub_date = as_datetime(pub_date)
  ) %>%

  # Adding an entry for root
  add_row(
    col_id = 'jhu',
    title = 'JHU Root Dataverse',
    pub_date = as_datetime('2013-12-17'),
    description = 'This is the root dataverse for the JHU Installation',
    root_id = 1
  )
get_str(collections_df)


## Add n_files and downloads
# Get DF of n_files and downloads from datasets
# Group by identifiers to add it to collections
get_str(results$dataset)
ds_summary <- results$dataset %>%
  group_by(identifier) %>%
  summarize(
    n_files = sum(n_files, na.rm = TRUE),
    file_downloads = sum(file_downloads, na.rm = TRUE)
  )
ds_summary

# Join it to collections
collections_df <- left_join(
  collections_df,
  ds_summary,
  by = join_by(col_id == identifier)
) %>%
  rename(identifier = col_id)
get_str(collections_df)

# Bring in a numeric col_id and remove old one
collections_df <- collections_df %>%
  mutate(col_id = 1:nrow(.)) %>%
  select(col_id, everything())
get_str(collections_df)


## Now we can join it to datasets to fix the col_id and make it numeric
results$dataset <- collections_df %>%
  select(col_id, identifier) %>%
  right_join(results$dataset) %>%
  select(-identifier)
get_str(results$dataset)


# Save collections too, but remove identifier and ds_id
# Also put columns in intuitive order
# Also add the dataset count
results$collection <- results$dataset %>%
  select(col_id) %>%
  group_by(col_id) %>%
  summarize(n_datasets = n()) %>%
  right_join(collections_df) %>%
  select(
    col_id,
    root_id,
    title,
    description,
    pub_date,
    n_files,
    n_datasets,
    file_downloads
  )
get_str(results$collection)



# Authors -----------------------------------------------------------------


# Explore data
get_str(dat$jhu_datasets_native[[1]]$latestVersion)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value, 3)
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
  bind_rows() %>%
  rename(
    name = authorName.value,
    ds_id = global_id
  )
get_str(big_author_df)



## Consolidate Author Name -------------------------------------------------


# String distance matrix for author names
dist_matrix <- stringdistmatrix(
  big_author_df$name,
  big_author_df$name,
  method = "jw"
)

# Cluster by distance
hc <- hclust(as.dist(dist_matrix))

# Set threshold for similar names
clusters <- cutree(hc, h = 0.2)

# Create a mapping of cluster IDs to representative names
cluster_to_name <- tapply(
  big_author_df$name,
  clusters,
  function(names) names[1]
)

# Map old names to consolidated names
big_author_df$name <- as.vector(cluster_to_name[clusters])
get_str(big_author_df)



## Consolidate Institution -------------------------------------------------


## Consolidate affiliations from author DF
get_str(big_author_df)

# Recode NA to None so string dist doesn't flip out
big_author_df$authorAffiliation.value <- ifelse(
  is.na(big_author_df$authorAffiliation.value),
  'None',
  big_author_df$authorAffiliation.value
)

dist_matrix <- stringdistmatrix(
  big_author_df$authorAffiliation.value,
  big_author_df$authorAffiliation.value,
  method = "jw"
)

# Cluster by distance
hc <- hclust(as.dist(dist_matrix))

# Set threshold for similar names
clusters <- cutree(hc, h = 0.2)

# Create a mapping of cluster IDs to representative names
cluster_to_name <- tapply(
  big_author_df$authorAffiliation.value,
  clusters,
  function(names) names[1]
)

# Map old names to consolidated names
big_author_df$clean_inst <- as.vector(cluster_to_name[clusters])
get_str(big_author_df)



## Finish Authors ----------------------------------------------------------


# Get slimmer DF with just author names and IDs
# We are dropping Orcid IDs - just using integers
# Also adding registered user IDs
authors <- big_author_df %>%
  mutate(orcid = str_remove_all(authorIdentifier.value, "https://orcid.org/")) %>%
  group_by(name) %>%
  summarize(
    count = n(),
    ds_ids = list(ds_id),
    institution = list(authorAffiliation.value),
    orcid = first(na.omit(orcid))
  ) %>%
  mutate(
    auth_id = 1:nrow(.),
    ru_id = 1:nrow(.)
  )
get_str(authors)
# Save this for institutions below

# Save author table with just auth_id, ru_id, and name
results$author <- authors %>%
  select(auth_id, ru_id, name, orcid)
get_str(results$author)



# Institution, affiliation --------------------------------------------------


## Link registered users to institutions
get_str(authors)
get_str(big_author_df)

# Needs int_id and ru_id
# Join authors and big authors by name
affiliation <- inner_join(authors, big_author_df) %>%
  select(name = clean_inst, ru_id) %>%
  unique()
get_str(affiliation)
# This is M-N with registered users and institutions


## Now need a table of just unique institutions, name, id, maybe address
set.seed(42)
inst <- affiliation %>%
  select(name) %>%
  unique() %>%
  mutate(
    int_id = row_number(),
    address = paste0(
      sample(1:999, nrow(.), replace = TRUE),
      ' ',
      str_to_title(ipsum_words(nrow(.), collapse = FALSE)),
      ' ',
      sample(c('Road', 'Lane', 'Boulevard', 'Street'), nrow(.), replace = TRUE),
      ', ',
      sample(c('Lincoln', 'Burlington', 'Portland', 'Newton', 'Colchester'), nrow(.), replace = TRUE),
      ', ',
      sample(c('Maryland', 'California', 'Virginia', 'New York'))
    )
  ) %>%
  select(int_id, name, address)
get_str(inst)

# Save it
results$institution <- inst


## Now go back to affiliation, recode with int_id
get_str(affiliation)
affiliation <- affiliation %>%
  inner_join(inst) %>%
  select(int_id, ru_id)
get_str(affiliation)

# Save it
results$affiliation <- affiliation



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
    ds_id = pubs$global_id[[row]]
  )
}) %>%
  list_rbind() %>%
  mutate(pub_id = as.numeric(factor(citation))) %>%
  filter(!is.na(pub_id)) %>%

  # This is a bit jenky, losing a couple of pubs, but don't want to deal with it.
  # Some pubs are associated with more than one dataset. only 2 of them. Dont
  # want to make a whole new table for the M-N, so just losing them here.
  group_by(pub_id) %>%
  summarize(
    citation = first(citation),
    url = first(url),
    ds_id = first(ds_id)
  )
get_str(pubs_df)

# Clean up names
pubs_df <- pubs_df %>%
  select(
    pub_id,
    ds_id,
    everything()
  )
get_str(pubs_df)

# Save it to results list
results$publication <- pubs_df



# Produce -----------------------------------------------------------------


# Relation entity between authors and publications
get_str(results$author)
get_str(results$dataset)
get_str(results$publication)
get_str(big_author_df)

# Datasets table connects both authors and pubs
# All we need is auth id and pub id
produce <- authors %>%
  select(auth_id, ds_ids) %>%
  unnest(ds_ids) %>%
  rename(ds_id = ds_ids) %>%
  right_join(results$publication) %>%
  inner_join(results$author, relationship = 'many-to-many') %>%
  select(auth_id, pub_id)
get_str(produce)

# Save it
results$produce <- produce



# Dataset Upload ----------------------------------------------------------


# This is linking authors to datasets. Many to many.
# Datasets already has a pub_date
get_str(big_author_df)
get_str(results$author)
get_str(results$dataset)

ds_up <- authors %>%
  unnest(ds_ids) %>%
  select(
    name,
    auth_id,
    ds_id = ds_ids
  ) %>%
  group_by(ds_id) %>%
  inner_join(results$dataset, by = 'ds_id') %>%
  select(
    ds_id,
    auth_id,
    timestamp = pub_date
  )
get_str(ds_up)

# Save it
results$dataset_upload <- ds_up



# File Upload -------------------------------------------------------------


# Linking authors to files
get_str(results$dataset_upload)
get_str(results$file)
file_upload <- results$dataset_upload %>%
  select(-timestamp) %>%
  inner_join(results$file, relationship = 'many-to-many') %>%
  ungroup() %>%
  select(
    auth_id,
    file_id,
    timestamp = pub_date
  ) %>%
  unique()
get_str(file_upload)

# Save this
results$file_upload <- file_upload



# Keywords ----------------------------------------------------------------


get_str(dat$jhu_datasets)
get_str(dat$jhu_datasets$keywords)

keywords <- dat$jhu_datasets %>%
  select(
    ds_id = global_id,
    keywords
  ) %>%
  unnest(keywords)
get_str(keywords)

results$keyword <- keywords



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
  mutate(lic_id = 1:nrow(.)) %>%
  select(lic_id, name, url = uri)
get_str(licenses_df)

# Save it
results$license <- licenses_df


## Now we can recode the lic id in datasets table
results$dataset <- licenses_df %>%
  select(-url) %>%
  right_join(results$dataset, by = join_by('name' == 'lic_id')) %>%
  select(-name) %>%
  relocate(lic_id, .after = last_col())
get_str(results$dataset)



# Grants et al. ------------------------------------------------------------


# Three tables here: Grant, funds, funding_agency

# Check grant numbers
get_str(dat$jhu_datasets_native)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value)
get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$citation$fields$value, 3)

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

# Remove missing, combing into DF, fix up names
grants_df <- Filter(Negate(is.logical), out) %>%
  list_rbind() %>%
  mutate(
    grant_id = 1:nrow(.),
    ds_id = str_split_i(dataset_url, '//', 2) %>%
      str_replace('.org/', ':'),
    amount = sample(seq(10000, 500000, 10000), nrow(.), replace = TRUE),
    .keep = 'unused'
  ) %>%
  select(
    grant_id,
    ds_id,
    number = grant_number,
    agency = grant_agency,
    amount
  )
get_str(grants_df)

# Clean up funding agency names
grants_df <- grants_df %>%
  mutate(agency = case_when(
    str_detect(agency, 'National Science.*Foundation') ~ 'National Science Foundation',
    str_detect(agency, 'Agency for.*Development') ~ 'U.S. Agency for International Development',
    str_detect(agency, 'Institute.*of Health') ~ 'National Institutes of Health',
    str_detect(agency, 'Institute on Aging') ~ 'National Institute on Aging',
    str_detect(agency, 'health.*Management') ~ 'National Institute of Healthcare Management',
    str_detect(agency, 'Heart Association') ~ 'National Heart Association',
    .default = agency
  ))

# Pull out funding agency into its own table
funding_agency <- grants_df %>%
  select(agency, amount) %>%
  group_by(agency) %>%
  summarize(
    total_amount = sum(amount)
  ) %>%
  mutate(
    agency_id = 1:nrow(.),
    location = ipsum_words(nrow(.), collapse = FALSE),
  ) %>%
  select(agency_id, name = agency, everything())
get_str(funding_agency)

# Save funding agency table
results$funding_agency <- funding_agency

# To get the FUNDS table, join grant table with funding agency table
funds <- inner_join(
  grants_df,
  funding_agency,
  by = join_by('agency' == 'name')
) %>%
  select(grant_id, ds_id, agency_id)
get_str(funds)

# Save it
results$funds <- funds

# now we can finally cut down grants table to what we need
grants_df <- grants_df %>%
  select(grant_id, number, amount)
get_str(grants_df)

# Save it
results$grant <- grants_df



# Software, SW lic, Analyze -----------------------------------------------


get_str(dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$value, 3)

# Names
dat$jhu_datasets_native[[1]]$latestVersion$metadataBlocks$software$fields$typeName
# We want swTitle, swLicense, consider swDependency
# swCodeRepositoryLink

vars <- c(
  'swLicense',
  'swTitle',
  'swDescription',
  'swLanguage',
  'swCodeRepositoryLink'
)

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
      # Get indices for pertinent variables
      indices <- map(vars, ~ {
        which(software_metadata$fields$typeName == .x)
      })

      # Pull licenses based on indices
      var_list <- map2(vars, indices, \(var, index) {
        if (length(index) == 0) {
          return(NA)
        } else if (length(index) == 1) {
          return(software_metadata$fields$value[[index]])
        }
      }) %>%
        c(dataset$persistentUrl)

      return(var_list)

    }
  }
})

get_str(out)
get_str(out[[1]])


sw_df <- Filter(Negate(is.logical), out) %>%
  map(~ as.data.frame(.x) %>%
        setNames(c(
          'name',
          'title',
          'description',
          'language',
          'repo_link',
          'ds_id'
        ))) %>%
  list_rbind() %>%
  mutate(ds_id = str_split_i(ds_id, '//', 2))
get_str(sw_df)

# Make sw license df
sw_license_df <- sw_df %>%
  select(name, ds_id) %>%
  filter(!is.na(name)) %>%
  group_by(name) %>%
  mutate(
    license_url = case_when(
      str_detect(name, '^MIT') ~ 'https://spdx.org/licenses/X11.html',
      str_detect(name, 'GPL-3') ~ 'https://www.gnu.org/licenses/gpl-3.0.html',
      str_detect(name, '^BSD 2') ~ 'https://www.freebsd.org/copyright/freebsd-license/',
      str_detect(name, '^BSD 3') ~ 'https://spdx.org/licenses/BSD-3-Clause.html',
      .default = NA_character_
    ),
    name = case_when(
      str_detect(name, '^MIT') ~ 'MIT',
      str_detect(name, 'GPL-3') ~ 'GPL-3.0',
      str_detect(name, '^BSD 2') ~ 'BSD 2',
      str_detect(name, '^BSD 3') ~ 'BSD 3',
      str_detect(name, '^Other') ~ 'Not Specified',
      .default = NA_character_
    ),
    gpl_compatible = case_when(
      str_detect(name, '^Other|Not Specified') | is.na(name) ~ FALSE,
      str_detect(name, '^GNU|GPL|^MIT|^BSD') ~ TRUE,
      .default = NA
    ),
    sw_lic_id = case_when(
      name == 'GPL-3.0' ~ 1,
      name == 'MIT' ~ 2,
      name == 'BSD 2' ~ 3,
      name == 'BSD 3' ~ 4,
      name == 'Not Specified' ~ 5
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(name)) %>%
  mutate(ds_id = str_replace_all(ds_id, '\\.org/', ':')) %>% # fix ds_id format
  rename(url = license_url) %>%
  unique()
get_str(sw_license_df)

# Now add sw_lic_id to datasets
# finishing dataset table here. making order of columns intuitive
results$dataset <- sw_license_df %>%
  select(sw_lic_id, ds_id) %>%
  right_join(results$dataset) %>%
  relocate(sw_lic_id, .after = last_col()) %>%
  unique() %>%
  select(
    ds_id,
    col_id,
    lic_id,
    sw_lic_id,
    title,
    description,
    pub_date,
    file_downloads,
    n_files,
    url
  )
get_str(results$dataset)


# Pull out sw license table, connects straight to dataset
sw_license_table <- sw_license_df %>%
  select(sw_lic_id, name, url, gpl_compatible) %>%
  unique()
get_str(sw_license_table)

# Save it
results$software_license <- sw_license_table


# 2 more to make here
# analyze: sw id, ds id, title, description
# software: sq id, name, description
# Start with analyze. Need to give each software package an ID
get_str(sw_df)

# Get a table of unique softwares and give them IDs
temp_sw <- sw_df %>%
  select(language) %>%
  unique() %>%
  filter(!is.na(language)) %>%
  arrange(language) %>%
  mutate(sw_id = row_number())
get_str(temp_sw)

# Now join it with sw_df to give it IDs
analyze <- sw_df %>%
  left_join(temp_sw)
get_str(analyze)

# Now we can pull out the plain software table
software <- analyze %>%
  select(
    sw_id,
    name = language
  ) %>%
  unique() %>%
  filter(!is.na(sw_id)) %>%
  mutate(description = ipsum_words(nrow(.), collapse = FALSE))
get_str(software)

# Save it
results$software <- software


## Last thing is the analyze table
get_str(analyze)
analyze <- analyze %>%
  select(
    sw_id,
    ds_id,
    title,
    description,
    repo_url = repo_link
  ) %>%
  mutate(ds_id = str_replace(ds_id, '.org/', ':'))
get_str(analyze)

# Save it
results$analyzes <- analyze



# User Groups -----------------------------------------------------------


# Let's make every author a registered user. And add some more registered users
# How many authors do we have?
get_str(results$author)
nrow(results$author)
# 806

# Let's make it 1000 registered users, including authors.
# So another 194 registered users who are not authors
set.seed(42)
reg_users <- data.frame(
  ru_id = 1:1000,
  auth_id = c(results$author$auth_id, rep(NA, 194)),
  name = c(results$author$name, str_to_title(ipsum_words(194, collapse = FALSE))),
  privilege = sample(c('read', 'write'), 1000, replace = TRUE),
  pw_hash = paste0(
    ipsum_words(1000, collapse = FALSE),
    sample(1:9999, 1000, replace = TRUE)
  )
)
get_str(reg_users)
tail(reg_users, 25)


## All 1000 registered users must be regular users. So add 500 more
# and also add emails to all 1500
# Start with user ref
set.seed(42)
user_ref <- data.frame(
  ru_id = c(reg_users$ru_id, rep(NA, 500)),
  u_id = 1:1500,
  user_ref = 1:1500,
  email = c(
    # reg_users$email,
    paste0(
      ipsum_words(1500, collapse = FALSE),
      sample(1:1500, replace = FALSE),
      '@email.com'
    )
  )
)
get_str(user_ref)


## From this we can make our user table
user <- user_ref %>%
  select(u_id, email)
get_str(user)

# Save it
results$user <- user


## Add back to reg users to get reg user table
get_str(user_ref)
reg_users <- user_ref %>%
  inner_join(reg_users) %>%
  select(
    ru_id,
    u_id,
    auth_id,
    name:last_col()
  )
get_str(reg_users)
# Note that we are leaving in u_id so that we don't need to have user_ref table
# Although we are leaving user ref table in the code anyway.
# Also keeping author ID so we can use it a bit later, but will remove it then


## Now go back and finish user ref table
user_ref <- user_ref %>%
  select(u_id, ru_id, user_ref)
get_str(user_ref)

# Save it
results$user_reference <- user_ref


## Admins. Make 25 of them. Just need ru_id and random start date
# Let's use 25 reg users who are not authors
set.seed(42)
admin <- reg_users %>%
  filter(is.na(auth_id)) %>%
  slice_sample(n = 25, replace = FALSE) %>%
  select(ru_id) %>%
  mutate(start_date = sample(
    seq(ymd('2015-08-01'), ymd('2017-08-01'), 'days'),
    25,
    replace = TRUE
  ))
get_str(admin)

# Save it
results$admin <- admin


## Now we can remove author id from reg users and save it
results$registered_user <- reg_users %>%
  select(-auth_id)
get_str(results$registered_user)


## Now that registered users are linked to authors, we can remove author name
results$author <- results$author %>%
  select(-name)
get_str(results$author)



# Manage DS, Collection ---------------------------------------------------


## Manage Dataverse
# Make 2 admins in charge of it
get_str(results$admin)

# Just make it straight up. Should be after 2016-12 though, thats when admins started
# DV Admins
dv_admins <- results$admin$ru_id[1:2]
set.seed(42)
manage_dv <- data.frame(
  ru_id = sample(dv_admins, 15, replace = TRUE),
  root_id = 1,
  timestamp = as_datetime(
    runif(
      15,
      as.numeric(as_datetime('2015-01-01')),
      as.numeric(now())
    )
  ),
  description = replicate(
    15,
    paste0(
      'Regular maintenance, code ',
      paste(
        sample(c('A', 'B', 'C', 'X', 'Y', 'Z', 1:9), 6, replace = TRUE),
        collapse = ''
      )
    )
  )
)
get_str(manage_dv)

# Save it
results$manage_dataverse <- manage_dv


## Manage Collections
# Make the other 23 admins manage collections. And get selections of collections
# Making this table 50 deep
col_admins <- results$admin$ru_id[3:25]
collections <- results$collection$col_id

# Start with collection IDs and pub_dates
set.seed(42)
cols <- results$collection %>%
  select(col_id, pub_date) %>%
  slice_sample(n = 50, replace = TRUE)
get_str(cols)

# Add other columns
set.seed(42)
manage_col <- cols %>%
  mutate(
    ru_id = sample(col_admins, 50, replace = TRUE),
    col_id = sample(collections, 50, replace = TRUE),
    timestamp = as_datetime(
      runif(
        n(),
        as.numeric(pub_date),
        as.numeric(now())
      )
    ),
    description = paste0(
      'Updated collection ',
      col_id,
      ' dataset ',
      sample(results$dataset$ds_id, 50, replace = TRUE)
    )
  ) %>%
  select(-pub_date)
get_str(manage_col)

# Save it
results$manage_collection <- manage_col



# Download DS, File -------------------------------------------------------


## DS Download
# random selection of non-reg users, dataset ID, and timestamp
# Make it 25 deep

# First get non-reg users
non_reg_users <- results$user %>%
  left_join(results$user_reference) %>%
  anti_join(results$registered_user) %>%
  pull(u_id)
non_reg_users

# Get a selection of 5 datasets to download more
sets <- results$dataset$ds_id[1:5]

set.seed(42)
ds_down <- data.frame(
  ds_id = c(
    sample(results$dataset$ds_id, 25, replace = TRUE),
    sample(sets, 75, replace = TRUE)
  ),
  u_id = sample(non_reg_users, 100, replace = TRUE)
) %>%
  mutate(
    timestamp = as_datetime(
      runif(
        n(),
        as.numeric(as_datetime('2024-11-01')),
        as.numeric(now())
      )
    )
  )
get_str(ds_down)

# Save it
results$dataset_download <- ds_down

# Also save the sets here for reference
results$sets <- sets



## File Download
# 25 deep
set.seed(42)
file_down <- data.frame(
  file_id = sample(results$file$file_id, 25, replace = TRUE),
  u_id = sample(non_reg_users, 25, replace = TRUE)
) %>%
  mutate(
    timestamp = as_datetime(
      runif(
        n(),
        as.numeric(as_datetime('2024-11-01')),
        as.numeric(now())
      )
    )
  )
get_str(file_down)

# Save it
results$file_download <- file_down



# Contact -----------------------------------------------------------------


# Just making up contacts between users and authors
# Let's say that every author is contacted... because we do not specify a
# contact author.

# Need selection of users (not reg), selection of datasets to find authors
# Let's do 15

# Get some users
get_str(results$user)
set.seed(42)
contact_users <- results$user %>%
  anti_join(results$registered_user) %>%
  slice_sample(n = 15, replace = FALSE)
get_str(contact_users)

# Get some datasets
set.seed(42)
contact_datasets <- results$dataset %>%
  slice_sample(n = 15, replace = FALSE)
get_str(contact_datasets)

# Bind them together, then join with authors to get full contacts list
contact <- bind_cols(contact_users, contact_datasets) %>%
  select(u_id, ds_id) %>%
  left_join(results$dataset_upload, relationship = 'many-to-many') %>%
  select(-auth_id) %>%
  unique()
get_str(contact)

# Now add a new timestamp that is later than the old timestamp
# Also a message
contact <- contact %>%
  mutate(
    timestamp = as_datetime(
      runif(
        n(),
        as.numeric(timestamp),
        as.numeric(now())
      )
    ),
    message = c(
      'This is a great dataset!',
      'This is absolutely the worst dataset I have ever seen.',
      'Hello, could you please send supplementary table 2A?',
      'What exactly were you thinking when you did this?',
      'I wrote my term paper using your data and I got a C+. Can you ask my professor to raise my grade?',
      'Did you know that you published your home address and social security number in this dataset?',
      'Great work.',
      'We should work together on a paper.',
      'This dataset was stolen from my cupboard last month.',
      'Not your best work.',
      'Thanks for sharing this.',
      'You give me hope that anyone can become an academic, no matter how little skill they have.',
      'Will you be my thesis advisor?',
      'I\'m out of ideas of things to write.',
      'This is the last one.'
    )
  )
get_str(contact)

# Save it
results$contact <- contact



# Root Dataverse ----------------------------------------------------------


# Just a table with a single record for JHU
root <- data.frame(
  root_id = 1,
  title = 'Johns Hopkins Research Data Repository',
  url = 'https://archive.data.jhu.edu/',
  description = 'An open access repository for Johns Hopkins University researchers to share their research data.'
)

# Save it
results$root_dataverse <- root



# Save and Clear ----------------------------------------------------------


## Check results
names(results)
get_str(results, 3)
map(results, get_str)

## Last minute wrangling
# Turn _id suffix into _ID to match diagrams
results <- map(results, \(x) {
  df <- x
  names(df) <- str_replace_all(names(df), '_id', '_ID')
  return(df)
})
map(results, get_str)

# Remove tables I'm deciding not to use
results <- results[names(results) != 'user_reference']
names(results)


## Saving
# As list of DFs
saveRDS(results, 'r/2_clean/jhu_dfs.Rds')

# Also save as separate CSVs
# iwalk(results, \(df, name) {
#   write.csv(df, paste0('csv/', name, '.csv'))
# })

# Clear
clear_data()
