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
  jsonlite,
  lorem,
  lubridate
)

dat <- readRDS('r/1_raw/jhu_metadata.RDS')
source('r/3_functions/wrangling_utilities.R')
source('r/3_functions/get_str.R')
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
    dataset_id = dataset_persistent_id,
    everything(),
    -type
  )
get_str(files_df)

# Fill in the file_id if one is ever missing.
# Just adding row number to the dataset_id
files_df <- files_df %>%
  mutate(file_id = case_when(
    is.na(file_id) ~ paste0(dataset_id, '/', row_number()),
    .default = file_id
  ))
get_str(files_df)

# Files are ready to go now
results$files <- files_df



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
  group_by(dataset_id) %>%
  summarize(downloads = sum(downloads)) %>%
  arrange(desc(downloads))
counts
get_str(counts)
table(counts$downloads)

# Join this into the datasets
datasets_df <- left_join(datasets_df,
                         counts,
                         join_by(global_id == dataset_id))
get_str(datasets_df)
table(datasets_df$downloads)

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
    downloads
  ) %>%
  mutate(pub_date = as_datetime(pub_date))
get_str(datasets_df_clean)



## Subjects ----------------------------------------------------------------


# Pulling out subject info here - multi value attribute
# Datasets can have more than one subject
get_str(datasets_df_clean)

subjects <- datasets_df_clean %>%
  select(ds_id, subjects) %>%
  unnest(cols = c(subjects))
get_str(subjects)

# That's it, just dataset IDs and the subject names
results$subjects <- subjects

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

# Join to datasets
datasets_df_clean <- datasets_df_clean %>%
  left_join(ids_licenses)
get_str(datasets_df_clean)



## Save it -----------------------------------------------------------------


results$datasets <- datasets_df_clean



# Collections -------------------------------------------------------------


get_str(dat$jhu_dataverses)
head(dat$jhu_dataverses)
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
    root_id = 'jhu',
    pub_date = as_date(pub_date)
  ) %>%
  add_row( # Adding an entry for root
    col_id = 'jhu',
    title = 'JHU Root Dataverse',
    pub_date = as_date('2013-12-17'),
    description = 'This is the root dataverse for the JHU Installation',
    root_id = 'jhu'
  )
get_str(collections_df)


## Add n_files and downloads
# Get DF of n_files and downloads from datasets
get_str(results$datasets)
ds_summary <- results$datasets %>%
  group_by(ds_id) %>%
  summarize(
    col_id = col_id,
    n_files = sum(n_files, na.rm = TRUE),
    downloads = sum(downloads, na.rm = TRUE)
  )
ds_summary

# Join it to collections
collections_df <- left_join(collections_df, ds_summary)
get_str(collections_df)

# Save it
results$collections <- collections_df



# Authors -----------------------------------------------------------------


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
  bind_rows()
get_str(big_author_df)

# This is basically the author/dataset relationship entity
# we can pull that out, as well as a pure author dataset.
# and also use this to get institutions

### First get pure author dataset
get_str(big_author_df)
big_author_df <- big_author_df %>%
  rename(
    name = authorName.value,
    auth_id_type = authorIdentifierScheme.value,
    auth_id = authorIdentifier.value,
    ds_id = global_id
  ) %>%
  unique() %>%
  mutate(auth_id = str_sub(auth_id, start = -19)) %>%
  arrange(name)

# How many NAs
n_na <- sum(is.na(big_author_df$auth_id))

# Assign the ones missing an ID a 1:n_na id
big_author_df$auth_id[is.na(big_author_df$auth_id)] <- seq(1:n_na)
big_author_df$auth_id_type <- ifelse(is.na(big_author_df$auth_id_type),
                                      'dataverse_id',
                                      big_author_df$auth_id_type)
get_str(big_author_df)
# Leave this for later

# Now just get columns for author table
authors <- big_author_df %>%
  select(auth_id, name)
get_str(authors)

# Save it
results$authors <- authors



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
  mutate(pub_id = as.numeric(factor(citation)))
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
results$publications <- pubs_df



# Produce -----------------------------------------------------------------


# Relation entity between authors and publications
get_str(results$authors)
get_str(results$datasets)
get_str(results$publications)
get_str(big_author_df)

# Datasets table connects both authors and pubs
# All we need is auth id and pub id
produce <- big_author_df %>%
  select(auth_id, ds_id) %>%
  right_join(results$publications) %>%
  inner_join(results$authors, relationship = 'many-to-many') %>%
  select(auth_id, pub_id)
get_str(produce)

# Save it
results$produce <- produce



# Dataset Upload ----------------------------------------------------------


# This is linking authors to datasets. Many to many.
# Datasets already has a pub_date
get_str(big_author_df)
get_str(results$authors)
get_str(results$datasets)

ds_up <- big_author_df %>%
  select(
    name,
    auth_id,
    ds_id
  ) %>%
  group_by(ds_id) %>%
  inner_join(results$datasets, by = 'ds_id') %>%
  select(
    ds_id,
    auth_id,
    timestamp = pub_date
  )
get_str(ds_up)

results$ds_uploads <- ds_up



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

results$keywords <- keywords



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
results$licenses <- licenses_df


## Now we can recode the lic id in datasets table
results$datasets <- licenses_df %>%
  select(-url) %>%
  right_join(results$datasets, by = join_by('name' == 'lic_id')) %>%
  select(-name) %>%
  relocate(lic_id, .after = last_col())
get_str(results$datasets)



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
    ds_id = str_split_i(dataset_url, '//', 2),
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



# Software ----------------------------------------------------------------


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
  mutate(ds_id = str_replace_all(ds_id, '\\.org/', ':')) # fix ds_id format
get_str(sw_license_df)

# Now add sw_lic_id to datasets
results$datasets <- sw_license_df %>%
  select(sw_lic_id, ds_id) %>%
  right_join(results$datasets) %>%
  relocate(sw_lic_id, .after = last_col()) %>%
  unique()
get_str(results$datasets)

# Pull out sw license table, connects straight to dataset
sw_license_table <- sw_license_df %>%
  select(sw_lic_id, name, license_url, gpl_compatible) %>%
  unique()
get_str(sw_license_table)

# Save it
results$sw_license <- sw_license_table


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
    description
  )
get_str(analyze)

# Save it
results$analyzes <- analyze



# Create Groups -----------------------------------------------------------


# Making made up data for Users, Registered Users, and Admins

## Start with users.
# Just do IDs for now, then add attributes later
users <- data.frame(
  user_id = 1:250
  # email = paste0(
  #   lorem::ipsum_words(100, collapse = FALSE),
  #   '@madeupemail.com'
  # )
)
get_str(users)


## Registered Users
# This will be a subset of users. Let's say half - 125
set.seed(42)
reg <- users %>%
  slice_sample(n = 125) %>%
  mutate(
    reg_id = row_number()
  )
get_str(reg)

# Add these reg ids back into users
users <- users %>%
  full_join(reg)
get_str(users)

# Let's make 25 of our authors into registered users.
# Links will just connect the IDs
# Just slice and bind
links <- results$authors %>%
  slice(1:25) %>%
  bind_cols(reg[1:25, ]) %>%
  select(auth_id, reg_id)
get_str(links)

## Now use links to add author IDs to reg users and reg users to author IDs
# Reg
reg <- reg %>%
  left_join(links)
get_str(reg)

# Authors
results$authors <- results$authors %>%
  left_join(links)
get_str(results$authors)


## Admins
# Of the 125 reg users, 25 are authors. Let's turn 25 of the last 100 into admins
get_str(reg)

# First add 25 more IDs to reg. Start at 26 to dodge authors
reg <- reg %>%
  mutate(admin_id = c(rep(NA, 25), 1:25, rep(NA, 75)))
get_str(reg)

# Now make admins table, as subset of registered users
set.seed(42)
admins <- reg %>%
  filter(!is.na(admin_id)) %>%
  select(-user_id, -auth_id) %>%
  mutate(
    privilege = sample(c('superuser', 'edit', 'read'), nrow(.), replace = TRUE),
    collection_id = sample(results$collections$col_id, nrow(.), replace = FALSE),
    start_date = Sys.Date() - sort(sample(3000:7000, 25))
  )
get_str(admins)
results$admins <- admins

# Add attributes to users and ditch reg id
get_str(users)
users <- users %>%
  mutate(
    email = paste0(
      lorem::ipsum_words(250, collapse = FALSE),
      '@madeupemail.com')
  ) %>%
  select(-reg_id)
get_str(users)
results$users <- users

# Add attributes to registered users, pulling names from authors, making up rest
get_str(reg)
set.seed(42)
reg_names <- ipsum_words(200, collapse = FALSE)
set.seed(42)
reg <- reg %>%
  left_join(users) %>%
  left_join(authors) %>%
  select(-admin_id) %>%
  mutate(
    name = ifelse(is.na(name), reg_names[row_number()], name),
    pw_hash = ipsum_words(nrow(.), collapse = FALSE),
    privilege = sample(c('read', 'view', 'download'), nrow(.), replace = TRUE)
  )
get_str(reg)
results$reg <- reg

get_str(authors)



# Save and Clear ----------------------------------------------------------


# Save as list of DFs
saveRDS(results, 'r/2_clean/jhu_dfs.Rds')

# Also save as separate CSVs
iwalk(results, \(df, name) {
  write.csv(df, paste0('r/6_outputs/jhu_', name, '.csv'))
})

# Clear
clear_data()
