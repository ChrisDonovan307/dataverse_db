# Graveyard

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

# Expand Everything? ------------------------------------------------------


#' NOTE: This is not working how I want it to. Might have to wrangle each
#' entity individually

# get_str(dat$jhu_datasets_native)
# get_str(dat$jhu_datasets_native[[1]]$latestVersion)
#
# df <- dat$jhu_datasets_native %>%
#   map(list_flatten) %>%
#   map(list_flatten) %>%
#   map(list_flatten)
# get_str(df)
#
# # Get all possible variable names
# var_names <- df %>%
#   map(names) %>%
#   unlist() %>%
#   unique() %>%
#   sort()
#
# #
# expand_list_element <- function(element) {
#   df_cols <- element %>% keep(is.data.frame)
#   other_cols <- element %>% discard(is.data.frame)
#   unnested <- map_dfc(df_cols, ~ as_tibble(.x))  # Convert all data frames to tibbles
#   repeated_other_cols <- map_dfc(other_cols, ~ rep(.x, each = nrow(unnested)))
#   combined_df <- bind_cols(repeated_other_cols, unnested)
#   return(combined_df)
# }
#
# df_combined <- df %>%
#   map(expand_list_element)
#
# get_str(df_combined)



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


# Produce -----------------------------------------------------------------


# Relation entity between authors and publications
get_str(just_authors)
get_str(pubs_df)

# Need to pull from big author DF which has dataset_ids (actually don't)
# get_str(big_author_df)

produce_df <- big_author_df %>%
  select(
    name = authorName.value,
    auth_id = authorIdentifier.value,
    ds_id = global_id
  ) %>%
  mutate(
    split = str_split(name, ', '),
    name = map_chr(split, ~ paste(.x[2], .x[1]))
  ) %>%
  select(-split) %>%
  unique()

get_str(produce_df)
results$produce <- produce_df

