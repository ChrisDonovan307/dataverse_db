# Convert to table insert statements
# 2024-11-23

# Function to convert data frame to sql insert statements
# Then write out to .sql file



# Housekeeping -------------------------------------------------------------


source('r/4_scripts/housekeeping.R')
pacman::p_load(
  dplyr,
  purrr,
  stringr
)
dat <- readRDS('r/2_clean/jhu_dfs.Rds')
sets <- dat$sets
dat <- dat[names(dat) != 'sets']
names(dat)



# -------------------------------------------------------------------------

get_str(dat$dataset)
dat$dataset$pub_date



# Clean Data --------------------------------------------------------------


## Rename tables - some are not allowed in oracle
names(dat) <- case_when(
  names(dat) == 'file' ~ 'files',
  names(dat) == 'grant' ~ 'grants',
  names(dat) == 'user' ~ 'users',
  names(dat) == 'subject' ~ 'subjects',
  names(dat) == 'keyword' ~ 'keywords',
  .default = names(dat)
)

names(dat)


## Check max str length of all variables
map(dat, \(x) map(x, \(y) max(str_length(y), na.rm = TRUE)))
# dataset and collection descriptions are above 2000


# 1. First we have to make sure our pub_date and timestamp columns are in the
#   very silly Oracle formats.
# 2. Let's also just limit any value to 950 characters. This was set to 1000
#   at first but they ended up a few over - not sure why.
# 3. There is some bullshit in some of the file records where there are
#   thousands of lines of posterior distribution data. Just remove anything that
#   says posterior so we don't have to deal with it. I'll just replace it with
#   'posterior nonsense'
# 4. Also remove white space from either side of any value
# 5. Apparently having the word 'Memory' in a value fucks things up for SQL.
#   Let's replace it with 'Mem'
# 6. change & to 'and'. Ampersands mess with SQL
# 7. The numeric NAs do not jive with SQL. They need to be null. Presumably
#   without quotes?
# 8. Backslashes \ mess with SQL. Thinks it is end of command. There are lots
#   of newlines in descriptions (\n). Remove those.
# 9. Make file size in KB, no scientific notation
get_str(dat)
dat <- map(dat, \(df){
  df %>%
    unique() %>%
    mutate(
      across(
        .cols = matches('pub_date|start_date'),
        .fns = ~ format(.x, format = "%d-%b-%Y")
      ),
      across(
        .cols = matches('timestamp'),
        .fns = ~ format(.x, format = "%d-%b-%Y %I:%M:%S")
      ),
      across(where(is.character), ~ str_sub(.x, end = 750)),
      across(where(is.character), ~ case_when(
        str_detect(.x, 'Posterior') ~ 'Posterior nonsense',
        .default = .x
      )),
      across(everything(), str_trim),
      across(everything(), ~ str_replace_all(.x, '&', 'and')),
      across(everything(), ~ str_remove_all(.x, '\n')),
      across(matches('filesize'), ~ format(round(as.numeric(.x) / 1000, 1), scientific = FALSE)))
})


## Fix analyze table so that it has no nulls in primary key
dat$analyzes <- dat$analyzes %>%
  filter(!is.na(sw_ID), !is.na(ds_ID))

## Fix a problem with produce table
# Get rid of pub id 52 and 95
dat$produce <- dat$produce %>%
  filter(!pub_ID %in% c('52', '95'))

dat$publication <- dat$publication %>%
  filter(!pub_ID %in% c('52', '95'))



# Define function ---------------------------------------------------------


to_sql_insert <- function(df, table_name) {
  insert_statements <- apply(df, 1, function(row) {

    # Escape single quotes for SQL
    row <- gsub("'", "''", row)

    # Replace NA with NULL, put single quotes around other values
    values <- sapply(row, function(value) {
      if (is.na(value)) {
        "NULL"
      } else {
        paste0("'", value, "'")
      }
    })

    # Create insert statements
    values <- paste(values, collapse = ", ")
    sprintf("insert into %s values (%s);", table_name, values)
  })

  # Add header to each section of inserts by table name
  # Also add a couple of blank lines after each section before next header
  out <- c(
    paste0('-- Insert into ', table_name, ' ', strrep('-', 50)),
    '',
    insert_statements,
    '',
    ''
  )

  return(out)
}



# Create Insert Statements ------------------------------------------------


# Choose order to input tables
names(dat)
table_names <- c(
  'institution',
  'license',
  'funding_agency',
  'grants',
  'software_license',
  'software',
  'users',
  'root_dataverse',
  'collection',
  'dataset',
  'files',
  'subjects',
  'registered_user',
  'author',
  'affiliation',
  'publication',
  'produce',
  'file_upload',
  'file_download',
  'dataset_upload',
  'dataset_download',
  'keywords',
  'funds',
  'admin',
  'manage_dataverse',
  'manage_collection',
  'contact',
  'analyzes'
)

# Filter dat to prelim set
prelim <- dat[names(dat) %in% table_names]

# Get insert statements
out <- imap(prelim, ~ to_sql_insert(.x, .y))

# Reorder lists of statements to the order above to meet FK constraints
out <- out[match(table_names, names(out))]
names(out)

# Write to a .sql file
out %>%
  unlist() %>%
  writeLines('sql/2_insert.sql')



# Clear -------------------------------------------------------------------


# Clear data and run garbage collection. R hates this function for some reason
clear_data()
gc()
