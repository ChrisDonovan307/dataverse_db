# Table of Contents
# 2024-11-20


# Housekeeping ------------------------------------------------------------


# Run this first
source('r/4_scripts/housekeeping.R')

# README
knitr::knit('README.rmd')



# Workflow ----------------------------------------------------------------


# Figure out how the APIs work with some small tests
'r/4_scripts/explore_apis.R'

# Pull relevant Johns Hopkins data from search API and metrics API
'r/4_scripts/pull_johns_hopkins.R'

# Join data, explore, make csvs
source('r/4_scripts/wrangle_johns_hopkins.R')

# Explore clean tables
'r/4_scripts/explore_clean_tables.R'

