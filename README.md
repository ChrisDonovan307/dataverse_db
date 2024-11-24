---
title: "Dataverse Database"
author: "Chris Donovan, Ben Sevey\n"
date: "2024-11-24"
output: 
  github_document:
    toc: yes
    toc_depth: 3
    number_sections: 3
---



# Introduction

Dataverse scraping and database programming for or CS5040 - Database Systems course. 

Data wrangling is in the `r/` folder. The `r/table_of_contents.R` file navigates through the workflow. Once the data is cleaned, it converts data frames into sql insert statements and saves it into the `sql/` folder.

On the sql side, scripts are numbered in order starting with 1_tables. The run.sql file is the script Byung shared that sets some options and has outputs saved to .out files. The runall.sql script runs all the project scripts starting with 1. 

