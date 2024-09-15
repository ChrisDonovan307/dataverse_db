#' Scraping
#' 2024-09-15
#'
#' Run and format GET request, given input URL

pacman::p_load(
  GET,
  httr,
  dplyr
)

get <- function(url) {
  url %>%
    GET() %>%
    content(as = 'text') %>%
    fromJSON(flatten = TRUE)
}

