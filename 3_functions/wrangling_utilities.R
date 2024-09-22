# Pull out names and emails from list and put in DF
extract_pairs <- function(x) {
  map_dfr(x, ~ {
    if(length(.x) == 2) {
      setNames(as.data.frame(t(.x)), c("name", "email"))
    } else {
      data.frame(name = NA, email = NA)
    }
  })
}

# Use Google custom search API to get website
get_website <- function(university_name, api_key, cse_id) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  response <- GET(base_url, query = list(
    key = api_key,
    cx = cse_id,
    q = university_name
  ))

  result <- fromJSON(content(response, "text"))
  browser()
  if (!is.null(result$items) && length(result$items) > 0) {
    return(result$items$link[1])
  } else {
    return(NA)
  }
}

# Function to check if a data frame is empty or NULL
is_valid_df <- function(df) {
  !is.null(df) && nrow(df) > 0
}
