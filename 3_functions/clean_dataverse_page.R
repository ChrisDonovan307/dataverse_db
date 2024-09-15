pacman::p_load(
  dplyr
)

clean_dataverse_page <- function(df) {
  df %>%
    .$data %>%
    .$items
}
