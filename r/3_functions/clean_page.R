pacman::p_load(
  dplyr
)

clean_page <- function(df) {
  df %>%
    .$data %>%
    .$items
}
