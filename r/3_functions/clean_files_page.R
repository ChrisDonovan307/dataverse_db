pacman::p_load(
  dplyr
)

clean_files_page <- function(df) {
  df %>%
    .$data %>%
    .$items %>%
    select(-c(md5, checksum.type, checksum.value))
}
