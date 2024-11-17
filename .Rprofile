source("renv/activate.R")
# Load utils so we can use install.packages()
library(utils)

# Set CRAN mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

# Install pacman if not installed already
suppressPackageStartupMessages(
  if (!requireNamespace('pacman')) {
    install.packages('pacman', dependencies = TRUE, quiet = TRUE)
  }
)

# Load conflicted for namespace conflicts
pacman::p_load(conflicted)
pacman::p_load_gh('ChrisDonovan307/projecter')

# Set common conflict preferences
conflicts_prefer(
  dplyr::select,
  dplyr::filter
)

# Load table of contents script
if (Sys.info()["sysname"] == "Mac") {
  tryCatch({
    system('open "table_of_contents.R"')
    cat('\nLoading Table of Contents')
  }, error = function(e) {
    cat('\nCould not open table of contents.')
  })

} else if (Sys.info()["sysname"] == "Windows") {
  tryCatch({
    shell.exec('table_of_contents.R')
    cat('\nLoading Table of Contents')
  }, error = function(e) {
    cat('\nCould not open table of contents.')
  })

} else {
  cat('\nCould not open table of contents.')
}
