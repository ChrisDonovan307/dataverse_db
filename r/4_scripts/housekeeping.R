# Install package manager
suppressPackageStartupMessages(
  if (!requireNamespace('pacman')) {
    install.packages('pacman', dependencies = TRUE, quiet = TRUE)
  }
)

# Load conflicted for namespace conflicts
pacman::p_load(conflicted)

# Set common conflict preferences
conflicts_prefer(
  dplyr::select,
  dplyr::filter,
  dplyr::rename,
  .quiet = TRUE
)

# Print options
options(
  max.print = 990,
  pillar.print_max = 990,
  pillar.print_min = 990
)

# Project utilities
try(pacman::p_load_gh('ChrisDonovan307/projecter'))
