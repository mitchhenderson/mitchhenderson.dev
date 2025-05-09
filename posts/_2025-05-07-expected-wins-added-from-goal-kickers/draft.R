library(tidyverse)
library(arrow)

player_stats_import <- map(
  list.files("posts/_2025-05-07-expected-wins-added-from-goal-kickers/data/player_stats", full.names = T),
  read_parquet
) |>
  bind_rows()
