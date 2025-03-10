library(tidyverse)
library(patchwork)

raw_data <- read_csv("posts/_2025-03-04-modelling-try-scoring-ability-without-the-influence-of-team-strength/SFM_data_byPlayer.csv")

complete_data <- raw_data |>
  mutate(season_nbr = match(season, sort(unique(season))) - 1,
         .by = name_player) |>
  mutate(share_center = N_games_center / (N_games_center + N_games_left + N_games_right)) |>
  drop_na(share_center) |>
  filter(!(name_player == "thierry-henry" & season == "2011/12")) |>
  arrange(name_player, kick_off)

player_ordered <- complete_data |>
  pull(name_player) |>
  unique()

complete_data |>
  count(goals_in_match) |>
  mutate(n = n / sum(n),
         n = round(n, 4))

complete_data |>
  count(goals_in_match)

complete_data <- complete_data |>
  mutate(goal_cats = if_else(goals_in_match >= 3, 3, goals_in_match))

# Descriptive chart
(
complete_data |>
  ggplot(aes(x = goals_in_match)) +
  geom_histogram()
|
complete_data |>
  ggplot(aes(x = goal_cats)) +
  geom_histogram() +
  scale_x_continuous(
    limits = c(-0.5, 5.5),
    breaks = seq(0, 5, by = 1)
  )
)
