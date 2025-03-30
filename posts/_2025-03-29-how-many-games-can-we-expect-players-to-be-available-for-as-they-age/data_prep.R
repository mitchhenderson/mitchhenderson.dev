# nrl_data working directory (not on Github)

library(tidyverse)
library(arrow)

player_list <- map(
  list.files("data/player_list", full.names = T),
  read_parquet
  ) |>
  bind_rows()

match_information <- map(
  list.files("data/match_information", full.names = T),
  read_parquet
) |>
  bind_rows()

season_info <- match_information |>
  select(matchId, season, homeTeamId, awayTeamId)

dates_of_birth <- read_csv("data/dates_of_birth_2014-2024_players.csv") |>
  mutate(dob = dmy(dob))

# Create mapping tables to account for inconsistent names
player_mapping <- player_list |>
  summarize(
    # Take the most frequent firstName for each playerId
    firstName = names(sort(table(firstName), decreasing = TRUE)[1]),
    # Take the most frequent lastName for each playerId
    lastName = names(sort(table(lastName), decreasing = TRUE)[1]),
    .by = playerId
  )

# Remove low involvement players
players_with_less_than_100_games <- player_list |>
  drop_na(playerId) |>
  left_join(season_info, by = join_by(matchId)) |>
  mutate(teamId = if_else(team == "Home", homeTeamId, awayTeamId)) |>
  mutate(team_number_of_matches = n_distinct(matchId),
         .by = c(season, teamId)) |>
  summarise(matches_played = n_distinct(matchId),
            .by = c(playerId)) |>
  filter(matches_played < 100) |>
  pull(playerId)

# Remove player seasons who moved midyear
players_who_moved_midseason <- player_list |>
  drop_na(playerId) |>
  left_join(season_info, by = join_by(matchId)) |>
  mutate(teamId = if_else(team == "Home", homeTeamId, awayTeamId)) |>
  mutate(team_number_of_matches = n_distinct(matchId),
         .by = c(season, teamId)) |>
  count(playerId, season, teamId) |>
  count(playerId, season) |>
  filter(n == 2)


data <- player_list |>
  drop_na(playerId) |>
  filter(!playerId %in% players_with_less_than_100_games) |>
  select(-firstName, -lastName) |> # Remove inconsistent names
  left_join(player_mapping, by = join_by(playerId)) |> # Add the consistent ones
  left_join(season_info, by = join_by(matchId)) |> # Add season
  anti_join(players_who_moved_midseason, by = join_by(playerId, season)) |>
  mutate(teamId = if_else(team == "Home", homeTeamId, awayTeamId),
         fullName = glue::glue("{firstName} {lastName}")) |>
  mutate(team_number_of_matches = n_distinct(matchId),
         .by = c(season, teamId)) |>
  summarise(matches_played = n_distinct(matchId),
            .by = c(season, playerId, fullName, team_number_of_matches)) |>
  left_join(dates_of_birth, by = join_by(playerId == nrl_com_playerId)) |>
  mutate(reference_date = ymd(glue::glue("{season}-01-01")),
         age_at_start_of_year = interval(dob, reference_date) %/% years(1),
         availability_perc = matches_played / team_number_of_matches,
         availability_adjusted = (availability_perc * (team_number_of_matches - 1) + 0.5) / team_number_of_matches,
         playerId = factor(playerId),
         season = as.integer(season))

data |> write_csv("nrl_availability_and_ages_2014-2024.csv")
