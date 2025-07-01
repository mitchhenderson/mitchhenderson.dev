library(tidyverse)
library(arrow)
library(mgcv)
library(ggrepel)

# TODO
# Looks like some Sin Bin Returns are missing and the advantage is being retained to the end of match in the data
# (maybe dynamically only retain it for 10 minutes and not use the Sin Bin Return records?)

# Can we find pre match odds? This will allow a more reflective starting probability
# Convert to bayesian model
# Bring back 2014 and 2015 data?
# Post event state after sin bins and returns? or after ALL events?


# Import match info ------------------------------------------------------------
match_info_data_path <- "posts/_2025-06-29-ingame-win-probability-in-rugby-league/data/match_information"
match_info_files_list <- list.files(match_info_data_path, full.names = T)

match_info_data_import <- map(
  match_info_files_list, read_parquet
) |> bind_rows()

home_team_map <- match_info_data_import |>
  mutate(home_team_win = as_factor(if_else(homeFinalScore >= awayFinalScore, 1L, 0L)),
         draw = ifelse(homeFinalScore == awayFinalScore, 1L, 0L)) |>
  filter(draw != 1) |>
  select(matchId, homeTeamId, awayTeamId, home_team_win)

# Import event data -------------------------------------------------------------
play_by_play_data_path <- "posts/_2025-06-29-ingame-win-probability-in-rugby-league/data/play_by_play"
play_by_play_files_list <- list.files(play_by_play_data_path, full.names = T)

play_by_play_data_import <- map(
  play_by_play_files_list, read_parquet
) |> bind_rows() |>
  mutate(type = case_match(
    title,
    "Sin Bin Return" ~ "Sin Bin Return", 
    "Sent Off" ~ "Sent Off",
    "Inside 10 Metres" ~ "Penalty",
    "Ruck Infringement" ~ "Penalty",
    .default = type)) |>
  filter(!type %in% c("Sin Bin Return"))

# ---------------------------------------------------------------------------------

data <- play_by_play_data_import |>
  left_join(home_team_map, by = join_by(matchId)) |>
  arrange(matchId, gameSeconds) |> 
  mutate(
    seconds_remaining = 4800L - gameSeconds,
    # Calculate points scored on each play
    points = case_when(
        type == "Try" ~ 4L,
        type == "Goal" ~ 2L,
        type == "OnePointFieldGoal" ~ 1L,
        type == "TwoPointFieldGoal" ~ 2L,
        .default = 0L
    ),
    # Determine if home team scored (positive) or away team scored (negative)
    home_points = if_else(teamId == homeTeamId, points, -points),
    # Calculate running score differential BEFORE each play
    points_diff_at_end_of_play = cumsum(home_points),
    .by = matchId) |>
  select(matchId, gameSeconds, event_type = type, teamId, homeTeamId, seconds_remaining, points_diff_at_end_of_play, home_team_win)

# Dynamic sin bin returns ------------------------------------------

# Step 1: Create virtual sin bin return events
virtual_returns <- data |>
  filter(event_type == "SinBin") |>
  mutate(
    gameSeconds = gameSeconds + 600,  # 10 minutes = 600 seconds later
    synthetic_row = TRUE
  )

# Step 2: Add virtual_return column to original data
data_with_flag <- data |>
  mutate(synthetic_row = FALSE)

# Step 3: Combine original data with virtual returns
combined_data <- bind_rows(data_with_flag, virtual_returns)

# Step 4: Calculate player advantage
data_with_auto_returns <- combined_data |>
  arrange(matchId, gameSeconds) |>
  mutate(
    advantage_change = case_when(
      event_type == "SinBin" & teamId == homeTeamId ~ -1L,      # Home team loses a player
      event_type == "SinBin" & teamId != homeTeamId ~ +1L,      # Away team loses a player (home advantage)
      event_type == "Sent Off" & teamId == homeTeamId ~ -1L,     # Home team loses a player
      event_type == "Sent Off" & teamId != homeTeamId ~ +1L,     # Away team loses a player (home advantage)
      event_type == "Sin Bin Return" & teamId == homeTeamId ~ +1L,  # Home team gets player back
      event_type == "Sin Bin Return" & teamId != homeTeamId ~ -1L,  # Away team gets player back (home disadvantage)
      .default = 0L
    ),
    player_advantage = cumsum(advantage_change),
    .by = matchId
  ) |>
  filter(gameSeconds <= 4800,
        !is.na(home_team_win))


# Modeling --------------------------------------------------------------------------


# This is the formula that aligns with the nflWAR paper's logic.
# It predicts the probability of winning from any given state.
tictoc::tic()
nrl_wp_model <- bam(
  home_team_win ~ te(points_diff_at_end_of_play, seconds_remaining, k = 10),
  data = data_with_auto_returns,
  family = "binomial"
)
tictoc::toc()

summary(nrl_wp_model)
save(nrl_wp_model, file = "bam_nrl_wp_model.RData")

match_to_plot <- "20141112550"

single_game_data <- data_with_auto_returns |> 
  filter(matchId == match_to_plot) |>
  complete(gameSeconds = 0:4800) |>
  fill(c(-gameSeconds, -synthetic_row), .direction = "down") |>
  mutate(
    synthetic_row = if_else(is.na(synthetic_row), T, F),
    seconds_remaining = 4800 - gameSeconds,
    predicted_wp = predict(nrl_wp_model, newdata = pick(everything()), type = "response")
  )

# Plot final_wp vs. gameSeconds
ggplot(single_game_data, aes(x = gameSeconds, y = predicted_wp)) +
  geom_step(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_point(
    data = single_game_data %>% filter(event_type %in% c("Home_Try", "Away_Try", 
                                                          "Home_Goal", "Away_Goal", 
                                                          "Home_OnePointFieldGoal", "Away_OnePointFieldGoal")),
    color = "red",
    size = 3
  ) +
  ggrepel::geom_text_repel(data = single_game_data |> filter(synthetic_row == F), 
                  aes(label = points_diff_at_end_of_play)) +
scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 4800)) +
    labs(
      title = paste("Win Probability Chart for Match:", match_to_plot),
      subtitle = "Home Team (Team_A) vs. Away Team (Team_B)",
      x = "Game Seconds",
      y = "Home Team Win Probability"
    ) +
    theme_minimal()


# Calibration plot --------------------------------------------------

# Create probability bins
calibration_data <- data_with_auto_returns %>%
  # Convert factor back to numeric for calculation
  mutate(predicted_wp = predict(nrl_wp_model, newdata = pick(everything()), type = "response"),
         home_team_win_numeric = as.numeric(as.character(home_team_win))) %>%
  # Create bins of size 0.05 for the predicted WP
  mutate(wp_bin = cut(predicted_wp, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  # Group by the bin
  group_by(wp_bin) %>%
  # Calculate the actual win rate and the average predicted rate in that bin
  summarise(
    n_plays = n(),
    actual_win_rate = mean(home_team_win_numeric),
    predicted_win_rate = mean(predicted_wp)
  ) %>%
  # Remove bins with no plays (if any)
  filter(n_plays > 0)

  ggplot(calibration_data, aes(x = predicted_win_rate, y = actual_win_rate)) +
    geom_point(aes(size = n_plays), alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "NRL Win Probability Model Calibration",
      x = "Predicted Win Probability",
      y = "Actual Win Probability",
      caption = "Points should lie on the dashed red line for a perfectly calibrated model."
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
