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
  filter(!type %in% c("VideoReferee", "CaptainsChallenge", "GameTime", "Interchange", "Sin Bin Return"))
  
# Import scoring data ------------------------------------------------------------
#scoring_data_path <- "posts/_2025-06-29-ingame-win-probability-in-rugby-league/data/scoring"
#scoring_files_list <- list.files(scoring_data_path, full.names = T)

#scoring_data_import <- map(
#  scoring_files_list, read_parquet
#) |> bind_rows()


#play_by_play_data_import |> count(title, type) |> View()
# match_info_data_import |> filter(matchId == 20161110120) |> View()

# ---------------------------------------------------------------------------------
events_that_turnover_possession <- c("Penalty",
                                     "LineDropout",
                                     "Error",
                                     "OnePointFieldGoalMissed",
                                     "TwoPointFieldGoalMissed",
                                     "Sent Off",
                                     "SinBin")

data <- play_by_play_data_import |>
  left_join(home_team_map, by = join_by(matchId)) |>
  arrange(matchId, gameSeconds) |> 
  mutate(
    game_phase = as_factor(if_else(gameSeconds <= 4800, "Regulation", "GoldenPoint")),
    seconds_remaining = case_when(
      game_phase == "Regulation" ~ 4800L - gameSeconds,
      
      # For Golden Point, we treat it as counting down from 600s (10 mins)
      # This maintains the "countdown" format which is easier for models to interpret
      # than a count-up.
      game_phase == "GoldenPoint" ~ (4800L + 600L) - gameSeconds,
      
      # Default case just in case
      .default = 0L
    ),
    advantage_change = case_when(
      type == "SinBin" & teamId == homeTeamId ~ -1L,      # Home team loses a player
      type == "SinBin" & teamId != homeTeamId ~ +1L,      # Away team loses a player (home advantage)
      type == "Sent Off" & teamId == homeTeamId ~ -1L,     # Home team loses a player
      type == "Sent Off" & teamId != homeTeamId ~ +1L,     # Away team loses a player (home advantage)
      type == "Sin Bin Return" & teamId == homeTeamId ~ +1L,  # Home team gets player back
      type == "Sin Bin Return" & teamId != homeTeamId ~ -1L,  # Away team gets player back (home disadvantage)
      .default = 0L  # No change for other event types
    ),
    # Calculate running player advantage (starting from 0)
    player_advantage = lag(cumsum(advantage_change), default = 0),
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
    points_diff_at_start_of_play = lag(cumsum(home_points), default = 0),
    team_in_posession_at_end_of_play = case_when(
      type %in% events_that_turnover_possession & teamId == homeTeamId ~ awayTeamId,
      type %in% events_that_turnover_possession & teamId == awayTeamId ~ homeTeamId,
      .default = teamId),
    # Create the new, context-aware event type
    type = case_when(
      teamId == homeTeamId ~ paste0("Home_", type),
      teamId == awayTeamId ~ paste0("Away_", type),
      .default = type
    ),
    type = as_factor(type),
    .by = matchId) |>
  select(matchId, gameSeconds, event_type = type, game_phase, seconds_remaining, player_advantage, points_diff_at_start_of_play, team_in_posession_at_end_of_play, home_team_win) |>
  slice(9000:12000)

# Check that there's no event_type that perfectly predict a win or loss in the dataset
data %>%
  group_by(event_type) %>%
  summarise(
    n_obs = n(),
    win_rate = mean(as.numeric(as.character(home_team_win)))
  ) %>%
  arrange(-win_rate)

skimr::skim(data)

# Dynamic sin bin returns ------------------------------------------

# Step 1: Create virtual sin bin return events
virtual_returns <- data |>
  filter(event_type %in% c("Home_SinBin", "Away_SinBin")) |>
  mutate(
    gameSeconds = gameSeconds + 600,  # 10 minutes = 600 seconds later
    event_type = if_else(event_type == "Home_SinBin", "Home_Sin Bin Return", "Away_Sin Bin Return"),
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
      event_type == "Home_SinBin" ~ -1L,      # Home team loses a player
      event_type == "Away_SinBin" ~ +1L,      # Away team loses a player (home advantage)
      event_type == "Home_Sent Off" ~ -1L,     # Home team loses a player
      event_type == "Away_Sent Off" ~ +1L,     # Away team loses a player (home advantage)
      event_type == "Home_Sin Bin Return" ~ +1L,
      event_type == "Away_Sin Bin Return" ~ -1L,
      .default = 0L
    ),
    player_advantage = lag(cumsum(advantage_change), default = 0),
    .by = matchId
  )

# Post score state synthetic data ----------------------------------------
# Step 1: Create synthetic post-score rows
post_score_rows <- data_with_auto_returns |>
  filter(event_type %in% c("Home_Try", "Home_Goal", "Home_OnePointFieldGoal", "Home_TwoPointFieldGoal",
                           "Away_Try", "Away_Goal", "Away_OnePointFieldGoal", "Away_TwoPointFieldGoal")) |>
  mutate(
    gameSeconds = gameSeconds + 0.1,
    event_type = "Post_Score_State",
    synthetic_row = TRUE
  )

# Step 2: Add flag to original data and combine
data_with_flag <- data_with_auto_returns |>
  mutate(synthetic_row = FALSE)

combined_data <- bind_rows(data_with_flag, post_score_rows)

# Step 3: Calculate score differential
data_with_post_score <- combined_data |>
  arrange(matchId, gameSeconds) |>
  mutate(
    points = case_when(
      event_type == "Home_Try" ~ 4,
      event_type == "Home_Goal" ~ 2, 
      event_type == "Home_OnePointFieldGoal" ~ 1,
      event_type == "Home_TwoPointFieldGoal" ~ 2,
      event_type == "Away_Try" ~ -4,
      event_type == "Away_Goal" ~ -2, 
      event_type == "Away_OnePointFieldGoal" ~ -1,
      event_type == "Away_TwoPointFieldGoal" ~ -2,
      .default = 0
    ),
    points_diff_at_start_of_play = lag(cumsum(points), default = 0),  # Show score before event
    .by = matchId
  )

training_data <- data_with_post_score |>
  select(matchId, gameSeconds, event_type, game_phase, seconds_remaining, player_advantage, points_diff_at_start_of_play, team_in_posession_at_end_of_play, home_team_win) |>
  mutate(event_type = as_factor(event_type)) |>
  filter(game_phase == "Regulation", seconds_remaining <= 4800)

base_data <- training_data |> select(home_team_win, points_diff_at_start_of_play, seconds_remaining)


# Modeling --------------------------------------------------------------------------

# Step 1b: Fit a simple GAM
base_wp_model <- gam(home_team_win ~ te(points_diff_at_start_of_play, seconds_remaining, k=10), 
                     data = base_data, family = "binomial")

# Step 1c: Add the baseline WP to your main dataframe
training_data$base_wp <- predict(base_wp_model, newdata = training_data, type = "response")


# Use dplyr's lag() function to get the WP of the previous state
training_data <- training_data |>
  mutate(
    # The WP of the previous state
    previous_wp = lag(base_wp, default = 0.50), 
    # The change caused by the current event's state
    wpa = base_wp - previous_wp,
    .by = matchId
  )


wpa_model <- lm(wpa ~ 0 + event_type, data = training_data)
summary(wpa_model)

# Get the predicted WPA for each event
training_data$predicted_wpa <- predict(wpa_model, newdata = training_data)

# Create the new, trustworthy WP chart
final_wp_data <- training_data |>
  mutate(
    # The new WP is the baseline of 50% plus the cumulative sum of event values
    final_wp = 0.50 + cumsum(predicted_wpa),
    .by = matchId
  )

match_to_plot <- "20141112910"
single_game_data <- final_wp_data |> filter(matchId == match_to_plot)



# Identify the key non-scoring events you want to label
events_to_label <- single_game_data %>%
  filter(event_type %in% c("Home_LineBreak", "Away_LineBreak", "Home_Try", "Away_Try", "Home_Goal", "Away_Goal"))

# Plot final_wp vs. gameSeconds
ggplot(single_game_data, aes(x = gameSeconds, y = final_wp)) +
  geom_step(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_point(
    data = single_game_data %>% filter(event_type %in% c("Home_Try", "Away_Try", 
                                                          "Home_Goal", "Away_Goal", 
                                                          "Home_OnePointFieldGoal", "Away_OnePointFieldGoal")),
    color = "red",
    size = 3
  ) +
    geom_text_repel(
      data = events_to_label,
      aes(label = event_type),
      box.padding = 0.5,
      point.padding = 0.5,
      size = 3,
      color = "black"
    ) +
scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = paste("Win Probability Chart for Match:", match_to_plot),
      subtitle = "Home Team (Team_A) vs. Away Team (Team_B)",
      x = "Game Seconds",
      y = "Home Team Win Probability"
    ) +
    theme_minimal()


tictoc::tic()
wp_model <- gam(
  home_team_win ~ 
    # 1. Keep the complex 2D smooth for regulation time
    te(points_diff_at_start_of_play, seconds_remaining, k = 10) +
    
    # The impact of specific, discrete events
    event_type,
    
  data = training_data,
  family = "binomial"
)
tictoc::toc()
summary(wp_model)

# Get the predicted probability for the "1" class (home team win)
training_data$predicted_wp <- predict(wp_model, type = "response")


# Calibration plot --------------------------------------------------

# Create probability bins
calibration_data <- training_data %>%
  # Convert factor back to numeric for calculation
  mutate(home_team_win_numeric = as.numeric(as.character(home_team_win))) %>%
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


# WP plot for a match ---------------------------------------------------
single_game_data <- training_data %>%
  filter(matchId == match_to_plot)

single_game_data |>
  ggplot(aes(x = gameSeconds, y = predicted_wp)) +
    geom_step(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dotted") +
    geom_point(
      data = single_game_data %>% filter(event_type %in% c("Home_Try", "Away_Try", 
                                                           "Home_Goal", "Away_Goal", 
                                                           "Home_OnePointFieldGoal", "Away_OnePointFieldGoal")),
      color = "red",
      size = 3
    ) +
  scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = paste("Win Probability Chart for Match:", match_to_plot),
        subtitle = "Home Team (Team_A) vs. Away Team (Team_B)",
        x = "Game Seconds",
        y = "Home Team Win Probability"
      ) +
      theme_minimal()


library(ggrepel)

# Identify the key non-scoring events you want to label
events_to_label <- single_game_data %>%
  filter(event_type %in% c("Home_LineBreak", "Away_LineBreak", "Home_Try", "Away_Try", "Home_Goal", "Away_Goal"))

ggplot(single_game_data, aes(x = gameSeconds, y = predicted_wp)) +
  geom_step(color = "blue", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  
  # Add labels for the key events
  geom_text_repel(
    data = events_to_label,
    aes(label = event_type),
    box.padding = 0.5,
    point.padding = 0.5,
    size = 3,
    color = "black"
  ) +

  # Add points for scoring events
  geom_point(
    data = single_game_data %>% filter(event_type %in% c("Try", "Goal", "OnePointFieldGoal")),
    color = "red",
    size = 3
  ) +
  scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = paste("Win Probability Chart for Match:", match_to_plot),
        subtitle = "Home Team (Team_A) vs. Away Team (Team_B)",
        x = "Game Seconds",
        y = "Home Team Win Probability"
      ) +
      theme_minimal()
