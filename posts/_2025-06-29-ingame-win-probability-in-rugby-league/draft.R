library(tidyverse)
library(arrow)
library(brms)
library(tidybayes)

# Import ---------------------------------------------------------------------------

import_data <- function(data_folder) {
  data_path <- paste0(
    "posts/_2025-06-29-ingame-win-probability-in-rugby-league/data/",
    data_folder
  )
  files_list <- list.files(data_path, full.names = T)

  data_import <- map(
    files_list,
    read_parquet
  ) |>
    bind_rows()
}

match_info_import <- import_data("match_information")
event_data_import <- import_data("play_by_play")

# Prepare data for modeling --------------------------------------------------------

data <- event_data_import |>
  left_join(match_info_import, by = join_by(matchId)) |>
  arrange(matchId, gameSeconds) |>
  mutate(
    home_team_win = if_else(
      homeFinalScore > awayFinalScore,
      1L,
      0L
    ),
    draw_at_fulltime = max(ifelse(gameSeconds > 4800, 1L, 0L)),
    type = if_else(title == "Sent Off", "SendOff", type),
    game_minutes = gameSeconds / 60,
    minutes_remaining = 80 - game_minutes,
    points = case_when(
      type == "Try" ~ 4L,
      type == "Goal" ~ 2L,
      type == "OnePointFieldGoal" ~ 1L,
      type == "TwoPointFieldGoal" ~ 2L,
      .default = 0L
    ),
    home_points = if_else(teamId == homeTeamId, points, -points),
    event = case_when(
      type == "GameTime" ~ type,
      teamId == homeTeamId ~ glue::glue("Home {type}"),
      teamId == awayTeamId ~ glue::glue("Away {type}")
    ),
    possession_change = case_when(
      event %in%
        c(
          "Away Error",
          "Away Penalty",
          "Home SetRestart",
          "Home FortyTwenty",
          "Home TwentyForty",
          "Home Goal",
          "Away GoalMissed",
          "Home LineBreak",
          "Away LineDropout",
          "Home OnePointFieldGoal",
          "Away OnePointFieldGoalMissed",
          "Home TwoPointFieldGoal",
          "Away TwoPointFieldGoalMissed",
          "Away OffsideWithinTenMetres",
          "Away RuckInfringement",
          "Home Try",
          "Away SinBin"
        ) ~
        "Home",
      event %in%
        c(
          "Home Error",
          "Home Penalty",
          "Away SetRestart",
          "Away FortyTwenty",
          "Away TwentyForty",
          "Away Goal",
          "Home GoalMissed",
          "Away LineBreak",
          "Home LineDropout",
          "Away OnePointFieldGoal",
          "Home OnePointFieldGoalMissed",
          "Away TwoPointFieldGoal",
          "Home TwoPointFieldGoalMissed",
          "Home OffsideWithinTenMetres",
          "Home RuckInfringement",
          "Away Try",
          "Home SinBin"
        ) ~
        "Away",
      .default = NA_character_
    ),
    .by = matchId
  ) |>
  filter(
    gameSeconds <= 4800,
    draw_at_fulltime != 1,
    !is.na(home_team_win),
    minutes_remaining != 0
  ) |>
  distinct(matchId, event, minutes_remaining, .keep_all = TRUE)


player_events <- data |>
  filter(
    event %in% c("Home SinBin", "Away SinBin")
  ) |>
  mutate(
    player_change = case_when(
      event == "Home SinBin" ~ -1,
      event == "Away SinBin" ~ +1
    )
  ) |>
  mutate(
    minutes_remaining = minutes_remaining - 10,
    player_change = player_change * -1,
    event = if_else(
      event == "Home SinBin",
      "Home SinBinReturns",
      "Away SinBinReturns"
    )
  ) |>
  filter(minutes_remaining >= 0) |>
  select(matchId, event, home_team_win, minutes_remaining, player_change)


full_data <- data |>
  bind_rows(player_events) |>
  arrange(matchId, -minutes_remaining) |>
  group_by(matchId) |>
  mutate(
    player_change = case_when(
      event == "Home SinBin" ~ -1,
      event == "Away SinBin" ~ +1,
      event == "Home SendOff" ~ -1,
      event == "Away SendOff" ~ +1,
      .default = player_change
    ),
    points_differential = cumsum(if_else(is.na(home_points), 0, home_points)),
    player_advantage = cumsum(if_else(is.na(player_change), 0, player_change))
  ) |>
  #  filter(!(minutes_remaining < 5 & abs(points_differential) > 18)) |>
  fill(possession_change, .direction = "down") |>
  ungroup() |>
  mutate(
    possession_change = case_when(
      !is.na(possession_change) ~ possession_change,
      # Random sample for NA values (only kickoffs)
      runif(n()) < 0.5 ~ "Home",
      .default = "Away"
    ),
    home_gains_possession = if_else(possession_change == "Home", 1, 0),
    inverse_minutes_remaining = 1 / (minutes_remaining + 0.1)
  ) |>
  select(
    matchId,
    event,
    points_differential,
    minutes_remaining,
    inverse_minutes_remaining,
    home_team_win,
    home_gains_possession,
    player_advantage
  )

# Modeling -------------------------------------------------------------------------

# fit_brms_v2 <- readRDS(
#   "posts/_2025-06-29-ingame-win-probability-in-rugby-league/nrl_wp_brms_model_v2_full.rds"
# )

priors <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b")
)

v2_formula <- bf(
  home_team_win ~
    1 +
      points_differential * inverse_minutes_remaining +
      home_gains_possession +
      player_advantage
)


tictoc::tic()
fit_brms_full <- brm(
  formula = v2_formula,
  data = full_data,
  family = bernoulli(link = "cauchit"),
  prior = priors,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  refresh = 500,
  seed = 2534,
  backend = "rstan",
  file = "nrl_wp_brms_model_v2_full"
)
tictoc::toc()

tictoc::tic()
loo_v2_full <- add_criterion(fit_brms_full, criterion = "loo")
tictoc::toc()


summary(fit_brms_full)

plot(fit_brms_full)

pp_check(fit_brms_full, ndraws = 100)
pp_check(your_model, type = "stat", stat = "mean")
pp_check(your_model, type = "stat_2d", stat = c("mean", "sd"))

fitted_probs <- fitted(fit_brms_full)[, 1]
summary(fitted_probs)
hist(fitted_probs)


# Calibration plot -------------------------------------------------------------

calibration_data_sample <- data_only_scoring |> slice_sample(n = 10000)


calibration_draws <- calibration_data_sample |>
  add_epred_draws(fit_brms, ndraws = 1000)

# Summarize the calibration data
calibration_summary <- calibration_draws |>
  group_by(matchId, points_differential, minutes_remaining, home_team_win) |>
  summarise(median_pred = median(.epred), .groups = "drop") |>
  # Use 10 bins for stable calibration results
  mutate(pred_bin = cut(median_pred, breaks = 10, include.lowest = TRUE)) |>
  group_by(pred_bin) |>
  summarise(
    n_games = n(),
    mean_pred_prob = mean(median_pred),
    mean_actual_prob = mean(home_team_win)
  )

# --- 4. Plot the Calibration Chart ---
ggplot(calibration_summary, aes(x = mean_pred_prob, y = mean_actual_prob)) +
  geom_abline(linetype = "dashed", color = "gray50") +
  geom_point(aes(size = n_games), color = "midnightblue", alpha = 0.8) +
  geom_line(color = "midnightblue", alpha = 0.7) +
  scale_x_continuous(
    name = "Predicted Win Probability (Bin)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    name = "Actual Win Rate (Bin)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    title = "Calibration Plot for Win Probability Model",
    subtitle = "Comparing predicted probabilities to actual win rates across 10 bins (sampled data)",
    size = "Number of\nGame States"
  ) +
  theme_minimal()

# Win probability plot for single match ---------------------------------------

match_to_plot_id <- "20231113110"

# Prepare the dense timeline for the match, including the inverse time variable
single_match_dense <- full_data |>
  filter(matchId == match_to_plot_id) |>
  complete(minutes_remaining = seq(80, 0, by = -0.1)) |>
  arrange(-minutes_remaining) |>
  fill(points_differential, .direction = "down") |>
  mutate(
    points_differential = if_else(
      is.na(points_differential),
      0,
      points_differential
    ),
    game_minute = 80 - minutes_remaining,
    inverse_minutes_remaining = 1 / (minutes_remaining + 0.1)
  ) |>
  add_epred_draws(fit_brms)

# Scoring events for annotations (use the sparse data)
scoring_events <- full_data |>
  filter(
    matchId == match_to_plot_id,
    points_differential != lag(points_differential, default = 0)
  ) |>
  mutate(game_minute = 80 - minutes_remaining)

# --- 1. Filter to a Manageable Number of Draws for Plotting ---
# Plotting all 4000 draws would be too messy. 100-200 is usually a good number.
plot_draws <- single_match_dense |>
  filter(.draw %in% 1:100)

# --- 2. Create the Spaghetti Plot ---
ggplot(plot_draws, aes(x = game_minute, y = .epred)) +
  # Draw a faint line for each posterior draw (.draw)
  geom_line(aes(group = .draw), alpha = 0.1, color = "grey") +

  # Overlay the posterior median line in a bold color
  # We use the full `dense_predictions` here to get the most accurate median
  stat_summary(
    data = single_match_dense,
    fun = median,
    geom = "line",
    color = "black",
    linewidth = 0.25
  ) +

  # Add the scoring event markers
  geom_point(
    data = scoring_events,
    aes(y = 0.5),
    color = "red",
    size = 3,
    shape = 18
  ) +

  # Aesthetics and labels
  scale_x_continuous(
    name = "Game Minute",
    breaks = seq(0, 80, by = 20),
    limits = c(0, 80)
  ) +
  scale_y_continuous(
    name = "Home Team Win Probability",
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray30") +
  labs(
    title = "Win Probability Chart (with 100 Posterior Draws)",
    subtitle = paste("Match ID:", match_to_plot_id),
    caption = "Faint grey lines are individual posterior draws; black line is the posterior median."
  ) +
  theme_minimal()

ggsave("2023 GF.png", bg = "white")
