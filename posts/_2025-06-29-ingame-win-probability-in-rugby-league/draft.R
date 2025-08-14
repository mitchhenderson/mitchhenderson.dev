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
    points_differential = cumsum(home_points),
    .by = matchId
  ) |>
  filter(gameSeconds <= 4800, draw_at_fulltime != 1, !is.na(home_team_win)) |>
  # Work with only non-scoring observations for now and can then think about adding complexity later
  select(
    matchId,
    points_differential,
    minutes_remaining,
    home_team_win,
    type,
    points
  )

data_only_scoring <- data |>
  filter(type == "GameTime" | points != 0) |>
  mutate(
    inverse_minutes_remaining = 1 / (minutes_remaining + 0.1)
  ) |>
  select(-type, -points)

# Modeling -------------------------------------------------------------------------

fit_brms <- readRDS(
  "posts/_2025-06-29-ingame-win-probability-in-rugby-league/nrl_wp_brms_model.rds"
)

# priors <- c(
#   prior(normal(0, 1), class = "Intercept"),
#   prior(normal(0, 1), class = "b")
# )

# tictoc::tic()
# fit_brms <- brm(
#   formula = home_team_win ~ 1 + points_differential * inverse_minutes_remaining,
#   data = data_only_scoring,
#   family = bernoulli(link = "logit"),
#   prior = priors,
#   chains = 4,
#   cores = 4,
#   iter = 2000,
#   warmup = 1000,
#   refresh = 500,
#   seed = 123,
#   save_pars = save_pars(all = TRUE),
#   backend = "rstan",
#   file = "nrl_wp_brms_model"
# )
# tictoc::toc()

summary(fit_brms)

plot(fit_brms, nvariables = 4, ask = FALSE)

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
single_match_dense <- data_only_scoring |>
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
scoring_events <- data_only_scoring |>
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
