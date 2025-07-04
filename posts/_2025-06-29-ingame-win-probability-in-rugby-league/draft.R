library(tidyverse)
library(arrow)
library(brms)

# 3D rendering of probability surface (two coordinate map with elevations representing probs)
# Probably don't need non-scoring observations

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
  filter(gameSeconds <= 4800, draw_at_fulltime != 1, !is.na(home_team_win))

# Modeling -------------------------------------------------------------------------

# 't2()' is the brms equivalent for a tensor product smooth.
brms_formula <- bf(
  home_team_win ~ t2(points_differential, minutes_remaining, k = 10),
  family = bernoulli(link = "logit")
)

# These are weakly regularising priors
# student_t(3, 0, 2.5) is a robust and commonly recommended default.
priors <- c(
  # Prior on the Intercept. Centered at 0 log-odds (50% prob).
  prior(student_t(3, 0, 2.5), class = "Intercept"),

  # Prior on the standard deviation of the smooth term ('sds').
  # This controls the overall "wiggliness" of the surface.
  prior(student_t(3, 0, 2.5), class = "sds")
)


print("Starting Bayesian GAM fit with brms. This will take several hours.")

wp_brms_model <- brm(
  formula = brms_formula,
  data = data,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  cores = 4,
  iter = 2000,
  seed = 2534,
  backend = "rstan",
  # control = list(adapt_delta = 0.99),
  file = "nrl_wp_brms_model"
)

print("Bayesian model fitting complete.")


# Calibration plot -------------------------------------------------------------

calibration_data <- data |>
  mutate(
    predicted_wp = predict(
      nrl_wp_model,
      newdata = pick(everything()),
      type = "response"
    ),
    home_team_win_numeric = as.numeric(as.character(home_team_win))
  ) |>
  mutate(
    wp_bin = cut(
      predicted_wp,
      breaks = seq(0, 1, by = 0.05),
      include.lowest = TRUE
    )
  ) |>
  summarise(
    n_plays = n(),
    actual_win_rate = mean(home_team_win_numeric),
    predicted_win_rate = mean(predicted_wp),
    .by = wp_bin
  ) |>
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

# Win probability plot for single match ---------------------------------------

match_to_plot <- "20141112550"

single_game_data <- data |>
  filter(matchId == match_to_plot) |>
  complete(gameSeconds = 0:4800, home_points = 0) |>
  fill(c(-gameSeconds, -title), .direction = "down") |>
  arrange(matchId, gameSeconds) |>
  mutate(
    points_differential = cumsum(home_points),
    game_minutes = gameSeconds / 60,
    minutes_remaining = 80 - game_minutes,
    predicted_wp = predict(
      nrl_wp_model,
      newdata = pick(everything()),
      type = "response"
    )
  )


label_events <- c(
  "Try",
  "Conversion-Made",
  "Penalty Shot-Made",
  "1 Point Field Goal-Made",
  "2 Point Field Goal-Made"
)

ggplot(single_game_data, aes(x = game_minutes, y = predicted_wp)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_point(
    data = single_game_data |>
      filter(
        title %in% label_events
      ),
    color = "red",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = single_game_data |>
      filter(
        title %in% label_events
      ),
    aes(label = points_differential)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 80)) +
  labs(
    title = paste("Win Probability Chart for Match:", match_to_plot),
    subtitle = "Home Team (Team_A) vs. Away Team (Team_B)",
    x = "Game Minutes",
    y = "Home Team Win Probability"
  ) +
  theme_minimal()
