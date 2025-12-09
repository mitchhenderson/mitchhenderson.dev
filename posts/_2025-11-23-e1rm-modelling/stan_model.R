library(brms)

# Data preparation =========================================================

model_data <- sim_data |>
  rename(
    # brms doesn't like underscores or . in variable names
    athlete = athlete_id,
    orm = true_1rm,
    weight = weight_observed
  )

# Train/test split: hold out last observation per athlete
holdout <- model_data |>
  group_by(athlete) |>
  slice_max(session_id, n = 1) |>
  ungroup()

train <- model_data |>
  anti_join(holdout, by = c("athlete", "session_id"))

# Model specification ======================================================

epley_formula <- bf(
  weight ~ orm / (1 + reps / exp(logk)),
  orm ~ 1 + (1 | athlete),
  logk ~ 1 + (1 | athlete),
  nl = TRUE
)

epley_priors <- c(
  prior(normal(140, 30), nlpar = "orm", coef = "Intercept"),
  prior(normal(3.4, 0.3), nlpar = "logk", coef = "Intercept"), # log(30) ≈ 3.4
  prior(exponential(0.05), class = "sd", nlpar = "orm"),
  prior(exponential(2), class = "sd", nlpar = "logk"),
  prior(exponential(0.2), class = "sigma")
)

# Prior predictive check ===================================================

epley_prior_check <- brm(
  formula = epley_formula,
  data = train,
  prior = epley_priors,
  family = gaussian(),
  sample_prior = "only",
  cores = 4,
  chains = 2,
  iter = 1000,
  seed = 2534
)

pp_check(epley_prior_check, ndraws = 100)

# Model fitting ============================================================

epley_fit <- brm(
  formula = epley_formula,
  data = train,
  prior = epley_priors,
  family = gaussian(),
  cores = 4,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 2534,
  file = "epley_fit"
)

summary(epley_fit)
pp_check(epley_fit, ndraws = 100)
plot(epley_fit)

# Prediction function ======================================================

estimate_1rm <- function(model, athlete_id, weight, reps) {
  model |>
    spread_draws(b_logk_Intercept, r_athlete__logk[athlete, ]) |>
    filter(athlete == athlete_id) |>
    mutate(
      k = exp(b_logk_Intercept + r_athlete__logk),
      orm_estimate = weight * (1 + reps / k)
    ) |>
    select(.draw, athlete, k, orm_estimate)
}

# Holdout evaluation =======================================================

holdout_predictions <- holdout |>
  rowwise() |>
  mutate(
    bayes_pred = estimate_1rm(epley_fit, athlete, weight, reps) |>
      summarise(pred = mean(orm_estimate)) |>
      pull(pred),
    epley_pred = weight * (1 + reps / 30)
  ) |>
  ungroup() |>
  left_join(
    athletes |> select(athlete = athlete_id, true_1rm),
    by = "athlete"
  ) |>
  mutate(
    bayes_error = bayes_pred - true_1rm,
    epley_error = epley_pred - true_1rm
  )

holdout_predictions |>
  summarise(
    bayes_rmse = sqrt(mean(bayes_error^2)),
    epley_rmse = sqrt(mean(epley_error^2)),
    bayes_mae = mean(abs(bayes_error)),
    epley_mae = mean(abs(epley_error))
  )

# Plot 1: Learned endurance profiles =======================================

athlete_k_summary <- epley_fit |>
  spread_draws(b_logk_Intercept, r_athlete__logk[athlete, ]) |>
  mutate(k = exp(b_logk_Intercept + r_athlete__logk)) |>
  group_by(athlete) |>
  summarise(mean_k = mean(k)) |>
  arrange(mean_k)

selected_athletes <- athlete_k_summary |>
  slice(c(1, 15, 30)) |>
  pull(athlete)

athlete_labels <- athlete_k_summary |>
  filter(athlete %in% selected_athletes) |>
  mutate(label = paste0("Athlete ", athlete, " (k = ", round(mean_k, 0), ")"))

endurance_profiles <- epley_fit |>
  spread_draws(
    b_orm_Intercept,
    b_logk_Intercept,
    r_athlete__orm[athlete, ],
    r_athlete__logk[athlete, ]
  ) |>
  filter(athlete %in% selected_athletes) |>
  mutate(
    orm = b_orm_Intercept + r_athlete__orm,
    k = exp(b_logk_Intercept + r_athlete__logk),
  ) |>
  crossing(reps = 1:15) |>
  mutate(pct_1rm = 100 / (1 + reps / k)) |>
  group_by(athlete, reps) |>
  median_qi(pct_1rm, .width = 0.8) |>
  left_join(athlete_labels, by = "athlete")

ggplot(
  endurance_profiles,
  aes(x = reps, y = pct_1rm, colour = label, fill = label)
) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  labs(
    x = "Repetitions",
    y = "% of 1RM",
    title = "Learned endurance profiles",
    subtitle = "Athletes with lower k fatigue faster at higher reps",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Effect of historical data on certainty ===========================

n_obs_lookup <- train |>
  count(athlete, name = "n_obs")

holdout_subset <- holdout |>
  filter(athlete %in% c(2, 21)) |>
  left_join(
    athletes |> select(athlete = athlete_id, true_1rm),
    by = "athlete"
  ) |>
  left_join(n_obs_lookup, by = "athlete") |>
  mutate(
    epley_estimate = weight * (1 + reps / 30),
    label = paste0(
      "Athlete ",
      athlete,
      " (",
      n_obs,
      " obs)\n",
      reps,
      " reps @ ",
      round(weight, 0),
      "kg"
    )
  )

plot2_data <- holdout_subset |>
  pmap_dfr(\(
    athlete,
    reps,
    weight,
    true_1rm,
    n_obs,
    epley_estimate,
    label,
    ...
  ) {
    estimate_1rm(epley_fit, athlete, weight, reps) |>
      mutate(
        true_1rm = true_1rm,
        epley_estimate = epley_estimate,
        label = label,
        n_obs = n_obs
      )
  })

ggplot(plot2_data, aes(x = orm_estimate, y = reorder(label, n_obs))) +
  stat_halfeye(.width = c(0.8, 0.95)) +
  geom_point(
    aes(x = true_1rm),
    shape = 4,
    size = 4,
    stroke = 2,
    colour = "red"
  ) +
  geom_point(
    aes(x = epley_estimate),
    shape = 1,
    size = 4,
    stroke = 2,
    colour = "blue"
  ) +
  labs(
    x = "Estimated 1RM (kg)",
    y = NULL,
    title = "Effect of historical data on prediction certainty",
    subtitle = "Each athlete's actual holdout observation",
    caption = "Red X = true 1RM | Blue circle = standard Epley estimate"
  ) +
  theme_minimal()

# Plot 3: Effect of rep count on certainty =================================

athlete2_obs <- train |>
  filter(athlete == 2) |>
  mutate(
    rep_bin = case_when(
      reps <= 5 ~ "low",
      reps <= 10 ~ "mid",
      TRUE ~ "high"
    )
  ) |>
  group_by(rep_bin) |>
  slice_sample(n = 1) |>
  ungroup()

plot3_data <- athlete2_obs |>
  pmap_dfr(\(reps, weight, ...) {
    estimate_1rm(epley_fit, athlete_id = 2, weight = weight, reps = reps) |>
      mutate(scenario = paste0(reps, " reps @ ", round(weight, 0), "kg"))
  }) |>
  mutate(scenario = fct_reorder(scenario, -parse_number(scenario)))

ggplot(plot3_data, aes(x = orm_estimate, y = scenario)) +
  stat_halfeye(.width = c(0.8, 0.95)) +
  labs(
    x = "Estimated 1RM (kg)",
    y = NULL,
    title = "Effect of rep count on prediction certainty",
    subtitle = "Athlete 2: actual training observations from different rep ranges"
  ) +
  theme_minimal()
