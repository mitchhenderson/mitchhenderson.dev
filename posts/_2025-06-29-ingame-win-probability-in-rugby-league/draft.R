library(tidyverse)
library(arrow)
#library(rstan)
library(brms)

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


avg_points_differential <- mean(data_only_scoring$points_differential)
sd_points_differential <- sd(data_only_scoring$points_differential)

avg_inv_minutes_remaining <- mean(data_only_scoring$inverse_minutes_remaining)
sd_inv_minutes_remaining <- sd(data_only_scoring$inverse_minutes_remaining)

dev_data <- data_only_scoring |>
    mutate(
        std_points_differential = (points_differential -
            avg_points_differential) /
            sd_points_differential,
        std_inv_minutes_remaining = (inverse_minutes_remaining -
            avg_inv_minutes_remaining) /
            sd_inv_minutes_remaining
    )

# Create the list of data required by the Stan model
# stan_data_list <- list(
#     N = nrow(dev_data),
#     home_team_win = dev_data$home_team_win,
#     pd_std = dev_data$pd_std,
#     inv_mr_std = dev_data$inv_mr_std
# )

# Modeling -------------------------------------------------------------------------

fit_brms_default_priors <- readRDS(
    "posts/_2025-06-29-ingame-win-probability-in-rugby-league/nrl_wp_brms_model.rds"
)

tictoc::tic()
fit_brms_default_priors <- brm(
    # * includes main effects + interaction
    home_team_win ~ std_points_differential * std_inv_minutes_remaining,
    family = bernoulli(),
    data = dev_data,
    seed = 2534,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 500,
    cores = 4,
    backend = "rstan",
    file = "nrl_wp_brms_model"
)
tictoc::toc()

summary(fit_brms_default_priors)

# Plot results
plot(fit_brms_default_priors)

# Model diagnostics
pp_check(fit_brms_default_priors)


#------------------------------------------------------

# =============================================================================
# CALIBRATION PLOT using probably package (tidymodels ecosystem)
# =============================================================================

library(probably)
library(tidybayes)

predictions_data <- dev_data |>
    add_epred_draws(fit_brms_default_priors) |>
    median_qi(.epred) |>
    rename(predicted_prob = .epred)

cal_data <- predictions_data |>
    mutate(
        .pred_yes = predicted_prob,
        .pred_no = 1 - predicted_prob,
        truth = factor(
            ifelse(home_team_win == 1, "yes", "no"),
            levels = c("no", "yes")
        )
    )

calibration_plot <- cal_plot_breaks(
    cal_data,
    truth = truth,
    estimate = .pred_yes,
    num_breaks = 10,
    event_level = "second"
) +
    labs(
        title = "Model Calibration Plot",
        subtitle = "How well predicted probabilities match observed outcomes"
    )

match_to_plot_id <- "20141110110" # Change as needed


# 1. Set up for parallel execution (optional but highly recommended)
# This tells rstan to use as many cores as are available on your machine.
options(mc.cores = parallel::detectCores())

# The `stan()` function compiles the model (if it hasn't been compiled in this session)
# and then runs the sampler.
tictoc::tic()
fit <- stan(
    file = "posts/_2025-06-29-ingame-win-probability-in-rugby-league/win_prob_v3.stan", # Path to your Stan model file
    data = stan_data_list, # Your data list
    seed = 123,
    chains = 4, # Number of Markov chains
    iter = 2000, # Total number of iterations per chain
    warmup = 1000, # Number of warmup (burn-in) iterations
    refresh = 500 # Show progress every x iterations
)
tictoc::toc()

saveRDS(fit, file = "win_prob_v3.rds")

print(fit)


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
