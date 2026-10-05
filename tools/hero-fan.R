# Generate the forecast fan behind the Home hero.
#
# Run from the project root after changing anything here, then render the site:
#   Rscript tools/hero-fan.R
# It writes _includes/hero-fan.svg, which index.qmd includes.
# tools/hero-fan.py is the same model in Python, for readers who prefer it.

# A Gaussian process is conditioned on nine simulated observations on the left
# of the band. Its posterior is tight where there is data and widens where there
# is none, so draws from it run together along the history and fan out after the
# last observation ("now"). Coordinates are pixels in a 1440 x 870 band, with y
# measured downwards as in SVG.

library(tidyverse)

set.seed(2534)

out_path <- "_includes/hero-fan.svg"

view_width <- 1440
view_height <- 870

# Layout: the history runs under the copy, in the band's bottom padding
now_x <- 800
baseline_y <- 852
# The trend is slow through the history and accelerates afterwards
total_rise <- 320

# Gaussian process: squared-exponential kernel
gp_sd <- 85
gp_lengthscale <- 260
obs_noise_sd <- 5

n_draws_shown <- 36
n_draws_bands <- 4000

compute_trend <- function(x) {
  total_rise * (x / view_width)^4
}

compute_kernel <- function(a, b) {
  gp_sd^2 * exp(-outer(a, b, "-")^2 / (2 * gp_lengthscale^2))
}

# Screen y for a residual around the trend (y is measured downwards)
place_y <- function(x, residual) {
  baseline_y - compute_trend(x) + residual
}

# ---- Observations: a gentle wobble around the trend, with noise ----

observations <- tibble(x = seq(40, now_x, length.out = 9)) |>
  mutate(
    residual = 5 * sin(x / 140 + 1) + rnorm(n(), sd = obs_noise_sd),
    y = place_y(x, residual)
  )

# ---- Posterior of the process given the observations ----

k_obs <- compute_kernel(observations$x, observations$x) +
  diag(obs_noise_sd^2, nrow(observations))
k_obs_inverse <- solve(k_obs)

compute_posterior <- function(grid_x) {
  k_cross <- compute_kernel(grid_x, observations$x)
  list(
    mean = as.vector(k_cross %*% k_obs_inverse %*% observations$residual),
    cov = compute_kernel(grid_x, grid_x) -
      k_cross %*% k_obs_inverse %*% t(k_cross)
  )
}

# History: the posterior mean through the observed stretch
history <- tibble(x = seq(min(observations$x), now_x, by = 20)) |>
  mutate(y = place_y(x, compute_posterior(x)$mean))

# Futures: draws from the posterior after the last observation
future_x <- seq(now_x, view_width + 20, by = 20)
future_posterior <- compute_posterior(future_x)
cholesky <- chol(future_posterior$cov + diag(1e-6, length(future_x)))

# One row per draw per x. Every future is shifted to leave from the same
# point: the fitted value at "now".
sample_futures <- function(n_draws) {
  z <- matrix(rnorm(n_draws * length(future_x)), nrow = n_draws)
  residuals <- sweep(z %*% cholesky, 2, future_posterior$mean, "+")

  tibble(
    draw = rep(seq_len(n_draws), times = length(future_x)),
    x = rep(future_x, each = n_draws),
    residual = as.vector(residuals)
  ) |>
    mutate(
      residual = residual - (first(residual) - future_posterior$mean[1]),
      .by = draw
    ) |>
    mutate(y = place_y(x, residual))
}

futures_shown <- sample_futures(n_draws_shown)

bands <- sample_futures(n_draws_bands) |>
  summarise(
    lower_90 = quantile(y, 0.05),
    lower_50 = quantile(y, 0.25),
    median = quantile(y, 0.5),
    upper_50 = quantile(y, 0.75),
    upper_90 = quantile(y, 0.95),
    .by = x
  )

# ---- SVG ----

format_points <- function(x, y) {
  str_flatten(str_c(round(x, 1), ",", round(y, 1)), collapse = "L")
}

build_line_path <- function(x, y) {
  str_c("M", format_points(x, y))
}

build_band_path <- function(x, lower, upper) {
  str_c(
    "M",
    format_points(x, lower),
    "L",
    format_points(rev(x), rev(upper)),
    "Z"
  )
}

# Timings in milliseconds. The history draws first; the futures leave "now"
# as it arrives, each at its own moment and pace.
history_delay <- 100
history_duration <- 650
futures_start <- history_delay + history_duration - 50

draw_elements <- futures_shown |>
  summarise(d = build_line_path(x, y), .by = draw) |>
  mutate(
    delay = round(futures_start + runif(n(), 0, 420)),
    duration = round(runif(n(), 900, 1400)),
    element = str_glue(
      '<path class="hero-fan-stroke hero-fan-draw" pathLength="1" ',
      'style="--d:{delay}ms;--t:{duration}ms" d="{d}"/>'
    )
  )

observation_elements <- observations |>
  filter(x < now_x) |>
  mutate(
    delay = round(
      history_delay + history_duration * (x - min(x)) / (now_x - min(x))
    ),
    element = str_glue(
      '<circle class="hero-fan-point" style="--d:{delay}ms" ',
      'cx="{round(x, 1)}" cy="{round(y, 1)}" r="2.5"/>'
    )
  )

band_90_d <- build_band_path(bands$x, bands$lower_90, bands$upper_90)
band_50_d <- build_band_path(bands$x, bands$lower_50, bands$upper_50)
median_d <- build_line_path(bands$x, bands$median)
history_d <- build_line_path(history$x, history$y)
now_y <- last(history$y)

svg <- c(
  str_glue(
    '<svg class="hero-fan" viewBox="0 0 {view_width} {view_height}" ',
    'preserveAspectRatio="xMidYMax slice" aria-hidden="true" ',
    'focusable="false">'
  ),
  str_glue('<path class="hero-fan-band" d="{band_90_d}"/>'),
  str_glue('<path class="hero-fan-band" d="{band_50_d}"/>'),
  draw_elements$element,
  str_glue(
    '<path class="hero-fan-stroke hero-fan-median" pathLength="1" ',
    'style="--d:{futures_start}ms;--t:1100ms" d="{median_d}"/>'
  ),
  str_glue(
    '<path class="hero-fan-stroke hero-fan-history" pathLength="1" ',
    'style="--d:{history_delay}ms;--t:{history_duration}ms" d="{history_d}"/>'
  ),
  observation_elements$element,
  str_glue(
    '<circle class="hero-fan-now" style="--d:{futures_start}ms" ',
    'cx="{now_x}" cy="{round(now_y, 1)}" r="4"/>'
  ),
  "</svg>"
)

write_lines(svg, out_path)

right_edge <- slice_tail(bands, n = 1)

message(str_glue(
  "Fan at the right edge, 5% / 50% / 95%: ",
  "{round(right_edge$lower_90)} / {round(right_edge$median)} / ",
  "{round(right_edge$upper_90)}\n",
  "Lowest point of any shown draw: {round(max(futures_shown$y))}\n",
  "Wrote {out_path} ({file.size(out_path)} bytes)"
))
