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

# ---- Layouts ----

now_y <- last(history$y)

# The wide layout is the drawing as computed above. The compact one, for
# phones, is the same draws around a gentler trend, scaled down about "now",
# so the whole fan fits in the strip beside the small portrait.
compact_scale <- 0.36
compact_rise <- 200
compact_width <- 520
compact_height <- 200
compact_now_x <- 280
compact_now_y <- 150

layouts <- list(
  wide = list(
    class = "hero-fan hero-fan-wide",
    width = view_width,
    height = view_height,
    place_x = \(x) x,
    place_y = \(x, y) y
  ),
  compact = list(
    class = "hero-fan hero-fan-compact",
    width = compact_width,
    height = compact_height,
    place_x = \(x) compact_now_x + compact_scale * (x - now_x),
    place_y = \(x, y) {
      trend_removed <- (total_rise - compact_rise) *
        ((x / view_width)^4 - (now_x / view_width)^4)
      compact_now_y + compact_scale * (y + trend_removed - now_y)
    }
  )
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

draw_timings <- futures_shown |>
  distinct(draw) |>
  mutate(
    delay = round(futures_start + runif(n(), 0, 420)),
    duration = round(runif(n(), 900, 1400))
  )

observation_timings <- observations |>
  filter(x < now_x) |>
  mutate(
    delay = round(
      history_delay + history_duration * (x - min(x)) / (now_x - min(x))
    )
  )

build_fan_svg <- function(layout) {
  place_x <- layout$place_x
  place_y <- layout$place_y

  draw_elements <- futures_shown |>
    summarise(d = build_line_path(place_x(x), place_y(x, y)), .by = draw) |>
    left_join(draw_timings, by = join_by(draw)) |>
    mutate(
      element = str_glue(
        '<path class="hero-fan-stroke hero-fan-draw" pathLength="1" ',
        'style="--d:{delay}ms;--t:{duration}ms" d="{d}"/>'
      )
    )

  observation_elements <- observation_timings |>
    mutate(
      element = str_glue(
        '<circle class="hero-fan-point" style="--d:{delay}ms" ',
        'cx="{round(place_x(x), 1)}" cy="{round(place_y(x, y), 1)}" r="2.5"/>'
      )
    )

  placed_bands <- bands |>
    mutate(across(-x, \(y) place_y(x, y)), x = place_x(x))

  band_90_d <- build_band_path(
    placed_bands$x,
    placed_bands$lower_90,
    placed_bands$upper_90
  )
  band_50_d <- build_band_path(
    placed_bands$x,
    placed_bands$lower_50,
    placed_bands$upper_50
  )
  median_d <- build_line_path(placed_bands$x, placed_bands$median)
  history_d <- build_line_path(
    place_x(history$x),
    place_y(history$x, history$y)
  )

  c(
    str_glue(
      '<svg class="{layout$class}" viewBox="0 0 {layout$width} ',
      '{layout$height}" preserveAspectRatio="xMidYMax slice" ',
      'aria-hidden="true" focusable="false">'
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
      'style="--d:{history_delay}ms;--t:{history_duration}ms" ',
      'd="{history_d}"/>'
    ),
    observation_elements$element,
    str_glue(
      '<circle class="hero-fan-now" style="--d:{futures_start}ms" ',
      'cx="{round(place_x(now_x), 1)}" ',
      'cy="{round(place_y(now_x, now_y), 1)}" r="4"/>'
    ),
    "</svg>"
  )
}

svg <- layouts |>
  map(build_fan_svg) |>
  list_c()

write_lines(svg, out_path)

right_edge <- slice_tail(bands, n = 1)
compact_y <- layouts$compact$place_y(futures_shown$x, futures_shown$y)

message(str_glue(
  "Fan at the right edge, 5% / 50% / 95%: ",
  "{round(right_edge$lower_90)} / {round(right_edge$median)} / ",
  "{round(right_edge$upper_90)}\n",
  "Lowest point of any shown draw: {round(max(futures_shown$y))}\n",
  "Compact fan, highest / lowest point of any shown draw: ",
  "{round(min(compact_y))} / {round(max(compact_y))} ",
  "(\"now\" at {compact_now_y} of {compact_height})\n",
  "Wrote {out_path} ({file.size(out_path)} bytes)"
))
