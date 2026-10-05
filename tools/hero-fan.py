# /// script
# requires-python = ">=3.11"
# dependencies = ["numpy>=2.0", "polars>=1.0"]
# ///
"""The forecast fan behind the Home hero, in Python.

The artwork on the site is made by tools/hero-fan.R. This is the same model
and layout, step for step, for readers who would rather read Python. NumPy and
R generate different random numbers, so the individual lines it draws differ
from the ones on the site, while the shape of the fan is the same.

It prints the SVG, so it never overwrites the site's artwork:

    uv run tools/hero-fan.py > fan.svg

A Gaussian process is conditioned on nine simulated observations on the left
of the band. Its posterior is tight where there is data and widens where there
is none, so draws from it run together along the history and fan out after the
last observation ("now"). Coordinates are pixels in a 1440 x 870 band, with y
measured downwards as in SVG.
"""

import sys

import numpy as np
import polars as pl

rng = np.random.default_rng(2534)

VIEW_WIDTH = 1440
VIEW_HEIGHT = 870

# Layout: the history runs under the copy, in the band's bottom padding
NOW_X = 800
BASELINE_Y = 852
# The trend is slow through the history and accelerates afterwards
TOTAL_RISE = 320

# Gaussian process: squared-exponential kernel
GP_SD = 85
GP_LENGTHSCALE = 260
OBS_NOISE_SD = 5

N_DRAWS_SHOWN = 36
N_DRAWS_BANDS = 4000


def compute_trend(x: np.ndarray) -> np.ndarray:
    return TOTAL_RISE * (x / VIEW_WIDTH) ** 4


def compute_kernel(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    distance = a[:, None] - b[None, :]
    return GP_SD**2 * np.exp(-(distance**2) / (2 * GP_LENGTHSCALE**2))


def place_y(x: np.ndarray, residual: np.ndarray) -> np.ndarray:
    """Screen y for a residual around the trend (y is measured downwards)."""
    return BASELINE_Y - compute_trend(x) + residual


# ---- Observations: a gentle wobble around the trend, with noise ----

obs_x = np.linspace(40, NOW_X, 9)
obs_residual = 5 * np.sin(obs_x / 140 + 1) + rng.normal(0, OBS_NOISE_SD, obs_x.size)
observations = pl.DataFrame({"x": obs_x, "y": place_y(obs_x, obs_residual)})

# ---- Posterior of the process given the observations ----

k_obs = compute_kernel(obs_x, obs_x) + OBS_NOISE_SD**2 * np.eye(obs_x.size)


def compute_posterior(grid_x: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Posterior mean and covariance of the residual at each x in the grid."""
    k_cross = compute_kernel(grid_x, obs_x)
    mean = k_cross @ np.linalg.solve(k_obs, obs_residual)
    cov = compute_kernel(grid_x, grid_x) - k_cross @ np.linalg.solve(k_obs, k_cross.T)
    return mean, cov


# History: the posterior mean through the observed stretch
history_x = np.arange(obs_x.min(), NOW_X + 1, 20)
history_mean, _ = compute_posterior(history_x)
history = pl.DataFrame({"x": history_x, "y": place_y(history_x, history_mean)})

# Futures: draws from the posterior after the last observation
future_x = np.arange(NOW_X, VIEW_WIDTH + 21, 20)
future_mean, future_cov = compute_posterior(future_x)
cholesky = np.linalg.cholesky(future_cov + 1e-6 * np.eye(future_x.size))


def sample_futures(n_draws: int) -> pl.DataFrame:
    """One row per draw per x.

    Every future is shifted to leave from the same point: the fitted value at
    "now".
    """
    z = rng.standard_normal((n_draws, future_x.size))
    residuals = future_mean + z @ cholesky.T
    residuals -= residuals[:, [0]] - future_mean[0]

    return pl.DataFrame(
        {
            "draw": np.repeat(np.arange(n_draws), future_x.size),
            "x": np.tile(future_x, n_draws),
            "y": place_y(future_x, residuals).ravel(),
        }
    )


futures_shown = sample_futures(N_DRAWS_SHOWN)

bands = (
    sample_futures(N_DRAWS_BANDS)
    .group_by("x", maintain_order=True)
    .agg(
        lower_90=pl.col("y").quantile(0.05, interpolation="linear"),
        lower_50=pl.col("y").quantile(0.25, interpolation="linear"),
        median=pl.col("y").quantile(0.5, interpolation="linear"),
        upper_50=pl.col("y").quantile(0.75, interpolation="linear"),
        upper_90=pl.col("y").quantile(0.95, interpolation="linear"),
    )
)

# ---- SVG ----


def format_points(x: str, y: str) -> pl.Expr:
    """The points of a path as "x,y" pairs joined by line-to commands."""
    return pl.format("{},{}", pl.col(x).round(1), pl.col(y).round(1)).str.join("L")


def build_line_path(frame: pl.DataFrame, y: str) -> str:
    return "M" + frame.select(format_points("x", y)).item()


def build_band_path(lower: str, upper: str) -> str:
    there = bands.select(format_points("x", lower)).item()
    back = bands.reverse().select(format_points("x", upper)).item()
    return f"M{there}L{back}Z"


# Timings in milliseconds. The history draws first; the futures leave "now"
# as it arrives, each at its own moment and pace.
HISTORY_DELAY = 100
HISTORY_DURATION = 650
FUTURES_START = HISTORY_DELAY + HISTORY_DURATION - 50

draw_elements = (
    futures_shown.group_by("draw", maintain_order=True)
    .agg(d=pl.lit("M") + format_points("x", "y"))
    .with_columns(
        delay=pl.Series(FUTURES_START + rng.uniform(0, 420, N_DRAWS_SHOWN)).round().cast(pl.Int32),
        duration=pl.Series(rng.uniform(900, 1400, N_DRAWS_SHOWN)).round().cast(pl.Int32),
    )
    .select(
        pl.format(
            '<path class="hero-fan-stroke hero-fan-draw" pathLength="1" '
            'style="--d:{}ms;--t:{}ms" d="{}"/>',
            "delay",
            "duration",
            "d",
        )
    )
    .to_series()
)

first_x = observations["x"].min()

observation_elements = (
    observations.filter(pl.col("x") < NOW_X)
    .with_columns(
        delay=(HISTORY_DELAY + HISTORY_DURATION * (pl.col("x") - first_x) / (NOW_X - first_x))
        .round()
        .cast(pl.Int32)
    )
    .select(
        pl.format(
            '<circle class="hero-fan-point" style="--d:{}ms" cx="{}" cy="{}" r="2.5"/>',
            "delay",
            pl.col("x").round(1),
            pl.col("y").round(1),
        )
    )
    .to_series()
)

now_y = history["y"].last()

svg = [
    (
        f'<svg class="hero-fan" viewBox="0 0 {VIEW_WIDTH} {VIEW_HEIGHT}" '
        'preserveAspectRatio="xMidYMax slice" aria-hidden="true" focusable="false">'
    ),
    f'<path class="hero-fan-band" d="{build_band_path("lower_90", "upper_90")}"/>',
    f'<path class="hero-fan-band" d="{build_band_path("lower_50", "upper_50")}"/>',
    *draw_elements,
    (
        '<path class="hero-fan-stroke hero-fan-median" pathLength="1" '
        f'style="--d:{FUTURES_START}ms;--t:1100ms" d="{build_line_path(bands, "median")}"/>'
    ),
    (
        '<path class="hero-fan-stroke hero-fan-history" pathLength="1" '
        f'style="--d:{HISTORY_DELAY}ms;--t:{HISTORY_DURATION}ms" '
        f'd="{build_line_path(history, "y")}"/>'
    ),
    *observation_elements,
    (
        f'<circle class="hero-fan-now" style="--d:{FUTURES_START}ms" '
        f'cx="{NOW_X}" cy="{now_y:.1f}" r="4"/>'
    ),
    "</svg>",
]

print("\n".join(svg))

right_edge = bands.tail(1).row(0, named=True)

print(
    "Fan at the right edge, 5% / 50% / 95%: "
    f"{right_edge['lower_90']:.0f} / {right_edge['median']:.0f} / {right_edge['upper_90']:.0f}\n"
    f"Lowest point of any shown draw: {futures_shown['y'].max():.0f}",
    file=sys.stderr,
)
