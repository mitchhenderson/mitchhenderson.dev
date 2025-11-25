# One Repetition Maximum (1RM) Estimation Formulas
# Tidyverse implementation
# Each formula estimates 1RM based on weight lifted and repetitions completed

library(tidyverse)

# Brzycki Formula (1993)
# One of the most popular and widely validated formulas
# Formula: 1RM = weight × (36 / (37 - reps))
e1rm_brzycki <- function(weight, reps) {
  weight * (36 / (37 - reps))
}

# Epley Formula (1985)
# Very popular, especially for moderate rep ranges
# Formula: 1RM = weight × (1 + 0.0333 × reps)
e1rm_epley <- function(weight, reps) {
  weight * (1 + 0.0333 * reps)
}

# Lombardi Formula (1989)
# Uses a non-linear exponential approach
# Formula: 1RM = weight × reps^0.1
e1rm_lombardi <- function(weight, reps) {
  weight * (reps^0.1)
}

# Mayhew et al. Formula (1992)
# Based on exponential regression, works well for moderate reps
# Formula: 1RM = (100 × weight) / (52.2 + 41.9 × e^(-0.055 × reps))
e1rm_mayhew <- function(weight, reps) {
  (100 * weight) / (52.2 + 41.9 * exp(-0.055 * reps))
}

# O'Conner et al. Formula (1989)
# Similar to Epley but assumes ~2.5% per additional rep
# Formula: 1RM = weight × (1 + 0.025 × reps)
e1rm_oconner <- function(weight, reps) {
  weight * (1 + 0.025 * reps)
}

# Wathan Formula (1994)
# Exponential formula that performs well across rep ranges
# Formula: 1RM = (100 × weight) / (48.8 + 53.8 × e^(-0.075 × reps))
e1rm_wathan <- function(weight, reps) {
  (100 * weight) / (48.8 + 53.8 * exp(-0.075 * reps))
}

# Lander Formula (1985)
# Linear formula, works well for low to moderate reps
# Formula: 1RM = (100 × weight) / (101.3 - 2.67123 × reps)
e1rm_lander <- function(weight, reps) {
  (100 * weight) / (101.3 - 2.67123 * reps)
}

# Example usage and comparison
# Create a dataset with different weight/rep combinations
example_data <- tibble(
  weight = c(100, 100, 100, 100, 100, 225, 225, 225),
  reps = c(1, 3, 5, 8, 10, 3, 5, 8)
)

# Apply all formulas to the dataset
results <- example_data %>%
  mutate(
    brzycki = e1rm_brzycki(weight, reps),
    epley = e1rm_epley(weight, reps),
    lombardi = e1rm_lombardi(weight, reps),
    mayhew = e1rm_mayhew(weight, reps),
    oconner = e1rm_oconner(weight, reps),
    wathan = e1rm_wathan(weight, reps),
    lander = e1rm_lander(weight, reps)
  )

# View the results
print(results)

# Calculate the mean and standard deviation across formulas for each row
results_summary <- results %>%
  rowwise() %>%
  mutate(
    mean_e1rm = mean(c(
      brzycki,
      epley,
      lombardi,
      mayhew,
      oconner,
      wathan,
      lander
    )),
    sd_e1rm = sd(c(brzycki, epley, lombardi, mayhew, oconner, wathan, lander)),
    cv_e1rm = (sd_e1rm / mean_e1rm) * 100 # Coefficient of variation
  ) %>%
  ungroup()

print(results_summary)

# Create a visualization comparing formulas across different rep ranges
comparison_data <- tibble(
  reps = rep(1:12, 7),
  weight = 100
) %>%
  mutate(
    formula = rep(
      c(
        "Brzycki",
        "Epley",
        "Lombardi",
        "Mayhew",
        "O'Conner",
        "Wathan",
        "Lander"
      ),
      each = 12
    ),
    e1rm = case_when(
      formula == "Brzycki" ~ e1rm_brzycki(weight, reps),
      formula == "Epley" ~ e1rm_epley(weight, reps),
      formula == "Lombardi" ~ e1rm_lombardi(weight, reps),
      formula == "Mayhew" ~ e1rm_mayhew(weight, reps),
      formula == "O'Conner" ~ e1rm_oconner(weight, reps),
      formula == "Wathan" ~ e1rm_wathan(weight, reps),
      formula == "Lander" ~ e1rm_lander(weight, reps)
    )
  )

# Plot the comparison
ggplot(comparison_data, aes(x = reps, y = e1rm, color = formula)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Comparison of 1RM Estimation Formulas",
    subtitle = "Based on 100 lbs lifted for varying repetitions",
    x = "Number of Repetitions",
    y = "Estimated 1RM (lbs)",
    color = "Formula"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  ) +
  scale_x_continuous(breaks = 1:12)

# Calculate and visualize the spread between formulas
spread_data <- comparison_data %>%
  group_by(reps) %>%
  summarise(
    min_e1rm = min(e1rm),
    max_e1rm = max(e1rm),
    spread = max_e1rm - min_e1rm,
    mean_e1rm = mean(e1rm)
  )

ggplot(spread_data, aes(x = reps)) +
  geom_ribbon(
    aes(ymin = min_e1rm, ymax = max_e1rm),
    alpha = 0.3,
    fill = "blue"
  ) +
  geom_line(aes(y = mean_e1rm), color = "darkblue", linewidth = 1) +
  geom_point(aes(y = mean_e1rm), color = "darkblue", size = 3) +
  labs(
    title = "Spread of 1RM Estimates Across All Formulas",
    subtitle = "Shaded area shows min-max range; line shows mean",
    x = "Number of Repetitions",
    y = "Estimated 1RM (lbs)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_x_continuous(breaks = 1:12)
