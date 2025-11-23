data {
  int<lower=1> N;              // Total number of observations (sets)
  int<lower=1> J;              // Number of athletes
  array[N] int<lower=1> athlete;     // Athlete index for each observation
  vector[N] reps;              // Predictor: Repetitions performed
  vector[N] weight;            // Outcome: Weight lifted
}

parameters {
  // --- Hyperparameters (Squad Level) ---
  // These define the distribution of the whole squad
  real<lower=0> mu_pop_1rm;    // Average 1RM of the squad
  real<lower=0> sigma_pop_1rm; // Variation in 1RM across the squad

  real<lower=0> mu_pop_beta;   // Average Endurance (Beta) of the squad
  real<lower=0> sigma_pop_beta;// Variation in Endurance across the squad

  real<lower=0> sigma_noise;   // Measurement error (how consistent are the lifters?)

  // --- Subject Parameters (Athlete Level) ---
  // We constrain these to be positive because negative strength/endurance is impossible
  vector<lower=0>[J] one_rm;
  vector<lower=0>[J] beta;
}

model {
  // --- Priors ---
  // Weakly informative priors based on human physiology
  mu_pop_1rm ~ normal(100, 50);
  sigma_pop_1rm ~ normal(0, 20);

  mu_pop_beta ~ normal(30, 10); // Centered around the standard Epley constant
  sigma_pop_beta ~ normal(0, 10);

  sigma_noise ~ normal(0, 10);

  // --- Hierarchical Structure ---
  // Each athlete's parameters are drawn from the squad distribution
  one_rm ~ normal(mu_pop_1rm, sigma_pop_1rm);
  beta ~ normal(mu_pop_beta, sigma_pop_beta);

  // --- Likelihood ---
  for (i in 1:N) {
    // The Modified Epley Formula: W = 1RM / (1 + R/Beta)
    real expected_weight = one_rm[athlete[i]] / (1 + reps[i] / beta[athlete[i]]);

    // The observation
    weight[i] ~ normal(expected_weight, sigma_noise);
  }
}
