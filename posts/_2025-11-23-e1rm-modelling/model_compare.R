library(loo)

# Extract log-likelihood
log_lik <- extract_log_lik(fit, parameter_name = "log_lik")

# Compute LOO
loo_result <- loo(log_lik)

# Compare with other models
loo_compare(loo_result1, loo_result2, loo_result3)
