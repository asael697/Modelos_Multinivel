functions {
  real generalized_gamma_lpdf(real x, real k, real mu, real sigma) {
    real w;
    real d;
    w = (log(x) - mu) / sigma;
    d = (k - .5) * log(k) - log(sigma) - lgamma(k) + (sqrt(k) * w - k * exp(1 / sqrt(k) * w)) - log(x);
    
    return d;
  }
  real generalized_gamma_rng(real k, real mu, real sigma) {
    real Q = 1.0 / sqrt(k);
    real gamma = gamma_rng(Q^-2, 1);
    real w = log(Q^2 * gamma) / Q;
    return exp(mu + sigma * w);
  }
}
data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
}
parameters {
  real mu;
  array[J] real mu_group;
  real<lower=0> k;
}
model {
  // priors
  mu ~ normal(0,10);
  // Prior for group-level parameters
  mu_group ~ normal(mu, 10);
  
  k~ normal(0, 1);
  // Prior for individual-level parameters
  
  //likelihood
  for(i in 1:n)
    target += generalized_gamma_lpdf(y[group[i]] | k, mu_group[group[i]], 1.0);
}
generated quantities{
  vector[n] y_rep;
  vector[n] log_lik;
  
 for(i in 1:n){
    y_rep[i] = generalized_gamma_rng(k,  mu_group[group[i]], 1.0);
    log_lik[i] = generalized_gamma_lpdf(y[group[i]] | k, mu_group[group[i]], 1.0);
  }
}
