data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
}
parameters {
  real mu;
  vector[J] mu_group; // Group-level means
  real<lower=0> sigma; // Individual-level standard deviation
}
model {
  mu ~ normal(0,10);
  // Prior for group-level parameters
  mu_group ~ normal(mu, 10);
  
  // Prior for individual-level parameters
  sigma ~ student_t(5, 0, 10);
  
  // Likelihood
  y ~ normal(mu_group[group], sigma);
}
generated quantities{
  vector[n] y_rep; 
  vector[n] log_lik;
  
  for(i in 1:n){
    y_rep[i] = normal_rng(mu_group[group[i]], sigma);
    log_lik[i] = normal_lpdf(y[i] | mu_group[group[i]], sigma);
  }
}
