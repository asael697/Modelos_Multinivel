data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
}
parameters {
  real mu;
  vector[J] mu_group; // Group-level means
  vector<lower=0>[J] alpha;
  real<lower=0> sigma; // Individual-level standard deviation
}
model {
  // priors
  mu ~ normal(0, 10);
  mu_group ~ normal(mu, 1);
  sigma ~ student_t(3, 0, 1);
  alpha ~ normal(0, 1);
  
  //likelihood
  y ~ skew_normal(mu_group[group], sigma, alpha[group]);
}
generated quantities{
  vector[n] y_rep; 
  vector[n] log_lik;
  
 for(i in 1:n){
    y_rep[i] = skew_normal_rng(mu_group[group[i]], sigma, alpha[group[i]]);
    log_lik[i] = skew_normal_lpdf(y[i] | mu_group[group[i]], sigma, alpha[group[i]]);
  }
}
