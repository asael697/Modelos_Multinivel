data {
  int<lower=0> n;
  vector[n] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> alpha;
}
model {
  // priors
  mu ~ normal(0, 10);
  sigma ~ student_t(3, 0, 1);
  alpha ~ normal(0, 1);
  
  //likelihood
  y ~ skew_normal(mu, sigma, alpha);
}
generated quantities{
  vector[n] y_rep; 
  vector[n] log_lik;
  
  for(i in 1:n){
    y_rep[i] = skew_normal_rng(mu, sigma, alpha);
    log_lik[i] = skew_normal_lpdf(y[i] | mu, sigma, alpha);
  }
}
