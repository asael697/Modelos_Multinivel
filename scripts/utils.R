#' compute loo from a stan model
#' 
#' @param stan_file_path a string with the Stan file location
#' @param data_list a list with Stan's data to run the model
#' 
#' @return a loo object with the model's PISI-LOO values
#' 
compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 0)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

#' Compare loo values of two  Stan models
#' 
#' @param stan_file_path1 a string with the first Stan file location.
#' @param stan_file_path2 a string with the second Stan file locatio.
#' @param data_list1 a list with Stan's data to run the first model.
#' @param data_list2 a list with Stan's data to run the second model.
#' 
#' @return a loo object with the model's PISI-LOO values
#' 
compare_loos <- function(stan_file_path1 = NULL, stan_file_path2 = NULL, 
                        data_list1 = NULL, data_list2 = NULL){
  
  if(is.null(stan_file_path2))
    stan_file_path2 = stan_file_path1
  
  loo1 = compute_loo(stan_file_path = stan_file_path1, data_list = data_list1)
  loo2 = compute_loo(stan_file_path = stan_file_path2, data_list = data_list2)
  
  comp = loo_compare(loo1, loo2)
  return(comp)
}