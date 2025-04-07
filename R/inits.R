
# warnings can occur like pasted below
# Chain XXX Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
# Chain XXX Exception: normal_lpdf: Location parameter is inf, but must be finite!
# ie mu is not finite..
# meaning inf values are drawn for sum of mustars, so product (mu_raw* mu_sigma)
# by setting inits, we avoid this for the initial values
# HOWEVER they can still occur early on in the chain (not sure if adding a cap is good thing to do)


init_fun <- function(chain_id, stan_data){
  set.seed(chain_id)
  inits <- list()
  if (stan_data$mu_raw_n_terms_estimate > 0){
    if (!stan_data[["mu_isvector"]]){
        inits <- c(inits,
                 list(mu_raw_estimate = rtruncnorm(stan_data$mu_raw_n_terms_estimate, mean = 0, sd = 1, a = -2, b = 2))
                 )
        if (stan_data$mu_n_sigma_estimate > 0){
          inits <- c(inits,
                     list(mu_sigma_estimate = rtruncnorm(stan_data$mu_n_sigma_estimate, mean = 0, sd = 0.5, a = stan_data$verysmallnumber, b = 1))
                     )
        }
    } else {
      inits <- c(inits,
                 list(mu_raw_estimate = matrix(
                   rtruncnorm(stan_data$mu_raw_n_terms_estimate*stan_data$mu_k_terms,  mean = 0, sd = 1, a = -2, b = 2),
                    nrow = stan_data$mu_raw_n_terms_estimate)
                    )
                 )
      if (stan_data$mu_n_sigma_estimate > 0){
        inits <- c(inits,
                   list(mu_sigma_estimate = matrix(
                     rtruncnorm(stan_data$mu_n_sigma_estimate*stan_data$mu_k_terms, mean = 0, sd = 0.5, a = stan_data$verysmallnumber, b = 1),
                     nrow = stan_data$mu_n_sigma_estimate)
                  ))
      }
    }
  }
  if (!stan_data$fix_nonse){
    inits <- c(inits,
               list(
                 nonse_estimate = rtruncnorm(1, mean = 0, sd = 1, a = stan_data$verysmallnumber, b = 1)
               ))
  }
  return(inits)
}

