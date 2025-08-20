functions {
// functions to create hierarchical parameters, using matrix set, and fixed values and estimates
#include ./functions_hierarchical_params.stan
}
/////////////////////////////////////////////////////
data {
  real<lower = 0> verysmallnumber; // lower bound for sigmas
  int n_geounit; // Number of lowest-level geographic units, e.g. countries or states
  int N; // Number of observations
  array[N] int<lower=0, upper=n_geounit> geo_unit; // Geographic unit of each observation, 0 if observation is for an aggregated unit

  // for data model
  vector[N] y;                         // Observations
  // 0: estimate nonse. 1: fix nonse at estimates from previous fit.
  int<lower=0, upper=1> fix_nonse;
  // if fix_nonse == 1, one nonse standard deviation estimate
  array[fix_nonse ? 1 : 0] real<lower=verysmallnumber> nonse_fixed;

  // parameter mu
  // for mu hierarchical set up (indep of whether mu is vector or not)
  int<lower=0> mu_raw_n_terms;
  int<lower=0> mu_raw_n_terms_fixed;
  int<lower=0> mu_raw_n_terms_estimate;
  int<lower=0> mu_n_sigma;
  int<lower=0> mu_n_sigma_fixed;
  int<lower=0> mu_n_sigma_estimate;
  array[mu_n_sigma + 1] int<lower=1, upper=mu_raw_n_terms> mu_re_start;
  array[mu_n_sigma + 1] int<lower=1, upper=mu_raw_n_terms> mu_re_end;
  matrix[n_geounit, mu_raw_n_terms] mu_model_matrix;
  real<lower = verysmallnumber> mu_scalarprior_sd;
  real mu_scalarprior_mean;
  real<lower = verysmallnumber> mu_prior_sd_sigma_estimate;

  // CHOOSE ONE
  // // if mu is a scalar:
  // vector[mu_raw_n_terms_fixed] mu_raw_fixed;
  // vector<lower=verysmallnumber>[mu_n_sigma_fixed] mu_sigma_fixed;

  // if mu is a vector:
  int mu_k_terms; // number of parameters in vector mu
  matrix[mu_raw_n_terms_fixed, mu_k_terms] mu_raw_fixed;
  matrix<lower=verysmallnumber>[mu_n_sigma_fixed, mu_k_terms] mu_sigma_fixed;
  // END CHOOSE ONE

}

/////////////////////////////////////////////////////
transformed data {
  // transformations for mu model matrix (same for scalar and vector)
  vector[rows(csr_extract_w(mu_model_matrix))] mu_model_matrix_w     = csr_extract_w(mu_model_matrix);
  array[size(csr_extract_v(mu_model_matrix))] int mu_model_matrix_v  = csr_extract_v(mu_model_matrix);
  array[size(csr_extract_u(mu_model_matrix))] int mu_model_matrix_u  = csr_extract_u(mu_model_matrix);

  // creating additional data for mu_vector runs
  matrix[N,mu_k_terms] y_matrix;
  for (k in 1:mu_k_terms){
    y_matrix[,k] = y + (k-1)/2.0;
  }
}

/////////////////////////////////////////////////////
parameters {
  // CHOOSE ONE
  // // if mu is a scalar:
  // vector[mu_raw_n_terms_estimate] mu_raw_estimate;
  // vector<lower=verysmallnumber>[mu_n_sigma_estimate] mu_sigma_estimate;

  // if mu is a vector:
  matrix[mu_raw_n_terms_estimate, mu_k_terms] mu_raw_estimate;
  // choose one here too
  // no ordering
  matrix<lower=verysmallnumber>[mu_n_sigma_estimate, mu_k_terms] mu_sigma_estimate;
  // if we want ordering, ie variance to decrease with the hierarchical level
  // then currently needs hardcoding of number of parameters
  // and sampling needs truncation at verysmallnumber
  // positive_ordered [mu_n_sigma_estimate] mu_sigma_estimate_reverse_1;
  // positive_ordered [mu_n_sigma_estimate] mu_sigma_estimate_reverse_2;
  // positive_ordered [mu_n_sigma_estimate] mu_sigma_estimate_reverse_3;
  // END CHOOSE ONE



  // Data model
  array[fix_nonse ? 0 : 1] real<lower = verysmallnumber> nonse_estimate;
}

/////////////////////////////////////////////////////
transformed parameters {

 // CHOOSE ONE
 // // if mu is a scalar:
 // vector[mu_raw_n_terms] mu_star = get_mu_star(
 //    mu_n_sigma, mu_n_sigma_fixed, mu_n_sigma_estimate,
 //    mu_sigma_fixed, mu_sigma_estimate,
 //    mu_scalarprior_sd,
 //    mu_raw_n_terms, mu_raw_n_terms_fixed, mu_raw_n_terms_estimate,
 //    mu_raw_fixed, mu_raw_estimate,
 //    mu_re_start, mu_re_end);
 // vector[n_geounit] mu = get_mu(
 //    mu_star,
 //    mu_raw_n_terms,
 //    n_geounit,
 //    mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u)

  // if mu is a vector:
  // // if sigmas are ordered
  // matrix[mu_n_sigma_estimate, mu_k_terms] mu_sigma_estimate;
  // if (mu_n_sigma_estimate > 0) {
  //   mu_sigma_estimate[1:mu_n_sigma_estimate, 1] = reverse(mu_sigma_estimate_reverse_1);
  //   mu_sigma_estimate[1:mu_n_sigma_estimate, 2] = reverse(mu_sigma_estimate_reverse_2);
  //   mu_sigma_estimate[1:mu_n_sigma_estimate, 3] = reverse(mu_sigma_estimate_reverse_3);
  // }
  matrix[mu_raw_n_terms,mu_k_terms] mu_star = get_mudimhk_star(mu_k_terms,
       mu_n_sigma, mu_n_sigma_fixed, mu_n_sigma_estimate,
       mu_sigma_fixed, mu_sigma_estimate,
       mu_scalarprior_mean, mu_scalarprior_sd,
       mu_raw_n_terms, mu_raw_n_terms_fixed, mu_raw_n_terms_estimate,
       mu_raw_fixed, mu_raw_estimate,
       mu_re_start, mu_re_end);
   matrix[n_geounit,mu_k_terms] mu = get_mudimhk(mu_k_terms,
       mu_star,
       mu_raw_n_terms,
       n_geounit,
       mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u);
  // END CHOOSE ONE


  // Data model: get sigma
  real nonse;
  if (fix_nonse) {
    nonse = nonse_fixed[1];
  } else {
    nonse = nonse_estimate[1];
  }

}

/////////////////////////////////////////////////////
model {
  // hierarchical parameters
  to_vector(mu_raw_estimate) ~ std_normal();
  // not ordered
  to_vector(mu_sigma_estimate) ~ normal(0, mu_prior_sd_sigma_estimate);
  // ordered
   // to_vector(mu_sigma_estimate_reverse_1) ~ normal(0, mu_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
   // to_vector(mu_sigma_estimate_reverse_2) ~ normal(0, mu_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
   // to_vector(mu_sigma_estimate_reverse_3) ~ normal(0, mu_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  // data model parameters
  nonse_estimate ~ normal(0, 1);

  //fit to the data
  // // if mu is a vector
  for (k in 1:mu_k_terms){
    y_matrix[1:N,k] ~ normal(mu[geo_unit[1:N],k], nonse);
  }
  for (i in 1:N){
    for (k in 1:mu_k_terms){
      y_matrix[i,k] ~ normal(mu[geo_unit[i],k], nonse);
  }
  }
  // END CHOOSE ONE

}


generated quantities {

}




