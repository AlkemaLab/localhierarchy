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

  // if mu is a scalar:
  vector[mu_raw_n_terms_fixed] mu_raw_fixed;
  vector<lower=verysmallnumber>[mu_n_sigma_fixed] mu_sigma_fixed;

  // END CHOOSE ONE

}

/////////////////////////////////////////////////////
transformed data {
  // transformations for mu model matrix (same for scalar and vector)
  vector[rows(csr_extract_w(mu_model_matrix))] mu_model_matrix_w     = csr_extract_w(mu_model_matrix);
  array[size(csr_extract_v(mu_model_matrix))] int mu_model_matrix_v  = csr_extract_v(mu_model_matrix);
  array[size(csr_extract_u(mu_model_matrix))] int mu_model_matrix_u  = csr_extract_u(mu_model_matrix);
}

/////////////////////////////////////////////////////
parameters {
  // CHOOSE ONE
  // if mu is a scalar:
  vector[mu_raw_n_terms_estimate] mu_raw_estimate;
  // non-ordered:
  vector<lower=verysmallnumber>[mu_n_sigma_estimate] mu_sigma_estimate;
  // ordered:
  // positive_ordered [mu_n_sigma_estimate] mu_sigma_estimate_reverse;


  // // if mu is a vector:
  // matrix[mu_raw_n_terms_estimate, mu_k_terms] mu_raw_estimate;
  // matrix<lower=verysmallnumber>[mu_n_sigma_estimate, mu_k_terms] mu_sigma_estimate;
  // END CHOOSE ONE

  // Data model
  array[fix_nonse ? 0 : 1] real<lower=verysmallnumber> nonse_estimate;
}

/////////////////////////////////////////////////////
transformed parameters {

  // if ordered, comment out
  // vector[mu_n_sigma_estimate] mu_sigma_estimate = reverse(mu_sigma_estimate_reverse);

 // CHOOSE ONE
 // if mu is a scalar:
 vector[mu_raw_n_terms] mu_star = get_mu_star(
    mu_n_sigma, mu_n_sigma_fixed, mu_n_sigma_estimate,
    mu_sigma_fixed, mu_sigma_estimate,
    mu_scalarprior_mean,
    mu_scalarprior_sd,
    mu_raw_n_terms, mu_raw_n_terms_fixed, mu_raw_n_terms_estimate,
    mu_raw_fixed, mu_raw_estimate,
    mu_re_start, mu_re_end);
 vector[n_geounit] mu = get_mu(
    mu_star,
    mu_raw_n_terms,
    n_geounit,
    mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u);

  // // if mu is a vector:
  // matrix[mu_raw_n_terms,mu_k_terms] mu_star = get_mudimhk_star(mu_k_terms,
  //      mu_n_sigma, mu_n_sigma_fixed, mu_n_sigma_estimate,
  //      mu_sigma_fixed, mu_sigma_estimate,
  //      mu_scalarprior_sd,
  //      mu_raw_n_terms, mu_raw_n_terms_fixed, mu_raw_n_terms_estimate,
  //      mu_raw_fixed, mu_raw_estimate,
  //      mu_re_start, mu_re_end);
  //  matrix[n_geounit,mu_k_terms] mu = get_mudimhk(mu_k_terms,
  //      mu_star,
  //      mu_raw_n_terms,
  //      n_geounit,
  //      mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u);
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
  // non-ordered
  to_vector(mu_sigma_estimate) ~ normal(0, mu_prior_sd_sigma_estimate);
  //ordered
  // mu_sigma_estimate_reverse ~ normal(0, mu_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];

  // data model parameters
  nonse_estimate ~ normal(0, 1);

  //fit to the data
  y[1:N] ~ normal(mu[geo_unit[1:N]], nonse);

}


generated quantities {

}




