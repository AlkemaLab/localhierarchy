functions {

// functions to create hierarchical parameters, using matrix set, and fixed values and estimates

// vector of sigmas (global, regional levels, country level, subnat level if used) for hierarchical model for one parameter
// just combining fixed and estimated values, where fixed values are for higher order levels only
vector get_onehierparam_sigmawpriorsd(vector oneparam_sigma_fixed,
      vector oneparam_sigma_estimate, real prior_sd_global, int oneparam_n_sigma, int oneparam_n_sigma_fixed, int oneparam_n_sigma_estimate){
  vector[oneparam_n_sigma + 1] oneparam_sigma;
  oneparam_sigma[1] = prior_sd_global;// prior sd for intercept
  if (oneparam_n_sigma_fixed > 0) {
    oneparam_sigma[(1 + 1):(1 + oneparam_n_sigma_fixed)] = oneparam_sigma_fixed;
  }
  if (oneparam_n_sigma_estimate > 0) {
    oneparam_sigma[(1 + oneparam_n_sigma_fixed + 1):(1+oneparam_n_sigma)] = oneparam_sigma_estimate;
  }
  return(oneparam_sigma);
}
// for oneparam in a hierarchical model, get the vector of lowest level values (eg country or subnat level)
// first combine fixed and estimated values, then scale them, then do matrix multiplication to get the
// lowest level values
vector get_onehierparam(vector oneparam_sigma,  vector oneparam_raw_fixed, vector oneparam_raw_estimate,
    array[] int oneparam_re_start, array[] int oneparam_re_end,
    int C, int oneparam_n_terms, int oneparam_n_terms_fixed, int oneparam_n_terms_estimate ,
    vector  oneparam_model_matrix_w, array[] int oneparam_model_matrix_v, array[] int oneparam_model_matrix_u

     ){

  vector[C] oneparam;
  vector[oneparam_n_terms] oneparam_star;
  vector[oneparam_n_terms] oneparam_raw;
  if (oneparam_n_terms_fixed > 0) {
    oneparam_raw[1:oneparam_n_terms_fixed] = oneparam_raw_fixed;
  }
  if (oneparam_n_terms_estimate > 0) {
    oneparam_raw[(1+oneparam_n_terms_fixed):oneparam_n_terms] = oneparam_raw_estimate;
  }
  oneparam_star = scale_blocks(oneparam_raw, oneparam_sigma, oneparam_re_start, oneparam_re_end);
  oneparam = (csr_matrix_times_vector(C, oneparam_n_terms, oneparam_model_matrix_w,
            oneparam_model_matrix_v, oneparam_model_matrix_u, oneparam_star));
  return(oneparam);
}
// just _star part
// to avoid repeated code, check whether it matters for efficiency to call this function in get_onehierparam
vector get_onehierparam_star(vector oneparam_sigma,  vector oneparam_raw_fixed, vector oneparam_raw_estimate,
    array[] int oneparam_re_start, array[] int oneparam_re_end,
  int oneparam_n_terms, int oneparam_n_terms_fixed, int oneparam_n_terms_estimate
     ){

  vector[oneparam_n_terms] oneparam_star;
  vector[oneparam_n_terms] oneparam_raw;
  if (oneparam_n_terms_fixed > 0) {
    oneparam_raw[1:oneparam_n_terms_fixed] = oneparam_raw_fixed;
  }
  if (oneparam_n_terms_estimate > 0) {
    oneparam_raw[(1+oneparam_n_terms_fixed):oneparam_n_terms] = oneparam_raw_estimate;
  }
  oneparam_star = scale_blocks(oneparam_raw, oneparam_sigma, oneparam_re_start, oneparam_re_end);
  return(oneparam_star);
}

matrix get_morehierparam_sigma(matrix a_sigma_fixed, matrix a_sigma_estimate,
        real prior_sd_a, // prior sd for intercept
        int a_n_sigma, int k, //number of parameters, here k = num_basis - num_constrained_zero,
                  int a_n_sigma_fixed,  int a_n_sigma_estimate
                  ){
  matrix[a_n_sigma, k] a_sigma;
  a_sigma[1, ] = rep_row_vector(prior_sd_a, k);
  if (a_n_sigma_fixed > 0) {
    a_sigma[(1 + 1):(1 + a_n_sigma_fixed), ] = a_sigma_fixed;
  }
  if (a_n_sigma_estimate > 0) {
      a_sigma[(1 + a_n_sigma_fixed + 1):a_n_sigma, ] = a_sigma_estimate;
  }
  return(a_sigma);
}

// matrix of non-zero coefficients
matrix get_morehierparam(matrix a_sigma,    matrix a_raw_fixed, matrix a_raw_estimate,
              int k,  //number of parameters, here k = num_basis - num_constrained_zero,
                 int a_n_terms_fixed, int a_n_terms_estimate,
                  array[] int a_re_start, array[] int a_re_end,
                  int C, int a_n_terms,
                  vector a_model_matrix_w, array[] int a_model_matrix_v, array[] int a_model_matrix_u
                  ){
  matrix[a_n_terms, k] a_raw;
  if (a_n_terms_fixed > 0) {
    a_raw[1:a_n_terms_fixed,] = a_raw_fixed;
  }
  if (a_n_terms_estimate > 0) {
    a_raw[(1+a_n_terms_fixed):a_n_terms,] = a_raw_estimate;
  }
  matrix[C, k] a;
  matrix[a_n_terms, k] a_star;
  for(i in 1:k) {
    a_star[, i] = scale_blocks(a_raw[, i], a_sigma[, i], a_re_start, a_re_end);
    a[, i] = csr_matrix_times_vector(C, a_n_terms, a_model_matrix_w, a_model_matrix_v, a_model_matrix_u, a_star[, i]);
  }
  return(a);
}



// helper function for hierarchical parameters
vector scale_blocks(vector raw, vector sigma, array[] int start, array[] int end) {
  int s = rows(sigma);
  vector[rows(raw)] result;
  for(i in 1:s) {
    result[start[i]:end[i]] = raw[start[i]:end[i]] * sigma[i];
  }
  return result;
}


}
/////////////////////////////////////////////////////
data {
  int C; // Number of lowest-level geographic units, e.g. countries or states
  int N; // Number of observations
  array[N] int<lower=0, upper=C> geo_unit; // Geographic unit of each observation, 0 if observation is for an aggregated unit

  // for parameters of process model
  int<lower=0> mu_n_terms;
  int<lower=0> mu_n_sigma; // update 2025/4/2 to NOT count prior SD
  int<lower=0> mu_n_terms_fixed;
  int<lower=0> mu_n_terms_estimate;
  int<lower=0> mu_n_sigma_fixed;
  int<lower=0> mu_n_sigma_estimate;
  array[mu_n_sigma + 1] int<lower=1, upper=mu_n_terms> mu_re_start;
  array[mu_n_sigma + 1] int<lower=1, upper=mu_n_terms> mu_re_end;
  matrix[C, mu_n_terms] mu_model_matrix;

  // int<lower=0> a_n_terms;
  // int<lower=0> a_n_terms_fixed;
  // int<lower=0> a_n_terms_estimate;
  // int<lower=0> a_n_sigma;
  // int<lower=0> a_n_sigma_fixed;
  // int<lower=0> a_n_sigma_estimate;
  // array[a_n_sigma] int<lower=1, upper=a_n_terms> a_re_start;
  // array[a_n_sigma] int<lower=1, upper=a_n_terms> a_re_end;
  // matrix[C, a_n_terms] a_model_matrix;

  // for data model
  vector[N] y;                         // Observations
  // 0: estimate nonse. 1: fix nonse at estimates from previous fit.
  int<lower=0, upper=1> fix_nonse;
  // if fix_nonse == 1, one nonse standard deviation estimate
  array[fix_nonse ? 1 : 0] real<lower=0> nonse_fixed;

  // process model: any fixe values from global runs
  vector[mu_n_terms_fixed] mu_raw_fixed;
  vector<lower=0>[mu_n_sigma_fixed] mu_sigma_fixed;
  // matrix[a_n_terms_fixed, k] a_raw_fixed;
  // matrix<lower=0>[a_n_sigma_fixed, k] a_sigma_fixed;

}

/////////////////////////////////////////////////////
transformed data {
  // transformations for hier parameters
  vector[rows(csr_extract_w(mu_model_matrix))] mu_model_matrix_w     = csr_extract_w(mu_model_matrix);
  array[size(csr_extract_v(mu_model_matrix))] int mu_model_matrix_v  = csr_extract_v(mu_model_matrix);
  array[size(csr_extract_u(mu_model_matrix))] int mu_model_matrix_u  = csr_extract_u(mu_model_matrix);

  // vector[rows(csr_extract_w(a_model_matrix))] a_model_matrix_w             = csr_extract_w(a_model_matrix);
  // array[size(csr_extract_v(a_model_matrix))] int a_model_matrix_v          = csr_extract_v(a_model_matrix);
  // array[size(csr_extract_u(a_model_matrix))] int a_model_matrix_u          = csr_extract_u(a_model_matrix);


}

/////////////////////////////////////////////////////
parameters {
  // process model
  vector[mu_n_terms_estimate] mu_raw_estimate;
  vector<lower=0>[mu_n_sigma_estimate] mu_sigma_estimate;
  // Data model
  array[fix_nonse ? 0 : 1] real<lower=0> nonse_estimate;
}

/////////////////////////////////////////////////////
transformed parameters {
  vector[mu_n_sigma + 1] mu_sigma = get_onehierparam_sigmawpriorsd(mu_sigma_fixed, mu_sigma_estimate, 1, //mu_prior_sd_global,
      mu_n_sigma, mu_n_sigma_fixed,
      mu_n_sigma_estimate);
  vector[C] mu = get_onehierparam(mu_sigma, mu_raw_fixed, mu_raw_estimate,
      mu_re_start, mu_re_end, C, mu_n_terms, mu_n_terms_fixed, mu_n_terms_estimate,
      mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u);

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
  mu_raw_estimate ~ std_normal();
  mu_sigma_estimate ~ normal(0, 1)T[0, positive_infinity()];
  // data model parameters
  nonse_estimate ~ normal(0, 0.5)T[0, positive_infinity()];

  // fit to the data
  for(i in 1:N) {
    y[i] ~ normal(mu[geo_unit[i]], nonse);
  }
}


generated quantities {
  //if (add_hieroutputs){
    vector[mu_n_terms] mu_star = get_onehierparam_star(mu_sigma, mu_raw_fixed, mu_raw_estimate,
      mu_re_start, mu_re_end, mu_n_terms, mu_n_terms_fixed, mu_n_terms_estimate);
  //}


}




