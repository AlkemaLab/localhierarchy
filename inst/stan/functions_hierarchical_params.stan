
/**
 * function get_mu_star
 * calculates mu_star FOR ONE PARAMETER MU from inputs for
 * vector mu_sigmasigmawpriorsd and (mu_fixed, mu_raw) vector.
 * for hierarchical model for one parameter
 * using fixed and estimated values of mu_sigmas and mu_raws, where fixed values are for higher order levels only.
 * part 1: to get mu_sigmawpriorsd
 * we combine prior sd and vector of sigmas (regional levels, country level, subnat level if used)
 * @param mu_n_sigma Number of sigmas
 * @param mu_n_sigma_fixed Number of fixed sigmas
 * @param mu_n_sigma_estimate Number of estimated sigmas
 * @param mu_sigma_fixed Fixed sigma values
 * @param mu_sigma_estimate Estimated sigma values
 * @param mu_prior_mean Prior mean
 * @param mu_prior_sd Prior sd
 * @param mu_raw_n_terms Number of mu_raws
 * @param mu_raw_n_terms_fixed Number of fixed mu_raws
 * @param mu_raw_n_terms_estimate Number of estimated mu_raws
 * @param mu_raw_fixed Fixed mu_raws
 * @param mu_raw_estimate Estimated mu_raws
 * @param mu_re_start Vector with start indices of the different hier levels
 * @param mu_re_end Vector with end indices of the different hier levels
 * @return Vector of mu_stars
 */
vector get_mu_star(
 int mu_n_sigma,
 int mu_n_sigma_fixed, int mu_n_sigma_estimate,
 vector mu_sigma_fixed, vector mu_sigma_estimate,
 real mu_prior_mean,
 real mu_prior_sd,
 // mu_raw part
 int mu_raw_n_terms,
   int mu_raw_n_terms_fixed, int mu_raw_n_terms_estimate,
 vector mu_raw_fixed, vector mu_raw_estimate,
    array[] int mu_re_start, array[] int mu_re_end
     ){

  // mu_sigmawpriorsd part
  vector[mu_n_sigma + 1] mu_sigmawpriorsd;
  mu_sigmawpriorsd[1] = mu_prior_sd;
  if (mu_n_sigma_fixed > 0) {
    mu_sigmawpriorsd[(1 + 1):(1 + mu_n_sigma_fixed)] = mu_sigma_fixed;
  }
  if (mu_n_sigma_estimate > 0) {
    mu_sigmawpriorsd[(1 + mu_n_sigma_fixed + 1):(1+mu_n_sigma)] = mu_sigma_estimate;
  }
  vector[mu_raw_n_terms] mu_raw;
  if (mu_raw_n_terms_fixed > 0) {
    mu_raw[1:mu_raw_n_terms_fixed] = mu_raw_fixed;
  }
  if (mu_raw_n_terms_estimate > 0) {
    mu_raw[(1+mu_raw_n_terms_fixed):mu_raw_n_terms] = mu_raw_estimate;
  }
  // note that mu_prior_mean is currently on standardized scale,
  // ie mu_global = sd*(gamma + mu_prior_mean)
  // (could update into +mu_prior_mean/mu_prior_sd is easier if mean is on mu_global scale)
  mu_raw[1] = mu_raw[1] + mu_prior_mean;
  vector[mu_raw_n_terms] mu_star = scale_blocks(mu_raw, mu_raw_n_terms, mu_sigmawpriorsd, mu_n_sigma + 1, mu_re_start, mu_re_end);
  return(mu_star);
}

/**
 * function scale_blocks
 * Helper function for hierarchical parameters
 * @param mu_raw Vector of mu_raws
 * @param mu_raw_n_terms Length of vector mu_raw
 * @param mu_sigmawpriorsd Vector of (priorsd, sigmas)
 * @param n_mu_sigmawpriorsd Length of vector mu_sigmawpriorsd
 * @param re_start
 * @param mu_model_matrix_v Model matrix component V
 * @param mu_model_matrix_u Model matrix component U
 * @return Vector mu_star
 */
vector scale_blocks(vector mu_raw, int mu_raw_n_terms,
 vector mu_sigmawpriorsd, int n_mu_sigmawpriorsd,
 array[] int re_start, array[] int re_end) {
  vector[mu_raw_n_terms] result;
  for(i in 1:n_mu_sigmawpriorsd) {
    result[re_start[i]:re_end[i]] = mu_raw[re_start[i]:re_end[i]] * mu_sigmawpriorsd[i];
  }
  return result;
}

/**
 * function get_mu
 * calculates vector of lowest level mus from mu_star and (info from) sparse model matrix
 * @param mu_star Vector of mu_stars obtained from get_mu_star
 * @param mu_raw_n_terms Number of mu_raws
 * @param n_geounit Number of lowest level units (eg countries or subnational regions)
 * @param mu_model_matrix_w Model matrix component W
 * @param mu_model_matrix_v Model matrix component V
 * @param mu_model_matrix_u Model matrix component U
 * @return Vector of mu_lowestlevel
 */
vector get_mu(vector mu_star, int mu_raw_n_terms,
    int n_geounit,
    vector  mu_model_matrix_w, array[] int mu_model_matrix_v, array[] int mu_model_matrix_u
     ){
  vector[n_geounit] mu = csr_matrix_times_vector(n_geounit, mu_raw_n_terms, mu_model_matrix_w,
            mu_model_matrix_v, mu_model_matrix_u, mu_star);
  return(mu);
}

// FUNCTIONS FOR VECTOR MU OF DIMENSION K
//mudimhk refers to parameter vector mu_k, when modeled hierarchically, dimension is mu_hk

/**
 * function get_mudimhk_star
 * calculates mu_star FOR VECTOR MU
 * mudimhk refers to parameter vector mu_k, when modeled hierarchically, dimension is mu_hk
 * using fixed and estimated values of mu_sigmas and mu_raws, where fixed values are for higher order levels only.
 * part 1: to get mu_sigmawpriorsd
 * we combine prior sd and vector of sigmas (regional levels, country level, subnat level if used)
 * @param k Number of parameters
 * @param mu_n_sigma Number of sigmas
 * @param mu_n_sigma_fixed Number of fixed sigmas
 * @param mu_n_sigma_estimate Number of estimated sigmas
 * @param mudimhk_sigma_fixed Fixed sigma values
 * @param mudimhk_sigma_estimate Estimated sigma values
 * @param mudimhk_scalarprior_mean Prior mean for intercept (repeated across the parameters, scalar)
 * @param mudimhk_scalarprior_sd Prior sd for intercept (repeated across the parameters, scalar)
 * @param mu_raw_n_terms Number of mu_raws
 * @param mu_raw_n_terms_fixed Number of fixed mu_raws
 * @param mu_raw_n_terms_estimate Number of estimated mu_raws
 * @param mudimhk_raw_fixed Fixed mu_raws
 * @param mudimhk_raw_estimate Estimated mu_raws
 * @param mu_re_start Vector with start indices of the different hier levels
 * @param mu_re_end Vector with end indices of the different hier levels
 * @return Matrix of mu_stars
 */
matrix get_mudimhk_star(
 int k,  //number of parameters
 // sigma part
 int mu_n_sigma,
 int mu_n_sigma_fixed, int mu_n_sigma_estimate,
 matrix mudimhk_sigma_fixed, matrix mudimhk_sigma_estimate,
 real mudimhk_scalarprior_mean,
 real mudimhk_scalarprior_sd,
 // mu_raw part
 int mu_raw_n_terms,
 int mu_raw_n_terms_fixed, int mu_raw_n_terms_estimate,
 matrix mudimhk_raw_fixed, matrix mudimhk_raw_estimate,
 array[] int mu_re_start, array[] int mu_re_end
     ){
 // part 1: get sigmawpriorsdmatrix
 matrix[mu_n_sigma + 1, k] mudimhk_sigmawpriorsd;
  mudimhk_sigmawpriorsd[1, ] = rep_row_vector(mudimhk_scalarprior_sd, k);
  if (mu_n_sigma_fixed > 0) {
    mudimhk_sigmawpriorsd[(1 + 1):(1 + mu_n_sigma_fixed), ] = mudimhk_sigma_fixed;
  }
  if (mu_n_sigma_estimate > 0) {
    mudimhk_sigmawpriorsd[(1 + mu_n_sigma_fixed + 1):(1 + mu_n_sigma), ] = mudimhk_sigma_estimate;
  }
 // part 2: get mu_raw
  matrix[mu_raw_n_terms, k] mudimhk_raw;
  if (mu_raw_n_terms_fixed > 0) {
    mudimhk_raw[1:mu_raw_n_terms_fixed,] = mudimhk_raw_fixed;
  }
  if (mu_raw_n_terms_estimate > 0) {
    mudimhk_raw[(1+mu_raw_n_terms_fixed):mu_raw_n_terms,] = mudimhk_raw_estimate;
  }
  mudimhk_raw[1,] = mudimhk_raw[1,] + mudimhk_scalarprior_mean;
  matrix[mu_raw_n_terms, k] mudimhk_star;
  for(i in 1:k) {
    mudimhk_star[, i] = scale_blocks(mudimhk_raw[, i], mu_raw_n_terms, mudimhk_sigmawpriorsd[, i], mu_n_sigma + 1, mu_re_start, mu_re_end);
  }
  return(mudimhk_star);
}


/**
 * function get_mudimhk
 * calculates matrix of lowest level mus from mu_star and (info from) sparse model matrix
 * @param k Number of parameters
 * @param mudimhk_star Matrix of mu_stars obtained from get_mudimhk_star
 * @param mu_raw_n_terms Number of mu_raws
 * @param n_geounit Number of lowest level units (eg countries or subnational regions)
 * @param mu_model_matrix_w Model matrix component W
 * @param mu_model_matrix_v Model matrix component V
 * @param mu_model_matrix_u Model matrix component U
 * @return Matrix of mu_lowestlevel
 */
matrix get_mudimhk(int k,
    matrix mudimhk_star,
    int mu_raw_n_terms,
    int n_geounit,
    vector  mu_model_matrix_w, array[] int mu_model_matrix_v, array[] int mu_model_matrix_u
                  ){

  matrix[n_geounit, k] mudimhk;
  for(i in 1:k) {
    mudimhk[, i] = csr_matrix_times_vector(n_geounit, mu_raw_n_terms, mu_model_matrix_w, mu_model_matrix_v, mu_model_matrix_u, mudimhk_star[, i]);
  }
  return(mudimhk);
}


// no longer needed (not evaluated separately)
// /**
//  * function get_mu_sigmawpriorsd
//  * combines prior sd and vector of sigmas (regional levels, country level, subnat level if used)
//  * for hierarchical model for one parameter
//  * uses fixed and estimated values of mu_sigmas, where fixed values are for higher order levels only.
//  * @param mu_n_sigma Number of sigmas
//  * @param mu_n_sigma_fixed Number of fixed sigmas
//  * @param mu_n_sigma_estimate Number of estimated sigmas
//  * @param mu_sigma_fixed Fixed sigma values
//  * @param mu_sigma_estimate Estimated sigma values
//  * @param mu_prior_sd Prior sd
//  * @return Vector (priorsd, mu_sigmas)
//  */
// vector get_mu_sigmawpriorsd(int mu_n_sigma,
//                           int mu_n_sigma_fixed, int mu_n_sigma_estimate,
//                           vector mu_sigma_fixed, vector mu_sigma_estimate,
//                           real mu_prior_sd
//       ){
//   vector[mu_n_sigma + 1] mu_sigma;
//   mu_sigma[1] = mu_prior_sd;
//   if (mu_n_sigma_fixed > 0) {
//     mu_sigma[(1 + 1):(1 + mu_n_sigma_fixed)] = mu_sigma_fixed;
//   }
//   if (mu_n_sigma_estimate > 0) {
//     mu_sigma[(1 + mu_n_sigma_fixed + 1):(1+mu_n_sigma)] = mu_sigma_estimate;
//   }
//   return(mu_sigma);
// }

