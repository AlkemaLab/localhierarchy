

#' Plot prior and posterior densities of location parameters
#'
#' This function plots the posterior densities of location (`mu_raw`) parameters, with priors added.
#'
#' @param fit List that includes "parname"_raw_estimate and stan_data
#' @param parname Selected parameter name (example: mu)
#' @param morethan1param Logical, does parname refer to more than 1 parameter (a vector)
#' @param nresultsperpage Number of results per page in summary plots
#'
#' @returns lists with list 'summary_plots' and list 'plots_allmuraw'.
#' summary plots gives summary CIs, nresultsperpage at a time.
#' plots_allmuraw gives all the individual plots of each mu_raw, with density per chain and prior added
#' If morethan1param = TRUE, then each list contains a list per parameter k
#'
#' @export
#'
#' @importFrom posterior as_draws_list
#' @importFrom bayesplot mcmc_dens_overlay
#'
plot_muraw_localhierarchy <- function(fit, parname, morethan1param = FALSE,
                        nresultsperpage = 30){
  helper_get_plots_allmuraw <- function(samples_tibble){
    parnames <- names(samples_tibble)[-seq(1,3)] # %>% select(-.chain, -.iteration, -.draw))
    samp <- posterior::as_draws_list(samples_tibble)
    plots_allmuraw <- list()
    for (parname in parnames){
      plots_allmuraw[[parname]] <- bayesplot::mcmc_dens_overlay(samp, pars = parname) +
        # warning when adding the dnorm, not sure why
        stat_function(fun = dnorm, color = "red")
    }
    return(plots_allmuraw)
  }
  # for overview plots
  # can do by specific group
  #mcmc_intervals(samp, pars = parnames[grepl("mu_subcluster", parnames)])
  #mcmc_intervals(samp, pars = parnames[!grepl("mu_iso", parnames)])
  # here just 50 at a time
  plot_summary_cis <- function(samp, parnames){
    summary_plots <- list()
    nparam <- length(parnames)
    j <- 1
    for (i in seq(1, nparam, by = nresultsperpage)){
      if ((i + (nresultsperpage-1)) > nparam){
        i_end <- nparam
      } else {
        i_end <- i + (nresultsperpage-1)
      }
      summary_plots[[j]] <- mcmc_intervals(samp, pars = parnames[i:i_end])
      j <- j + 1
    }
    return(summary_plots)
  }

  # getting results
  muraw <- get_mu_raw_labeled(fit = fit, parname = parname, morethan1param = morethan1param)
  samples_tibble_all <- muraw %>%
    mutate(parname = paste0(parname, hierarchical_level, hierarchical_unit)) %>%
    select(-c(i, hierarchical_level, hierarchical_unit)) %>%
    pivot_wider(values_from = paste0(parname, "_raw_estimate"),
                names_from = parname)
  # has samples, including a column with k for multiparameter
  # so need to account for k in plotting
  if (!morethan1param){
    plots_allmuraw <- helper_get_plots_allmuraw(samples_tibble_all)
    parnames <- names(plots_allmuraw)
    summary_plots <- plot_summary_cis(samples_tibble_all, parnames)
  } else {
    plots_allmuraw <- summary_plots <- list()
    for (k_select in 1:max(samples_tibble_all$k)){
      samples_tibble <- samples_tibble_all %>%
        ungroup(k) %>%
        filter(k == k_select) %>%
        select(-k)
      plots_allmuraw[[k_select]] <- helper_get_plots_allmuraw(samples_tibble)
      parnames <- names(plots_allmuraw[[k_select]])
      summary_plots[[k_select]] <- plot_summary_cis(samples_tibble, parnames)
    }
  }
  return(list(plots_allmuraw = plots_allmuraw,
              summary_plots = summary_plots))
}

#' Plot prior and posteriors of sigmas from hierarchical models
#'
#' This function plots the prior and posterior densities of sigma_estimate parameters.
#'
#' @param fit List, needs to include parname_sigma_estimate and stan_data
#' @param parname Selected parameter name (example: "mu")
#'
#' @returns Plot with density of sigma_estimate and prior added
#' @export
#'
#'@importFrom truncnorm dtruncnorm
#'
plot_prior_post_sigmas_localhierarchy <- function(fit, parname){
  # sigmas, fine to look at all k's combined
  samp <- fit$samples$draws(paste0(parname, "_sigma_estimate"))
  parnames <- dimnames(samp)[[3]]
  # yes that's a convoluted way to get either the number of parameters for multiparam setting, or number of sigmas otherwise
  max_korlevel <- max(unlist(map(strsplit(parnames, split = ","), function(x) as.numeric(readr::parse_number(rev(x)[1])))))
  p <- mcmc_dens_overlay(samp,
                         facet_args = list(nrow = max_korlevel)) +
    stat_function(fun = dtruncnorm, color = "red",
                  args = list(a = 0, mean = 0,
                              sd = fit$stan_data[[paste0(parname, "_prior_sd_sigma_estimate")]]
                              ))
  return(p)
}
