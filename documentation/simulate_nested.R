
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
library(truncnorm) # for inits
library(bayesplot) # diagnostic plots
library(posterior) # else error for as_draws_list
devtools::load_all(here::here())

sim_data <- simulate_etas_nested_multilevel_data(
  n_levels = 3,
  n_units_perlevel = c(30, 20, 10),
  sigma_perlevel = c(1, 0.5, 0.3)
)
sim_data
table(sim_data$level1)         # 30 subcluster
table(sim_data$level2)         # 20 countries
table(sim_data$level3)         # 10 observations in each country
dim(sim_data)                  # 6000 rows, 5 columns (3 levels + eta + y)
sim_data %>% filter(level2 %in% c("1_1", "1_2"))

hierarchical_level <- c("intercept",  "level1", "level2")
sim_data %>%
  group_by(level2) %>%
  group_by(level1) %>%
  summarize(nlevel1 = length(unique(level1)), ncountries = length(unique(level2)))


area <- "level2"
#Create district index for matching district and district index
geo_unit_index <- get_geo_unit_index_data(sim_data,
                                          hierarchical_levels = c(hierarchical_level),
                                          area = area)
geo_unit_index
hier_data <- hierarchical_data(geo_unit_index, hierarchical_level)
names(hier_data)
# what data entries does it expect?
dat <- sim_data %>%
  dplyr::rename(y = total_effect)
dat

fit_global <- fit_model_localhierarchy(runstep = "global_national",
                                       hierarchical_level     =  hierarchical_level,
                                       area = area,
                                       survey_df = dat,
                                       y = "y",
                                       chains = 4)

#create summary object
fit_global$post_summ <- get_posterior_summaries(fit_global)
fit_global$post_summ

#do local fit... here we fix things but do get results for all countries
hier_stan_data <- hierarchical_param_stan_data(
  global_fit = fit_global,
  param_name = "mu",
  param_data = hier_data,
  hierarchical_terms_fixed = hierarchical_level[1:2],
  hierarchical_sigmas_fixed = hierarchical_level[1:3]
)
names(hier_stan_data)
#get a local fit
fit_local <- fit_model_localhierarchy(runstep = "local_national",
                                      global_fit = fit_global,
                                      area = area,
                                      survey_df = dat,
                                      y = "y",
                                      chains = 4)

# get summaries, takes a little while. this creates, per fit, a list with each entry being one level, and the results (mu estimates) for that level

res_global <- posterior_summary_hierparam(fit = fit_global, parname = "mu")
res_local <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

#Plots that show outputs
plot_posterior_summaries(res = res_global, modelname1 = "global")
#how values have been fixed in the local runs
plot_posterior_summaries(res = res_local,
                         res2 = res_global,
                         modelname2 = "global", modelname1 = "local")

# check sigma_y, called nonse
fit_global$post_summ %>%
  filter(variable == "nonse[1]")

# reproduce intercept estimate
# this is the gamma, standardized value
fit_global$post_summ %>%
  filter(variable == "mu_raw[1]")
# to get mu_global: mu_global = sd*(gamma + mu_prior_mean)
# such that gamma = mu_global/sd - mu_prior_mean
# for rescaling, we used
# from stan function
#mu_sigmawpriorsd[1] = mu_prior_sd;
#mu_raw[1] = mu_raw[1] + mu_prior_mean;
fit_global$stan_data$mu_scalarprior_mean
fit_global$stan_data$mu_scalarprior_sd

gamma <- fit_global$post_summ %>%
  filter(variable == "mu_raw[1]") %>%pull(postmean)
fit_global$stan_data$mu_scalarprior_sd*(gamma + fit_global$stan_data$mu_scalarprior_mean)

