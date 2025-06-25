
# different use cases
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
library(truncnorm) # for inits
library(bayesplot) # diagnostic plots
library(posterior)
devtools::load_all(here::here())
data_folder <- "data_raw"

# national data
dat <- read_csv(here::here(data_folder, "coverage_data.csv"))
dat_subnat <- read_csv(here::here(data_folder, "coverage_data_subnat.csv"))
hierarchical_level <- c("intercept",  "subcluster", "iso")

### overview, for more than 1 param
# use case 1: global then local national, all countries (in quarto too)
# use case 2: global then local national, 1 country
# use case 3: global then subnational global then local subnational
### extended to also be able to produce subnational estimates for countries that were
# included in global national run but not in global subnational run
# use case 4 needed? invariance to re-ordering of input data in use case 3


fit1a_mult <- fit_model_localhierarchy(runstep = "global_national",
                                       mu_isvector = TRUE,
                                       hierarchical_level     =  hierarchical_level,
                                       survey_df = dat,
                                       chains = 4)

# check the mu_raws
plots <- plot_mu_raw(fit = fit1a_mult, parname = "mu", morethan1param = TRUE)
# a list with plots
# summary plots is a list of dimension k, for each k, it gives summaries, 30 at a time
plots[["summary_plots"]][[1]][[1]]
plots[["summary_plots"]][[2]][[1]]
plots[["summary_plots"]][[3]][[1]]
# specific plots for one parameter are in plots[["plots_allmuraw"]][[k_select]]
names(plots[["plots_allmuraw"]][[1]])
plots[["plots_allmuraw"]][[1]][[1]]
plots[["plots_allmuraw"]][grepl("intercept", names(plots[["plots_allmuraw"]]))]
plots[["plots_allmuraw"]][grepl("subcluster", names(plots[["plots_allmuraw"]]))][[1]]

# plot the sigmas
# these are still fine even when ordered because they are defined in transformed params block
plot_prior_post_sigmas(fit = fit1a_mult, parname = "mu")

res_mult <-  posterior_summary_hierparam(fit = fit1a_mult, parname = "mu", morethan1param = TRUE)

# local multiparam
fit1a_mult$post_summ <- get_posterior_summaries(fit1a_mult)
# mu is dimension hxk
fit1a_mult$post_summ %>% filter(variable %in%
                c("mu_raw[1,1]",  "mu_raw[1,2]", "mu_raw[1,3]"))

fit_local_multi <- fit_model_localhierarchy(runstep = "local_national",
                                            mu_isvector = TRUE,
                                            global_fit = fit1a_mult,
                                            survey_df = dat,
                                            chains = 4)

res_local_mult <-  posterior_summary_hierparam(fit = fit_local_multi, parname = "mu", morethan1param = TRUE)

# shouldn't work
# plot_prior_post_sigmas(fit = fit_local_multi, parname = "mu")

# compare global and local national
for (k_select in 1:3){
  p <- plot_posterior_summaries(res = res_local_mult, res2 = res_mult,
                           hierarchy_select = "iso",
                           areas_select = res_local_mult$iso$name[1:100],
                           k_select = k_select)
  print(p)
}

# global subnat mult param
fit_subnational_mult <- fit_model_localhierarchy(runstep = "global_subnational",
                                                 mu_isvector = TRUE,
                                                 hierarchical_level =  hierarchical_level,
                                                 survey_df = dat_subnat,
                                                 area = "subnat",
                                                 global_fit = fit1a_mult,
                                                 chains = 4)
fit_subnational_mult$post_summ <- get_posterior_summaries(fit_subnational_mult)
fit_subnational_mult$post_summ %>% filter(variable %in%
             c("mu_raw[1,1]",  "mu_raw[1,2]", "mu_raw[1,3]"))

res_subnational_mult <- posterior_summary_hierparam(fit = fit_subnational_mult, parname = "mu", morethan1param = TRUE)

# local subnat mult param
# all region and 1-region
iso_select <- "BFA"
fit_local_subnat_mult <- fit_model_localhierarchy(runstep = "local_subnational",
                                                  mu_isvector = TRUE,
                                                  hierarchical_level     =  hierarchical_level,
                                                  survey_df = dat_subnat %>% filter(iso == iso_select),
                                                  area = "subnat",
                                                  global_fit = fit_subnational_mult,
                                                  chains = 4)

res_local_subnat_mult <- posterior_summary_hierparam(fit = fit_local_subnat_mult,
                                                     parname = "mu", morethan1param = TRUE)

p <- plot_posterior_summaries(res = res_subnational_mult, res2 = res_mult,
                              modelname2 = "global", modelname1 = "local")

p
plot_posterior_summaries(res = res_local_subnat_mult, res2 = res_subnational_mult,
                         hierarchy_select = "subnat",
                         areas_select = res_subnational_mult$subnat$name[1:100])

plot_posterior_summaries(res = res_local_subnat_mult, res2 = res_subnational_mult,
                         hierarchy_select = "subnat",
                         areas_select = res_subnational_mult$subnat$name[1:100],
                         k_select = 1)

plot_posterior_summaries(res = res_local_mult, res2 = res_mult,
                         hierarchy_select = "iso",
                         areas_select = res_subnational_mult$iso$name[1:100],
                         k_select = 1)
plot_posterior_summaries(res = res_local_mult, res2 = res_mult,
                         hierarchy_select = "iso",
                         areas_select = res_subnational_mult$iso$name[1:100],
                         k_select = 2)



##### extending use case 3
# after getting
# fit_subnational_mult
# fit1a_mult
# global subnat just adds mu_sigma for another level
fit_subnational_mult$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit1a_mult$post_summ %>%
  filter(variable_no_index == "mu_sigma")
# goal = keep national estimates from 1a, also for countries w/o subnat data

# approach
# use global fit nat
# add in sigmas by selecting params not in global fit nat, "sigma_"
# and save as globalsubnat_fromnat_fit
# for local subnational, update hierarchical levels (do add one, just like in global subnat)

# parameters to add:
param_add <- anti_join(fit_subnational_mult$post_summ %>%
                         # select rows where variable_no_index has sigma in it
                         filter(grepl("sigma", variable_no_index)),
                       fit1a_mult$post_summ %>%
                         filter(grepl("sigma", variable_no_index)))
param_add
fit_globalsubnat_fromnat <- fit1a_mult
fit_globalsubnat_fromnat$post_summ <- bind_rows(fit_globalsubnat_fromnat$post_summ, param_add)

fit_local1 <- fit_model_localhierarchy(runstep = "local_subnational",
                                       mu_isvector = TRUE,
                                       use_globalsubnat_fromnat = FALSE,
                                       area = "subnat",
                                       global_fit = fit_subnational_mult,
                                       survey_df = dat_subnat,
                                       chains = 4)

fit_local2 <- fit_model_localhierarchy(runstep = "local_subnational",
                                       mu_isvector = TRUE,
                                       use_globalsubnat_fromnat = TRUE,
                                       area = "subnat",
                                       global_fit = fit_globalsubnat_fromnat,
                                       survey_df = dat_subnat,
                                       chains = 4)

res_localsubnat2 <- posterior_summary_hierparam(fit = fit_local2, parname = "mu",
                                                morethan1param = TRUE)
res_localsubnat1 <- posterior_summary_hierparam(fit = fit_local1, parname = "mu",
                                                morethan1param = TRUE)

# plots: compare global subnat to local subnat
plot_posterior_summaries(res = res_localsubnat1, res2 = res_mult,
                         modelname2 = "global nat", modelname1 = "local",
                         hierarchy_select = "subcluster")
plot_posterior_summaries(res = res_localsubnat1, res2 = res_subnational_mult,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "iso")
plot_posterior_summaries(res = res_localsubnat2, res2 = res_subnational_mult,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "iso")
plot_posterior_summaries(res = res_localsubnat2, res2 = res_subnational_mult,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "subnat",
                         areas_select = res_localsubnat2$subnat$name[1:100])
plot_posterior_summaries(res = res_localsubnat1, res2 = res_localsubnat2,
                         modelname2 = "local2", modelname1 = "local", hierarchy_select = "subnat",
                         areas_select = res_localsubnat$subnat$name[1:100])
