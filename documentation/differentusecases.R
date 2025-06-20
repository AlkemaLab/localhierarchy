
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

### overview
# use case 1: global then local national, all countries (in quarto too)
# use case 2: global then local national, 1 country
# use case 3: global then subnational global then local subnational
### extended to also be able to produce subnational estimates for countries that were
# included in global national run but not in global subnational run
# use case 4 needed? invariance to re-ordering of input data in use case 3

### use case 1: global then local national, all countries (in quarto too)

fit1a <- fit_model_localhierarchy(runstep = "step1a",
                              hierarchical_level     =  hierarchical_level,
                              survey_df = dat,
                              chains = 4)
fit1a$post_summ <- get_posterior_summaries(fit1a)
fit1a$post_summ
fit_local <- fit_model_localhierarchy(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat,
                                  chains = 4)

# check the mu_raws
devtools::load_all(here::here())
plots <- plot_mu_raw(fit = fit1a, parname = "mu")
# a list with plots
# summary plots gives summaries, 30 at a time
plots[["summary_plots"]][[1]]
# specific plots for one parameter are in plots[["plots_allmuraw"]]
plots[["plots_allmuraw"]][[1]]
plots[["plots_allmuraw"]][grepl("intercept", names(plots[["plots_allmuraw"]]))]
plots[["plots_allmuraw"]][grepl("subcluster", names(plots[["plots_allmuraw"]]))][[1]]

# plot the sigmas
plot_prior_post_sigmas(fit = fit1a, parname = "mu")

plots <- plot_mu_raw(fit = fit_local, parname = "mu")
plots[[1]][[1]]
plots[[2]][[1]]

# the total mus
res_global <- posterior_summary_hierparam(fit = fit1a, parname = "mu")
res_local <- posterior_summary_hierparam(fit = fit_local, parname = "mu")
p <- plot_posterior_summaries(res = res_global)
p
plot_posterior_summaries(res = res_global, hierarchy_select = "iso", areas_select = res_global$iso$name[1:100])

p <- plot_posterior_summaries(res = res_local, res2 = res_global, hierarchy_select = "iso")
p

p <- plot_posterior_summaries(res = res_local, res2 = res_global, hierarchy_select = "subcluster")
p


### use case 2: global then local national, 1 country
iso_select <- "BFA"
fit1a$samples <- NULL
devtools::load_all(here::here())
fit_local2 <- fit_model_localhierarchy(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat %>% filter(iso == iso_select),
                                  chains = 4)
# equivalent is using in area_select
fit_local2 <- fit_model_localhierarchy(runstep = "local_national",
                                   global_fit = fit1a,
                                   area_select = c(iso_select,"NER", "PER"),
                                   survey_df = dat,
                                   chains = 4)


res_local2 <- posterior_summary_hierparam(fit = fit_local2, parname = "mu")
res_local2$iso
# plot
p <- plot_posterior_summaries(res = res_local2, res2 = res_global,
                              modelname2 = "global", modelname1 = "local")#, hierarchy_select = "iso")
p

### use case 3: global then subnational global then local subnational
#dat_subnat
devtools::load_all(here::here())
fit_globalsubnat <- fit_model_localhierarchy(runstep = "global_subnational",
                                         area = "subnat",
                              survey_df = dat_subnat,
                              global_fit = fit1a,
                              chains = 4)

# check the mu_raws
plots <- plot_mu_raw(fit = fit_globalsubnat, parname = "mu")
# a list with plots
# summary plots gives summaries, 30 at a time
plots[["summary_plots"]][[1]]
# specific plots for one parameter are in plots[["plots_allmuraw"]]
plots[["plots_allmuraw"]][[1]]

# plot the sigmas
plot_prior_post_sigmas(fit = fit_globalsubnat, parname = "mu")



# continue
fit_globalsubnat$post_summ <- get_posterior_summaries(fit_globalsubnat)
fit_globalsubnat$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit1a$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit_local <- fit_model_localhierarchy(runstep = "local_subnational",
                                  area = "subnat",
                                  global_fit = fit_globalsubnat,
                                  survey_df = dat_subnat,
                                  chains = 4)


# results global subnat
res_globalsubnat <- posterior_summary_hierparam(fit = fit_globalsubnat, parname = "mu")
res_localsubnat <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

# plots: compare global subnat to local subnat
# compare global subnat to global national up to level that was fixed
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                              modelname2 = "global", modelname1 = "local", hierarchy_select = "subcluster")
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "iso")
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "subnat",
                         areas_select = res_localsubnat$subnat$name[1:100])



# compare global subnat to global national up to level that was fixed
plot_posterior_summaries(res = res_globalsubnat, res2 = res_global,
                         modelname1 = "global", modelname2 = "local", hierarchy_select = "subcluster")
plot_posterior_summaries(res = res_globalsubnat, res2 = res_global,
                         modelname1 = "global", modelname2 = "local", hierarchy_select = "iso")

### use case 4 needed? invariance to re-ordering of input data
fit_local2 <- fit_model_localhierarchy(runstep = "local_subnational",
                                  area = "subnat",
                                  global_fit = fit_globalsubnat,
                                  survey_df = dat_subnat[seq(dim(dat_subnat)[1],1),],
                                  chains = 4)
res_local2 <- posterior_summary_hierparam(fit = fit_local2, parname = "mu")
plot_posterior_summaries(res = res_localsubnat, res2 = res_local2,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "subnat",
                         areas_select = res_localsubnat$subnat$name[1:100])

plot_posterior_summaries(res = res_localsubnat, res2 = res_local2,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "iso")





##### extending use case 3
# after getting
#fit_globalsubnat$post_summ
#fit1a$post_summ

# global subnat just adds mu_sigma for another level
fit_globalsubnat$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit1a$post_summ %>%
  filter(variable_no_index == "mu_sigma")
# goal = keep national estimates from 1a, also for countries w/o subnat data

# approach
# use global fit nat
# add in sigmas by selecting params not in global fit nat, "sigma_"
# and save as globalsubnat_fromnat_fit
# for local subnational, update hierarchical levels (do add one, just like in global subnat)

# parameters to add:
param_add <- anti_join(fit_globalsubnat$post_summ %>%
    # select rows where variable_no_index has sigma in it
    filter(grepl("sigma", variable_no_index)),
    fit1a$post_summ %>%
        filter(grepl("sigma", variable_no_index)))

fit_globalsubnat_fromnat <- fit1a
fit_globalsubnat_fromnat$post_summ <- bind_rows(fit_globalsubnat_fromnat$post_summ, param_add)
devtools::load_all(here::here())
fit_local <- fit_model_localhierarchy(runstep = "local_subnational",
                                      use_globalsubnat_fromnat = TRUE,
                                      area = "subnat",
                                      global_fit = fit_globalsubnat_fromnat,
                                      survey_df = dat_subnat,
                                      chains = 4)


# results global subnat
res_globalsubnat <- posterior_summary_hierparam(fit = fit_globalsubnat, parname = "mu")
res_localsubnat <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

# plots: compare global subnat to local subnat
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "subcluster")
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "iso")
plot_posterior_summaries(res = res_localsubnat, res2 = res_globalsubnat,
                         modelname2 = "global", modelname1 = "local", hierarchy_select = "subnat",
                         areas_select = res_localsubnat$subnat$name[1:100])


# to test
# use a fit1a instead
# should give an error for sigmas
# confirmed
fit_local <- fit_model_localhierarchy(runstep = "local_subnational",
                                      use_globalsubnat_fromnat = TRUE,
                                      area = "subnat",
                                      global_fit = fit1a,
                                      survey_df = dat_subnat,
                                      chains = 4)




