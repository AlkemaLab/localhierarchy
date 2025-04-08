
# different use cases
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
library(truncnorm) # for inits
library(bayesplot) # diagnostic plots
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
# use case 4 needed? invariance to re-ordering of input data in use case 3
# use case 5: use case 3 for multiparam

### use case 1: global then local national, all countries (in quarto too)

devtools::load_all(here::here())
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

res_global <- posterior_summary_hierparam(fit = fit1a, parname = "mu")
res_local <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

# plot
#names(res_global)
devtools::load_all(here::here())
p <- plot_posterior_summaries(res = res_global)
p
plot_posterior_summaries(res = res_global, hierarchy_select = "iso", areas_select = res_global$iso$name[1:100])


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



### use case 5: use case 3 for multiparam
# for model fitting, call other stan model and add argument
# for summaries, need to use morethan1param = TRUE
devtools::load_all(here::here())
fit1a_mult <- fit_model_localhierarchy(runstep = "step1a",
                              mu_isvector = TRUE,
                              hierarchical_level     =  hierarchical_level,
                              survey_df = dat,
                              chains = 4)
fit1a_mult$post_summ <- get_posterior_summaries(fit1a_mult)
fit1a_mult$post_summ %>% filter(variable %in% c("mu_raw[1,1]", "mu_raw[1,2]", "mu_raw[2,1]", "mu_raw[2,2]"))

res_mult <-  posterior_summary_hierparam(fit = fit1a_mult, parname = "mu", morethan1param = TRUE)


# global subnat mult param
fit_subnational_mult <- fit_model_localhierarchy(runstep = "global_subnational",
                                             mu_isvector = TRUE,
                                   hierarchical_level     =  hierarchical_level,
                                   survey_df = dat_subnat,
                                   area = "subnat",
                                   global_fit = fit1a_mult,
                                   chains = 4)
fit_subnational_mult$post_summ <- get_posterior_summaries(fit_subnational_mult)
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

res_local_subnat_mult <- posterior_summary_hierparam(fit = fit_subnational_mult, parname = "mu", morethan1param = TRUE)

# plots
#res_mult
#res_subnational_mult
#res_local_subnat_mult
names(res_subnational_mult)
res_subnational_mult[["subnat"]]
devtools::load_all(here::here())
names(res_subnational_mult[[1]])

p <- plot_posterior_summaries(res = res_subnational_mult, res2 = res_mult,
                         modelname2 = "global", modelname1 = "local")

p
plot_posterior_summaries(res = res_local_subnat_mult, res2 = res_subnational_mult,
                        hierarchy_select = "subnat",
                         areas_select = res_subnational_mult$subnat$name[1:100])

p <- plot_posterior_summaries(res = res_subnational_mult, res2 = res_mult)
p

