
# different use cases
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
library(bayesplot) # diagnostic plots
devtools::load_all(here::here())
data_folder <- "data_raw"


# for offsetting plots
dodge <- position_dodge(width=0.5)

# national data
dat <- read_csv(here::here(data_folder, "coverage_data.csv"))
dat_subnat <- read_csv(here::here(data_folder, "coverage_data_subnat.csv"))


### overview
# use case 1: global then local national, all countries (in quarto too)
# use case 2: global then local national, 1 country
# use case 3: global then subnational global then local subnational
# use case 4 needed? invariance to re-ordering of input data in use case 3
# use case 5: use case 3 for multiparam

### use case 1: global then local national, all countries (in quarto too)
hierarchical_level <- c("intercept",  "subcluster", "iso")
devtools::load_all(here::here())
fit1a <- fit_model_simplified(runstep = "step1a",
                              hierarchical_level     =  hierarchical_level,
                              survey_df = dat,
                              chains = 4)
fit1a$post_summ <- get_posterior_summaries(fit1a)
fit1a$post_summ
fit_local <- fit_model_simplified(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat,
                                  chains = 4)

res_global <- posterior_summary_hierparam(fit = fit1a, parname = "mu")
res_local <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

# plot
bind_rows(res_local$subcluster %>% mutate(model = "local"),
          res_global$subcluster %>% mutate(model = "global")) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_local$iso %>% mutate(model = "local"),
          res_global$iso %>% mutate(model = "global")) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

### use case 2: global then local national, 1 country
iso_select <- "BFA"
fit1a$samples <- NULL
devtools::load_all(here::here())
fit_local2 <- fit_model_simplified(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat %>% filter(iso == iso_select),
                                  chains = 4)
# equivalent is using in area_select
fit_local2 <- fit_model_simplified(runstep = "local_national",
                                   global_fit = fit1a,
                                   area_select = c(iso_select,"NER", "PER"),
                                   survey_df = dat,
                                   chains = 4)


res_local2 <- posterior_summary_hierparam(fit = fit_local2, parname = "mu")
res_local2$iso
# plot
bind_rows(res_local2$subcluster %>% mutate(model = "local"),
          res_global$subcluster %>% mutate(model = "global") %>% filter(name %in% res_local2$subcluster$name)) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_local2$iso %>% mutate(model = "local"),
          res_global$iso %>% mutate(model = "global")%>% filter(name %in% res_local2$iso$name)) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()


### use case 3: global then subnational global then local subnational
#dat_subnat
devtools::load_all(here::here())
fit_globalsubnat <- fit_model_simplified(runstep = "global_subnational",
                                         area = "subnat",
                              survey_df = dat_subnat,
                              global_fit = fit1a,
                              chains = 4)
fit_globalsubnat$post_summ <- get_posterior_summaries(fit_globalsubnat)
fit_globalsubnat$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit1a$post_summ %>%
  filter(variable_no_index == "mu_sigma")
fit_local <- fit_model_simplified(runstep = "local_subnational",
                                  area = "subnat",
                                  global_fit = fit_globalsubnat,
                                  survey_df = dat_subnat,
                                  chains = 4)


# results global subnat
res_globalsubnat <- posterior_summary_hierparam(fit = fit_globalsubnat, parname = "mu")
res_localsubnat <- posterior_summary_hierparam(fit = fit_local, parname = "mu")

# plots: compare global subnat to local subnat
# compare global subnat to global national up to level that was fixed

#compare global subnat to local subnat
bind_rows(res_localsubnat$subcluster %>% mutate(model = "local"),
          res_globalsubnat$subcluster %>% mutate(model = "global") ) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model, shape = model)) +
# confusing when both the same
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_localsubnat$iso %>% mutate(model = "local"),
          res_globalsubnat$iso %>% mutate(model = "global")) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()


bind_rows(res_localsubnat$subnat %>% mutate(model = "local"),
          res_globalsubnat$subnat %>% mutate(model = "global")) %>%
  arrange(name) %>%
  slice(1:(3*2*30)) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position=dodge) +
  geom_point(position=dodge)



# compare global subnat to global national up to level that was fixed
bind_rows(res_globalsubnat$subcluster %>% mutate(model = "global subnat"),
          res_global$subcluster %>% mutate(model = "global national") %>% filter(name %in% res_globalsubnat$subcluster$name)) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

### use case 4 needed? invariance to re-ordering of input data in use case 3


### use case 5: use case 3 for multiparam
# for model fitting, call other stan model and add argument
# for summaries, need to use morethan1param = TRUE

fit1a_mult <- fit_model_simplified(runstep = "step1a",
                              stan_file_path = file.path(here::here("inst/stan/", "hierfunctions_seq_2param.stan")),
                              mu2param = TRUE,
                              hierarchical_level     =  hierarchical_level,
                              survey_df = dat,
                              chains = 4)
fit1a_mult$post_summ <- get_posterior_summaries(fit1a_mult)
fit1a_mult$post_summ %>% filter(variable %in% c("mu_raw[1,1]", "mu_raw[1,2]", "mu_raw[2,1]", "mu_raw[2,2]"))

res_mult <-  posterior_summary_hierparam(fit = fit1a_mult, parname = "mu", morethan1param = TRUE)


# global subnat mult param
fit_subnational_mult <- fit_model_simplified(runstep = "global_subnational",
                                   stan_file_path = file.path(here::here("inst/stan/", "hierfunctions_seq_2param.stan")),
                                   mu2param = TRUE,
                                   hierarchical_level     =  hierarchical_level,
                                   survey_df = dat_subnat,
                                   area = "subnat",
                                   global_fit = fit1a_mult,
                                   chains = 4)
fit_subnational_mult$post_summ <- get_posterior_summaries(fit_subnational_mult)
res_subnational_mult <- posterior_summary_hierparam(fit = fit_subnational_mult, parname = "mu", morethan1param = TRUE)

# local subnat mult param
# all region and 1-region
fit_local_subnat_mult <- fit_model_simplified(runstep = "local_subnational",
                                   stan_file_path = file.path(here::here("inst/stan/", "hierfunctions_seq_2param.stan")),
                                   mu2param = TRUE,
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
bind_rows(res_subnational_mult$subcluster %>% mutate(model = "local"),
          res_mult$subcluster %>% mutate(model = "global") %>% filter(name %in% res_subnational_mult$subcluster$name)) %>%
  group_by(name, model, k) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_local_subnat_mult$subcluster %>% mutate(model = "local"),
          res_subnational_mult$subcluster %>% mutate(model = "global") %>% filter(name %in% res_local_subnat_mult$subcluster$name)) %>%
  group_by(name, model, k) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_local_subnat_mult$iso %>% mutate(model = "local"),
          res_subnational_mult$iso %>% mutate(model = "global") %>% filter(name %in% res_local_subnat_mult$iso$name)) %>%
  group_by(name, model, k) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()


bind_rows(res_local_subnat_mult$subnat %>% mutate(model = "local"),
          res_subnational_mult$subnat %>% mutate(model = "global") %>% filter(name %in% res_local_subnat_mult$subnat$name)) %>%
  group_by(name, model, k) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

