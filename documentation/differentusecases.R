
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

quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}
# for offsetting plots
dodge <- position_dodge(width=0.5)

# national data
dat <- read_csv(here::here(data_folder, "coverage_data.csv"))
dat_subnat <- read_csv(here::here(data_folder, "coverage_data_subnat.csv"))


### overview
# use case 1: global then local national, all countries (in quarto too)
# use case 2: global then local national, 1 country
# use case 3: global then subnational global then local subnational


### use case 1: global then local national, all countries (in quarto too)
hierarchical_level <- c("intercept",  "subcluster", "iso")
fit1a <- fit_model_simplified(runstep = "step1a",
                              hierarchical_level     =  hierarchical_level,
                              survey_df = dat,
                              chains = 4)
fit1a$post_summ <- get_posterior_summaries_simplified(fit1a) %>%
  dplyr::rename(median = mean)
fit1a$post_summ
fit_local <- fit_model_simplified(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat,
                                  chains = 4)
fit <- fit1a
mu <- list()
for(subhierarchy in fit$hierarchical_level) {
  mu[[subhierarchy]] <-
    extract_parameter_subhierarchical (
      hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
      subhierarchy = subhierarchy,
      parname = "mu",
      fit_samples = fit$samples)
}
res_global <- map(mu, function(tibble_samples)
  tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))
fit <- fit_local
mu <- list()
for(subhierarchy in fit$hierarchical_level) {
  mu[[subhierarchy]] <-
    extract_parameter_subhierarchical (
      hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
      subhierarchy = subhierarchy,
      parname = "mu",
      fit_samples = fit$samples)
}
res_local <- map(mu, function(tibble_samples)
  tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))
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
fit_local2 <- fit_model_simplified(runstep = "local_national",
                                  global_fit = fit1a,
                                  survey_df = dat %>% filter(iso == iso_select),
                                  chains = 4)

fit <- fit_local2
mu <- list()
for(subhierarchy in fit$hierarchical_level) {
  mu[[subhierarchy]] <-
    extract_parameter_subhierarchical (
      hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
      subhierarchy = subhierarchy,
      parname = "mu",
      fit_samples = fit$samples)
}
res_local <- map(mu, function(tibble_samples)
  tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))

# plot
bind_rows(res_local$subcluster %>% mutate(model = "local"),
          res_global$subcluster %>% mutate(model = "global") %>% filter(name %in% res_local$subcluster$name)) %>%
  group_by(name, model) %>%
  reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
  ggplot(aes(y = y, x = name, color = model)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_point()

bind_rows(res_local$iso %>% mutate(model = "local"),
          res_global$iso %>% mutate(model = "global")%>% filter(name %in% res_local$iso$name)) %>%
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
fit_globalsubnat$post_summ <- get_posterior_summaries_simplified(fit_globalsubnat) %>%
  dplyr::rename(median = mean)
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
fit <- fit_globalsubnat
mu <- list()
for(subhierarchy in fit$hierarchical_level) {
  mu[[subhierarchy]] <-
    extract_parameter_subhierarchical (
      hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
      subhierarchy = subhierarchy,
      parname = "mu",
      fit_samples = fit$samples)
}
res_globalsubnat <- map(mu, function(tibble_samples)
  tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))

# results local subnat
fit <- fit_localsubnat
mu <- list()
for(subhierarchy in fit$hierarchical_level) {
  mu[[subhierarchy]] <-
    extract_parameter_subhierarchical (
      hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
      subhierarchy = subhierarchy,
      parname = "mu",
      fit_samples = fit$samples)
}
res_localsubnat <- map(mu, function(tibble_samples)
  tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))

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
