devtools::load_all(here::here())
fit1a <- fit_model_simplified(runstep = "step1a",
                              hierarchical_level     = c("intercept",  "subcluster", "iso"),

                              survey_df = dat,
                              chains = 4)

# functions to clean out
## check_nas
## hier data helpers


