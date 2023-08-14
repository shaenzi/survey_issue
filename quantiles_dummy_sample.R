# Preparation -----------------------------------------------
library(survey)
library(dplyr)
library(ggplot2)
library(patchwork)


# check survey version
packageVersion("survey")

# source functions for estimation and plotting
source("functions_for_estimation_and_plotting.R")


# define quantiles to be estimated, some not-quite-straightforward numbers as
# some examples only pop up with special cases
quantiles <- c(0.01, 0.05, 0.1, 0.15, seq(21, 81, 10)*0.01, 0.85, 0.9, 0.95, 0.99)

# Dummy data no stratification -------------------------------------
# n = N = 10

data_no_strat <- data.frame(x = 1:10, id = 1:10, fpc = 10, probs = 1)

design_no_strat <- svydesign(id = ~id,
                             probs = ~probs,
                             data = data_no_strat,
                             fpc = ~fpc)



res <- svyquantile(~x, design_no_strat, quantiles, ci = TRUE,
            interval.type = "mean", qrule = "hf1")



quant_df <- quant_to_df(res, quantiles)
plot_quantile_ci(quant_df, "mean and hf1")



a <- purrr::map(quant_df$quantile, .f = \(z) survey::svymean(~ x <= z, design_no_strat)) %>%
  purrr::map_dbl(2)

plot_prop_quantile(quant_df$level, a)

# loop over different options newsvyquantile

quantile_plot_loop(design_no_strat, quantiles,
                   qrules = c("hf1", "hf7", "hf8"),
                   interval_types = c("mean", "beta", "xlogit"))
# loop over different options oldvyquantile

old_quantile_plot_loop(design_no_strat, quantiles,
                       ties = c("discrete", "rounded"),
                       interval_types = c("Wald", "betaWald"))

# Dummy data with stratification --------------------------------------------

data_strat <- data.frame(x = c(1:10, 1:10), id = 1:20,
                         strata =  c(rep(1, 10), rep(2,10)),
                         fpc = c(rep(10, 10), rep(100, 10)),
                         probs = c(rep(1, 10),rep(0.1,10)))

design_strat <- svydesign(id = ~id,
                          strata = ~strata,
                          probs = ~probs,
                          data = data_strat,
                          fpc = ~fpc)



res <- svyquantile(~x, design_strat, quantiles, ci = TRUE,
            interval.type = "mean", qrule = "hf1")

res



quant_df <- quant_to_df(res, quantiles)
plot_quantile_ci(quant_df, "mean and hf1")

# loop over different options newsvyquantile

quantile_plot_loop(design_strat, quantiles,
                   qrules = c("hf1", "hf7", "hf8"),
                   interval_types = c("mean", "beta", "xlogit"))

# loop over different options oldsvyquantile

old_quantile_plot_loop(design_strat, quantiles,
                       ties = c("discrete", "rounded"),
                       interval_types = c("Wald", "betaWald"))

