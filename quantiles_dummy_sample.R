# Preparation -----------------------------------------------
library(survey)
library(dplyr)
library(ggplot2)
library(patchwork)


# check survey version
packageVersion("survey")


# define quantiles to be estimated, some not-quite-straightforward numbers as
# some examples only pop up with special cases
quantiles <- c(0.01, 0.05, 0.1, 0.15, seq(21, 81, 10)*0.01, 0.85, 0.9, 0.95, 0.99)

# Function definitions ---------------------------------------------

# function to reshape output from svyquantile
quant_to_df <- function(res, quantile_levels){
  res_df <- data.frame(quantile = res$x[, "quantile"],
                       lwr = res$x[, "ci.2.5"],
                       upr = res$x[, "ci.97.5"],
                       se = res$x[, "se"],
                       level = quantile_levels)
 return(res_df)
}

# function to reshape output from oldsvyquantile
quant_to_df_old <- function(res, quantile_levels){
  res_df <- data.frame(quantile = t(res$quantiles)[,"x"],
                       lwr = confint(res)[, 1],
                       upr = confint(res)[, 2],
                       level = quantile_levels)
 return(res_df)
}

# function to plot the quantiles and their CIs
plot_quantile_ci <- function(data, title) {
  data |>
    mutate(na_flag = case_when((is.na(lwr) & is.na(upr)) == TRUE ~ 2,
                               (is.na(lwr) | is.na(upr)) == TRUE ~ 1,
                               .default = 0)) |>
    ggplot(aes(x = level)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#D6D6D6") +
    geom_line(aes(y = quantile)) +
    geom_point(aes(y = quantile, color = factor(na_flag))) +
    labs(title = title,
         x = "quantile level",
         y = "estimated quantile",
         color = "NAs in CI")
}

# function to plot the proportion of values below the estimated quantile
plot_prop_quantile <- function(quantile_levels, proportion){
  tibble(levels = quantile_levels, prop = proportion * 100) %>%
  ggplot(aes(x = levels, y = prop)) +
    geom_line() +
    geom_point() +
    labs(x = "quantile level", y = "estimated proportion with svymean") +
    theme(aspect.ratio = 1)
}

# function to loop over different combinations of qrule and interval.type
quantile_plot_loop <- function(design, quantiles, qrules = c("hf1", "hf7", "hf8"),
                               interval_types = c("mean", "beta", "xlogit")){

  for (qrule in qrules){
    for (interval_type in interval_types){
      res <- svyquantile(~x, design, quantiles, ci = TRUE,
            interval.type = interval_type, qrule = qrule)

      quant_df <- quant_to_df(res, quantiles)
      # plot quantiles and ci
      p_quant_ci <- plot_quantile_ci(quant_df,
                                title = "")
      # calculate and plot proportions at the estimated quantiles
      prop <- purrr::map(quant_df$quantile, .f = \(z) survey::svymean(~ x <= z, design)) %>%
        purrr::map_dbl(2)
      p_prop <- plot_prop_quantile(quant_df$level, prop)

      print(p_quant_ci + p_prop + plot_annotation(paste(interval_type, "and", qrule)))

    }
  }
}

# similar loop function but for oldsvyquantile
old_quantile_plot_loop <- function(design, quantiles, ties = c("discrete", "rounded"),
                               interval_types = c("Wald", "betaWald")){

  for (tie in ties){
    for (interval_type in interval_types){
      res <- oldsvyquantile(~x, design, quantiles, ci = TRUE,
            interval.type = interval_type, tie = tie)

      quant_df <- quant_to_df_old(res, quantiles)
      # plot quantiles and ci
      p_quant_ci <- plot_quantile_ci(quant_df,
                                title = "")
      # calculate and plot proportions at the estimated quantiles
      prop <- purrr::map(quant_df$quantile, .f = \(z) survey::svymean(~ x <= z, design)) %>%
        purrr::map_dbl(2)
      p_prop <- plot_prop_quantile(quant_df$level, prop)

      print(p_quant_ci + p_prop + plot_annotation(paste(interval_type, "and", tie)))

    }
  }
}

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

quantile_plot_loop(design_no_strat, quantiles)

# loop over different options oldvyquantile

old_quantile_plot_loop(design_no_strat, quantiles)

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

quantile_plot_loop(design_strat, quantiles)

# loop over different options oldsvyquantile

old_quantile_plot_loop(design_strat, quantiles)

