# survey_issue
reprex of issues found while calculating quantiles and their confidence intervals with the survey package

The "issues" occurring is somewhat loosely defined, not all aspects of what we saw in our analysis with a large sample are reproduced here, but what is definitely seen here is that sometimes the estimated quantiles are not monotonically increasing. What we saw in addition in our large sample is that even when the value of interest is known for all units in a domain, we get confidence intervals that are larger than zero. Some other points such as the estimated quantile value falling outside its confidence interval or getting only one of the values of the confidence limit (the other one being NaN) are possible/plausible from a statistical standpoint, even though it would be easier from an analysis standpoint if they did not occur.

As the issues seems to occur when when the value of interest for (almost) all units in a sample or in one stratum of the sample is known, this is what is reproduced here in a small example in two flavours: without and with stratification.

## Example without stratification
- dummy population with N = n = 10, values 1:10

## Example without stratification
- overall population: n = 20, N = 110
- stratum 1: n = 10, N = 10, everything is known
- stratum 2: n = 10, N = 100

## Parameter choices to estimate the quantiles and their CIs
Estimating the confidence intervals of quantiles is not an easy matter. The `survey` package offers a variety of algorithms both for the estimation of the quantiles themselves as well as for their confidence intervals. The examples here are reproduced with all combinations of the following parameter settings:
- For the estimation of the quantiles themselves, the `qrule` parameter is set to "hf1", "hf7" and "hf8"
- The parameter for the estimation of the confidence interval, `interval.type` is set to "mean", "beta", and "xlogit"

In addition, I also use the previous implementation (was used with the version of survey < 4.1) with the combination of the following parameters:
- `ties = c("discrete", "rounded")`
- `interval_types = c("Wald", "betaWald")`

## Code and results

Code to estimate the quantiles and their confidence intervals and then visualise the results is present in an [R script](quantiles_dummy_sample.R) as well as in a [Quarto document](quantiles_dummy_sample.qmd), which is rendered and can be viewed with results [here](quantiles_dummy_sample.md).

The code environment and the packages used are handled with [renv](https://rstudio.github.io/renv/index.html), the package versions used are recorded/specified in the [renv.lock file](renv.lock).
