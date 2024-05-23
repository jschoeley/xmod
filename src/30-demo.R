# Model Testbench

# Init ------------------------------------------------------------

library(tidyverse)

source('src/10-excess_count_function_collection.R')

# Exponential smoothing count prediction
source('src/23-model-CountES.R')
# INLA Poisson count prediction
source('src/21-model-CountINLA.R')
# Generalized Additive Model for counts prediction
source('src/20-model-CountGAM.R')
# Serfling GLM
source('src/22-model-SerflingGLM.R')

# Functions -------------------------------------------------------

MIS <- function (observed, lower, upper, alpha, na.rm = FALSE) {
  N = length(observed)
  below_lower = observed < lower
  above_upper = observed > upper
  interval_width = upper - lower
  
  mis <- sum(
    interval_width +
      2/alpha*(lower-observed)*below_lower +
      2/alpha*(observed-upper)*above_upper,
    na.rm = na.rm
  ) / N
  
  return(mis)
}

Coverage <- function (observed, lower, upper, na.rm = FALSE) {
  N = length(observed)
  below_upper = observed < upper
  above_lower = observed > lower
  within_interval = below_upper & above_lower
  
  cov <- sum(within_interval, na.rm = na.rm) / N
  
  return(cov)
}

# Weekly death counts ---------------------------------------------

mocy_cv <- readRDS('dat/weekly_mortality_cv.rds')
dat <- filter(mocy_cv, region_iso == 'DE', cv_id == 0, sex != 'Total')

# data with some challenging attributes
dat2 <-
  dat %>%
  mutate(
    # new factor levels in test set
    holiday = as.character(holiday),
    holiday = ifelse(cv_sample == 'test' & iso_week == 10, 'test holiday', holiday),
    holiday = fct_relevel(factor(holiday), 'none'),
    # NA's in predictors
    #iso_week = ifelse(cv_sample == 'test' & iso_week == 12, NA, iso_week),
    #epi_week = ifelse(cv_sample == 'test' & iso_week == 12, NA, epi_week)
  )

# Fit -------------------------------------------------------------

expected_counts <- list()

# Exponential smoothing
expected_counts$exsm <- CountES(
  dat, col_sample = cv_sample, col_counts = observed,
  col_date = date, col_stratum = stratum_id,
  lags = c(1,1,52), loss = 'likelihood', nsim = 100
)

# INLA Bayesian Autoregressive Poisson
expected_counts$inla <- CountINLA(
  dat,
  formula1 =
    death ~ 1 + global_slope + holiday +
    f(time_ar,
      model = 'ar', order = 2,
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    f(time_seas,
      model = 'seasonal', season.length = 52,
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    # effect of temperature anomaly varies in a cyclic fashion
    # over the week of a year
    f(week_rw, tanomaly,
      model = 'rw2', cyclic = TRUE,
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    # independent remaining errors
    f(resid_iid,
      model = 'iid',
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    offset(log(exposure)),
  formula2 =
    death ~ 1 + global_slope +
    f(time_ar,
      model = 'ar', order = 2,
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    f(time_seas,
      model = 'seasonal', season.length = 52,
      hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
    ) +
    offset(log(exposure)),
  formula2_threshold = 100,
  col_stratum = 'stratum_id', col_sample = 'cv_sample',
  col_death = 'observed', col_exposure = 'exposure',
  col_tanomaly = 'tanomaly', col_week = 'iso_week',
  col_time = 'origin_weeks', col_holiday = 'holiday3',
  # number of draws from posterior predictive distribution
  nsim = 100,
  # specify weeks used for training the model
  weeks_for_training_within_year = NULL,
  weeks_for_training_pre_test = NULL,
  threads = 8
)

# Count GAM
expected_counts$cgam <- CountGAM(
  df = dat,
  formula = formula(
    observed ~
      # log linear long term trend
      origin_weeks +
      # penalized cyclic spline for seasonality
      s(epi_week, bs = 'cp', fx = FALSE) +
      # temperature effect
      s(epi_week, bs = 'cp', k = 12, fx = FALSE, by = tanomaly) +
      # adjustment for special weeks
      s(holiday, bs = 're') +
      # exposures
      offset(log(exposure))
  ),
  col_sample = 'cv_sample',
  col_stratum = 'stratum_id',
  col_week = 'epi_week',
  #weeks_for_training = c(0:20, 40:51),
  family = nb(link = 'log'),
  simulate_beta = TRUE,
  simulate_y = TRUE
)

# Serfling GLM
expected_counts$srfl <-
  SerflingGLM(
    df = dat,
    models <- list(
      formula(
        observed ~
          origin_weeks +
          sin(2*pi*iso_week/52) +
          cos(2*pi*iso_week/52) +
          sin(2*pi*iso_week/26) +
          cos(2*pi*iso_week/26) +
          offset(log(exposure))
      ),
      formula(
        observed ~
          origin_weeks +
          sin(2*pi*iso_week/52) +
          cos(2*pi*iso_week/52) +
          holiday +
          offset(log(exposure))
      ),
      formula(
        observed ~
          origin_weeks +
          holiday +
          offset(log(exposure))
      )
    ),
    family = quasipoisson(link = 'log'),
    col_stratum = 'stratum_id',
    col_sample = 'cv_sample',
    col_week = 'iso_week',
    col_year = 'iso_year', n_years_for_training = NULL,
    nsim = 100, simulate_beta = TRUE, simulate_y = TRUE
  )

# Derive prediction intervals -------------------------------------

expected_counts <- map(expected_counts, ~{
    rowwise(.x) %>%
    mutate(
      q025 = quantile(c_across(starts_with('simulated')), 0.025),
      q975 = quantile(c_across(starts_with('simulated')), 0.975)
    )
})

# Plot ------------------------------------------------------------

fig <- list()

fig <- lmap(expected_counts, ~{
  the_plot <- PlotObservedVsExpectedDeaths(
    .x[[1]],
    date = date, expected_deaths = predicted,
    expected_deaths_hi = q975, expected_deaths_lo = q025,
    observed_deaths = observed,
    training = cv_sample,
    facet_row = age_group,
    facet_col = sex
  )
  return(set_names(list(the_plot), names(.x)))
})

# Cross validation test -------------------------------------------

expected_counts_long <-
  bind_rows(expected_counts, .id = 'model')

expected_counts_long %>%
  ungroup() %>%
  filter(cv_sample == 'test', !is.na(predicted)) %>%
  group_by(model) %>%
  summarise(
    # mean prediction error
    mpe = mean((observed-predicted)/observed),
    # mean prediction error averaged over predictions
    # weighted by magnitude of prediction
    wmpe = sum((observed-predicted))/sum(observed),
    # mean absolute prediction error averaged over predictions
    mape = mean(abs(observed-predicted)/observed),
    # mean absolute prediction error averaged over predictions
    # weighted by magnitude of prediction
    wmape = sum(abs(observed-predicted))/sum(observed),
    # actual coverage of prediction interval
    cov = Coverage(observed, q025, q975),
    # interval score
    mis = MIS(observed, q025, q975, 0.05)
  )
