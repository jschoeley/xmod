# Count time series prediction based on the
# INLA Bayesian GAM
#
# Jonas Schöley
#
# See Kontis 2021 (https://doi.org/10.1038/s41591-020-1112-0) for
# an application.

# for debug purposes
# df_traintest <- dat
# 
# .stratum <- quo(stratum_id)
# .sample <- quo(cv_sample)
# .death <- quo(deaths_observed)
# .exposure <- quo(personweeks)
# .tanomaly <- quo(temperature_anomaly)
# .week <- quo(iso_week)
# .time <- quo(origin_weeks)
# .holiday <- quo(holiday3)
# 
# formula1 <- formula(
#   death ~
#     1 +
#     global_slope +
#     holiday +
#     f(time_ar,
#       model = 'ar', order = 2,
#       hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     ) +
#     # sin(2*pi*week_rw/52) +
#     # cos(2*pi*week_rw/52) +
#     #sin(2*pi*week_rw/26) +
#     #cos(2*pi*week_rw/26) +
#     offset(log(exposure)) +
#     f(time_seas,
#       model = 'seasonal', season.length = 52,
#       hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     ) +
#     # effect of temperature anomaly varies in a cyclic fashion
#     # over the week of a year
#     # f(week_rw, tanomaly,
#     #   model = 'rw2', cyclic = TRUE,
#     #   hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     # )# +
#     # independent remaining errors
#   f(resid_iid,
#     model = 'iid',
#     hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#   )
# )
# 
# formula2 <- formula(
#   death ~
#     1 +
#     #global_slope +
#     #holiday +
#     #tanomaly +
#     f(time_ar,
#       model = 'ar', order = 2,
#       hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     ) +
#     # f(time_seas,
#     #   model = 'seasonal', season.length = 52,
#     #   hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     # ) +
#     # effect of temperature anomaly varies in a cyclic fashion
#     # over the week of a year
#     # f(week_rw, tanomaly,
#     #   model = 'rw2', cyclic = TRUE,
#     #   hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#     # ) +
#     # independent remaining errors
#   f(resid_iid,
#     model = 'iid',
#     hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
#   ) +
#     offset(log(exposure))
# )

# Model -----------------------------------------------------------

#' Count Prediction with INLA
#'
#' @param df data frame containing the variables in the model.
#' @param formula1 formula for inla(). terms must only use the
#' following variable names:
#' death, holiday, tanomaly, exposure, global_slope, time_ar, time_seas,
#' week_rw, resid_iid.
#' @param formula2 alternative formula for inla().
#' @param formula2_threshold integer threshold. if the average weekly
#' death counts in the training data within a stratum fall below this
#' number, then use the alternative formula.
#' @param col_stratum name of column in <df> indicating strata.
#' @param col_sample name of column in <df> indicating training or test
#' data. must have values 'training' or 'test'.
#' @param col_death name of column in <df> indicating death counts.
#' @param col_exposure name of column in <df> indicating exposures.
#' @param col_tanomaly name of column in <df> indicating temperature
#' anomalies.
#' @param col_week name of column in <df> indicating week of year.
#' @param col_time name of column in <df> indicating weeks since start
#' of observation.
#' @param col_holiday name of column in <df> indicating public holidays.
#' @param nsim number of simulated predictions.
#' @param weeks_for_training_within_year vector of weeks within a year
#' in training data to be used for training the model.
#' default NULL uses all weeks of the year.
#' @param weeks_for_training_pre_test number of weeks in training data
#' to be used for training. counts backwards from the most recent week
#' in training data. default NULL uses all weeks in training.
#' @param threads number of threads to use.
#'
#' @details
#' Based upon the implementation in
#' https://doi.org/10.1038/s41591-020-1112-0.
#'
#' @return
#' <df> with added column <deaths_predicted> containing the expected
#' death counts, and columns <deaths_sim><1:nsim> containing simulated
#' death counts from the posterior predictive distribution.
CountINLA <- function (
  df,
  formula1,
  formula2 = NULL,
  formula2_threshold = NULL,
  # variable names
  col_stratum,
  col_sample,
  col_death,
  col_exposure,
  col_tanomaly,
  col_week,
  col_time,
  col_holiday,
  # number of draws from posterior predictive distribution
  nsim = 100,
  # specify weeks used for training the model
  weeks_for_training_within_year = NULL,#c(15:26, 36:45)
  weeks_for_training_pre_test = NULL,#52*5
  threads = 1
) {
  
  require(dplyr)
  require(INLA)
  
  # translate external to internal variables names
  .stratum <- enquo(col_stratum)
  .sample <- enquo(col_sample)
  .death <- enquo(col_death)
  .exposure <- enquo(col_exposure)
  .tanomaly <- enquo(col_tanomaly)
  .week <- enquo(col_week)
  .time <- enquo(col_time)
  .holiday <- enquo(col_holiday)
  
  # add temporary row id so that we can merge the predictions
  # back into the input data
  df$temporary_row_id <- 1:nrow(df)
  
  # prepare data for fit
  ready_for_fit <-
    df %>%
    # select variables of interest
    select(
      temporary_row_id,
      sample = !!.sample,
      stratum = !!.stratum,
      time = !!.time,
      week = !!.week,
      death = !!.death,
      exposure = !!.exposure,
      tanomaly = !!.tanomaly,
      holiday = !!.holiday
    ) %>%
    # if the observation is part of the test-set
    # make the outcome <NA>
    mutate(
      death = ifelse(sample == 'test', NA, death)
    ) %>%
    # add variables needed in model fit
    mutate(
      global_slope = time,
      time_ar = time,
      time_seas = time,
      week_rw = week,
      resid_iid = time
    )

  # exclude weeks within a year from training
  if (!is.null(weeks_for_training_within_year)) {
    ready_for_fit <-
      ready_for_fit %>%
      mutate(death = ifelse(week %in% weeks_for_training_within_year, death, NA))
  }
  
  # only use a certain number of weeks before start of test to train model
  if (!is.null(weeks_for_training_pre_test)) {
    training_weeks <-
      ready_for_fit %>% filter(sample=='training') %>%
      pull(time) %>% unique() %>%
      sort(decreasing = TRUE) %>% `[`(1:weeks_for_training_pre_test)
    ready_for_fit <-
      ready_for_fit %>%
      mutate(death = ifelse(time %in% training_weeks, death, NA))
  }
  
  SamplePosteriorPredictiveDistribution <- function(inla_fit, n) {
    # draws from posterior distribution of parameters
    draws <- inla.posterior.sample(n, inla_fit)
    # length of data series
    data_length <- attr(draws, '.contents')$length[1]
    
    count_samples <- lapply(draws, function(draw) {
      # posterior sample of lambdas
      lambda <- exp(draw$latent[grep('Predictor', rownames(draw$latent))])
      # posterior sample of Poisson counts
      rpois(data_length, lambda)
    })
    return(count_samples)
  }
  
  model <-
    ready_for_fit %>%
    # fit the model separately by stratum
    group_by(stratum) %>%
    group_modify(~{
      
      # decide which formula to use, main or alternative
      # if the average observed deaths are below a threshold, use
      # the alternative formula
      stratum_specific_formula <- formula1
      if (isTRUE(mean(.x[['death']], na.rm = TRUE) < formula2_threshold)) {
        stratum_specific_formula <- formula2
      }
      
      # fit model
      
      # initial fit to get starting values for full fit
      # aides in convergence
      init_fit <- inla(
        formula = stratum_specific_formula,
        family = 'poisson',
        control.predictor = list(link = 1),
        control.compute = list(dic = FALSE, config = TRUE),
        control.inla = list(
          int.strategy = 'eb', strategy = 'gaussian',
          diagonal = 10000
        ),
        num.threads = threads,
        data = .x
      )
      
      # full fit
      the_fit <- inla(
        formula = stratum_specific_formula,
        family = 'poisson',
        control.predictor = list(link = 1),
        control.compute = list(
          dic = FALSE, config = TRUE),
        control.inla = list(diagonal = 0),
        control.mode = list(result = init_fit, restart = TRUE),
        num.threads = threads,
        data = .x
      )
      
      # add mean prediction
      deaths_predicted <- the_fit$summary.fitted.values$mean
      # add N prediction series sampled from posterior predictive distribution
      deaths_sampled <-
        do.call('cbind',
                SamplePosteriorPredictiveDistribution(the_fit, n = nsim))
      colnames(deaths_sampled) <- paste0('simulated', 1:nsim)
      
      data.frame(
        temporary_row_id = .x$temporary_row_id,
        predicted = deaths_predicted,
        deaths_sampled
      )
      
    }) %>%
    ungroup() %>%
    select(-stratum)
  
  df <-
    left_join(df, model, by = 'temporary_row_id') %>%
    select(-temporary_row_id)
  
  return(df)
  
}
