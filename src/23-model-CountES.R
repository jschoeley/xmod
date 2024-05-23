# Count time series prediction based on the
# Exponential Smoothing Model
#
# Jonas Schöley

# for debug purposes
# df=dat
# model='AAA'
# lags=c(1,1,52)
# loss='likelihoo'
# persistence = NULL
# initial = 'optimal'
# # column names for count variable, training/test split, and strata
# col_sample = quo(cv_sample)
# col_counts = quo(deaths_observed)
# col_date = quo(date)
# col_stratum = quo(stratum_id)
# # simulation parameters
# nsim = 100

# Model -----------------------------------------------------------

# very rough version of exponential smoothing model
# assumes NAs only at beginning of training series, not within
# does not test for it
CountES <- function (
    df, lags,
    persistence = NULL, initial = 'optimal', loss = 'likelihood',
    # column names for count variable, training/test split, and strata
    col_sample, col_counts, col_date, col_stratum,
    # simulation parameters
    nsim = 100, fudge = 0.1
) {
  
  require(smooth); require(dplyr)
  
  .stratum <- enquo(col_stratum)
  .sample <- enquo(col_sample)
  .counts <- enquo(col_counts)
  .date <- enquo(col_date)
  
  # add temporary row id so that we can merge the predictions
  # back into the input data
  df$temporary_row_id <- 1:nrow(df)
  
  # names for columns holding predicted counts
  colnames_y_sim <- paste0('simulated', 1:nsim)
  
  predictions <-
    df %>%
    group_by(!!.stratum) %>%
    arrange(!!.date, .by_group = TRUE) %>%
    group_modify(~{
      
      counts_ts_train <-
        .x %>%
        drop_na(!!.counts) %>%
        filter(!!.sample == 'training') %>%
        pull(var = !!.counts)
      counts_ts_train <- log(counts_ts_train+fudge)
      
      # length of prediction data
      n_pred <- nrow(.x)
      # length of test data
      n_test <- nrow(filter(.x, !!.sample == 'test'))
      # length of training data
      n_train <- n_pred - n_test
      # length of no-na time series training data
      n_train_no_na <- length(counts_ts_train)
      # length of na time series training data
      n_train_na <- n_train - n_train_no_na
      # length of prediction time series without na
      n_pred_no_na <- n_train_no_na + n_test
      
      # fit model
      es_fit <- adam(
        counts_ts_train, model = 'AAA', loss = loss,
        h = n_test, lags = lags, persistence = persistence,
        initial = initial, silent = TRUE, interval = 'none',
        distribution = 'dnorm'
      )
      
      # get time specific variance of forecast
      variance_forecast <-
        diag(multicov(object = es_fit, type = 'analytical'))
      variance_train_no_na <- rep(0, n_train_no_na)
      variance_pred_no_na <- c(variance_train_no_na, variance_forecast)
      
      # add dummy predictions for now
      # names for columns holding predicted counts
      start_of_series_na <- rep(NA, n_train_na)
      E_log1p_y_fitted_no_na <- es_fit$fitted
      E_log1p_y_forecast <- es_fit$forecast
      E_y_pred <-
        exp(c(rep(NA, n_train_na), E_log1p_y_fitted_no_na, E_log1p_y_forecast))
      E_log1p_y_no_na <- c(E_log1p_y_fitted_no_na, E_log1p_y_forecast)
      E_log1p_y_sim_no_na <-
        matrix(E_log1p_y_no_na, nrow = n_pred_no_na, ncol = nsim)
      y_sim <- apply(E_log1p_y_sim_no_na, 2, function (E_link_y) {
        E_link_y_sim <-
          rnorm(n = n_pred_no_na, mean = E_link_y, sd = sqrt(variance_pred_no_na))
        E_count_y_sim <- exp(E_link_y_sim)
        y_no_na <- rpois(n = n_pred_no_na, lambda = E_count_y_sim)
        y <- c(rep(NA, n_train_na), y_no_na)
        return(y)
      })
      colnames(y_sim) <- colnames_y_sim
      
      
      tibble(
        temporary_row_id = .x$temporary_row_id,
        predicted = E_y_pred
      ) %>%
        bind_cols(
          as_tibble(y_sim)
        )
      
    }) %>%
    ungroup() %>%
    select(-!!.stratum)
  
  df <-
    left_join(df, predictions, by = 'temporary_row_id') %>%
    select(-temporary_row_id)
  
  return(df)
  
}