# Count time series prediction based on the
# Generalized Additive Model
#
# Jonas Schöley

# for debug purposes
# df <- dat2
# formula <-
#   formula(
#     deaths_observed ~
#       # log linear long term trend
#       origin_weeks +
#       # penalized cyclic spline for seasonality
#       s(epi_week, bs = 'cp', k = 12) +
#       # temperature effect
#       s(epi_week, bs = 'cp', k = 12, by = temperature_anomaly) +
#       # adjustment for special weeks
#       s(holiday, bs = 're') +
#       # exposures
#       offset(log(personweeks))
#   )
# col_sample <- 'cv_sample'
# col_year <- 'iso_year'
# col_stratum <- 'stratum_id'
# n_years_for_training <- 7
# family <- quasipoisson(link = 'log')
# nsim <- 100
# simulate_beta <- TRUE
# simulate_y <- TRUE
# method = 'GCV.Cp'

# Model -----------------------------------------------------------

#' Count Prediction with GAM
#'
#' @param df data frame containing the variables in the model.
#' @param formula formula for gam().
#' @param family family object passed to gam(). poisson(), nb(), or quasipoisson().
#' @param col_sample name of column in <df> indicating training or test
#' data. must have values 'training' or 'test'.
#' @param col_stratum name of column in <df> indicating strata.
#' @param weeks_for_training vector of weeks in training data to be used
#' for training the model. default NULL uses all weeks of the year.
#' @param col_week name of column used for <weeks_for_training> selection.
#' @param n_years_for_training number of years in training data to be used
#' for training. counts backwards from the last year in training data.
#' default NULL uses all years in training.
#' @param col_year name of column used for <n_years_for_training> selection.
#' @param nsim number of simulated predictions.
#' @param simulate_beta should the simulated predictions contain
#' uncertainty around the parameter estimates of the model? (default = TRUE)
#' @param simulate_y should the simulated predictions contain uncertainty
#' around the sampling distribution of the outcome (default = TRUE)
#'
#' @details
#' A GAM is fitted over the training data and expected
#' counts are predicted over the complete input data frame. The
#' training data is indicated by the column <col_sample> and can further
#' be subset by specifying <weeks_for_training> and <n_years_for_training>.
#' By default, the input <df> is returned with added expected
#' counts and <nsim> columns holding simulated counts from the
#' predicted distribution of counts. The model is fitted independently
#' over the strata specified in <col_stratum>.
#'
#' @return
#' <df> with added column <predicted> containing the expected
#' death counts, and columns <simulated><1:nsim> containing simulated
#' expectations if simulate_y = FALSE or simulated counts from
#' the predicted outcome distribution if simulate_y = TRUE.
CountGAM <- function (
  df, formula, family, method = 'GCV.Cp',
  # column names for training/test split and strata
  col_sample, col_stratum,
  # only fit on part of the year
  weeks_for_training = NULL, col_week = NULL,
  # only fit on part of the available years
  n_years_for_training = NULL, col_year = NULL,
  # simulation parameters
  nsim = 100, simulate_beta = TRUE, simulate_y = TRUE
) {
  
  require(mgcv)
  
  df['.rowid'] <- 1:nrow(df)
  
  ## subset input data to rows used for fitting the model ##
  
  # index of rows designated as training data
  idx_train <- df[[col_sample]] == 'training'
  # index of rows with weeks suitable for training
  idx_weeks <- TRUE
  if (!is.null(weeks_for_training)) {
    # only train on these weeks
    idx_weeks <- df[[col_week]] %in% weeks_for_training
  }
  # index of rows with years suitable for training
  idx_years <- TRUE
  if (!is.null(n_years_for_training)) {
    # most recent <n_years> in training data
    years_for_training <- sort(unique(df[idx_train,][[col_year]]),
                               decreasing = TRUE)[1:n_years_for_training]
    # only train on these years
    idx_years <- df[[col_year]] %in% years_for_training
  }
  # index of data used for fitting
  idx_fit <- idx_train & idx_years & idx_weeks
    
  # for each stratum, fit model, predict and simulate from model,
  # add results to df
  strata <- unique(df[[col_stratum]])
  # names for columns holding predicted death counts
  colnames_y_sim <- paste0('simulated', 1:nsim)
  for (i in strata) {

    # stratum subsets of training and prediction data
    idx_stratum <- df[[col_stratum]]==i
    df_prd <- df[idx_stratum,]
    df_trn <- df[idx_stratum&idx_train,]
    # normalized weights used for fitting
    # magnitude of log-likelihood not affected
    # by exclusion of periods for fitting
    include <- idx_fit[idx_stratum&idx_train]
    wgts <- (include)/mean(include)
    df_trn[,'wgts'] <- wgts
    
    ## fit model ##
    
    family_fit <- family
    # in case of quasipoisson, fit poisson and manually extract
    # overdispersion, so that we can get a quasi-likelihood from
    # the poisson fit
    if (identical(family$family, 'quasipoisson')) {family_fit$family <- 'poisson'}
    model <- gam(
      formula = formula, family = family_fit, data = df_trn,
      method = method, weights = wgts
    )
    # https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
    dfun <- function(object) {
      with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
    }
    dispersion <- dfun(model)
    
    ## predict from model ##
    
    # create a design matrix for prediction
    # keep input NAs in design matrix
    X_prd <- predict(model, newdata = df_prd, type = 'lpmatrix')
    # estimated coefficients
    beta <- coef(model)
    # linear predictor over prediction data w/o offset
    eta_prd_without_offset <- X_prd %*% beta
    # linear predictor over prediction data with offset included
    eta_prd_with_offset <- matrix(predict(model, newdata = df_prd, type = 'link'), ncol = 1)
    # I know of no easy way with mgcv to extract the offset over "newdata"
    # therefore this rather strange solution
    # offset over prediction data (may be 0)
    offset_prd <- eta_prd_with_offset - eta_prd_without_offset
    # inverse link function
    ILink <- model$family$linkinv
    # expected death counts
    Ey_prd <- ILink(eta_prd_with_offset)
    
    ## simulate model predictions ##
    
    # simulated model coefficients
    if (isTRUE(simulate_beta)) {
      beta_sim <- MASS::mvrnorm(nsim, beta, vcov(model, freq = FALSE, unconditional = TRUE))
    } else {
      beta_sim <- matrix(rep(beta, nsim), nrow = nsim, byrow = TRUE)
    }
    # simulated expectations of the outcome distribution
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) ILink(X_prd%*%b + offset_prd))
    # simulated outcome
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      # if a simulation algorithm hasn't been defined for a family
      # just return the expectation of the outcome
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu); mu_ <- mu[!idx_na]; N <- length(mu_)
      if (model$family$family == 'poisson') {
        y[!idx_na] <- rpois(n = N, lambda = mu_)      
      }
      if (model$family$family == 'quasipoisson') {
        # https://stats.stackexchange.com/q/157575
        # we estimate the rate and dispersion parameter via quasipoisson
        # and then sample from a Negative Binomial distribution with the
        # same mean and variance
        # For Quasi-Poisson E(Y) = mu and Var(Y) = phi*mu
        # For R's ngbinom E(Y) = mu and Var(Y) = mu+mu^2*1/size
        # Equating both variance expressions and re-arranging yields
        # an expression for the size parameter for the negbinom matching
        # the variance of the quasipoisson
        # phi*mu = mu+mu^2*1/size
        # phi = 1 + mu/size
        # mu / (phi - 1) = size
        phi <- dispersion
        # in case of under-dispersion, sample from Poisson
        if (phi < 1) { phi = 1 }
        y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      }
      if (grepl('^Negative Binomial', model$family$family)) {
        theta <- model$family$getTheta(TRUE)
        y[!idx_na] <- rnbinom(n = N, mu = mu_, size = theta)
      }
      # just return the expectation if outcome simulation is FALSE
      if (!isTRUE(simulate_y)) {
        y <- Ey
      }
      return(y)
    })
    
    # add predictions and simulations to input data
    df[df_prd[['.rowid']], 'predicted'] <- Ey_prd
    df[df_prd[['.rowid']], colnames_y_sim] <- y_sim
    
  }
  
  df[,'.rowid'] <- NULL
  return(df)
  
}
