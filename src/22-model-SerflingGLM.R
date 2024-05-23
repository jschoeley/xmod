# Count time series prediction based on the
# Serfling style GLM
#
# Jonas Schöley

# for debug purposes
# df = dat
# formula = formula(
#   deaths_observed ~
#     origin_weeks +
#     sin(2*pi*iso_week/52) +
#     cos(2*pi*iso_week/52) +
#     offset(log(personweeks))
# )
# family <- poisson(link = 'log')
# models <- list(
#   formula(
#     deaths_observed ~
#       origin_weeks +
#       sin(2*pi*iso_week/52) +
#       cos(2*pi*iso_week/52) +
#       offset(log(personweeks))
#   ),
#   formula(
#     deaths_observed ~
#       origin_weeks +
#       offset(log(personweeks))
#   )
# )
# col_sample = 'cv_sample'
# col_stratum = 'stratum_id'
# # only fit on part of the year
# weeks_for_training = c(15:26, 36:45)
# col_week = 'iso_week'
# # only fit on part of the available years
# n_years_for_training = 5
# col_year = 'iso_year'
# nsim = 100
# simulate_beta = TRUE
# simulate_y = TRUE

# Model -----------------------------------------------------------

#' Serfling GLM
#'
#' @param df data frame containing the variables in the model.
#' @param models list of formulas passed to glm().
#' @param family family object passed to glm(). poisson() or quasipoisson().
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
#' A Serfling style GLM is fitted over the training data and expected
#' death counts are predicted over the complete input data frame. The
#' training data is indicated by the column <col_sample> and can further
#' be subset by specifying <weeks_for_training> and <n_years_for_training>.
#' All models in <models> are fit separately by stratum and within
#' each stratum the model with the lowest AIC is chosen for prediction.
#' By default, the input <df> is returned with added expected death
#' counts and <nsim> columns holding simulated death counts from the
#' predicted distribution of counts.
#'
#' @return
#' <df> with added column <deaths_predicted> containing the expected
#' death counts, and columns <deaths_sim><1:nsim> containing simulated
#' expected death counts if simulate_y = FALSE or simulated deaths from
#' the predicted outcome distribution if simulate_y = TRUE.
SerflingGLM <- function (
  df, models, family,
  # column names for training/test split and strata
  col_sample, col_stratum,
  # only fit on part of the year
  weeks_for_training = NULL, col_week = NULL,
  # only fit on part of the available years
  n_years_for_training = NULL, col_year = NULL,
  # simulation parameters
  nsim = 100, simulate_beta = TRUE, simulate_y = TRUE
) {
  
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
  for (i in strata) {
    
    ## fit model ##
    
    # stratum subsets of training and prediction data
    df_prd <- df[df[[col_stratum]]==i,]
    df_trn <- df[df[[col_stratum]]==i&idx_fit,]
    
    if (family$family != 'quasipoisson') {
      # fit all the models and extract the one with lowest AIC
      fits <- lapply(models, function (x) {
        glm(formula = x, family = family,
            data = df_trn, method = glm2::glm.fit2)
      })
      k <- which.min(sapply(fits, AIC))
      formula <- models[[k]]
      model <- fits[[k]]
    }
    
    if (family$family == 'quasipoisson') {
      # first fit as poisson to get AIC
      fits <- lapply(models, function (x) {
        glm(formula = x, family = poisson(),
            data = df_trn, method = glm2::glm.fit2)
      })
      # determine model with lowest AIC and refit as quasipoisson
      k <- which.min(sapply(fits, AIC))
      formula <- models[[k]]
      model <- glm(formula, family = family,
                   data = df_trn, method = glm2::glm.fit2)
    }

    ## predict from model ##
    
    # create a design matrix for prediction
    # keep input NAs in design matrix
    # matrix has same number of rows as <df>
    formula_rhs <- update(formula, NULL~.)
    XX_prd <- model.frame(formula_rhs, df_prd, na.action = na.pass)
    X_prd <- model.matrix(formula_rhs, XX_prd)
    # ensure that the prediction matrix only includes factor level
    # interactions as seen during fit
    # this avoids errors when a factor has a new level in the prediction
    # data set and there's no coefficient estimated for that model;
    # prediction for novel levels will be the same as prediction for
    # reference level
    admissible_terms <- colnames(model.matrix(model))
    X_prd <- X_prd[,admissible_terms]
    
    # offset will be added to linear predictor, add 0 if no offset
    x_offset <- model.offset(XX_prd)
    if (is.null(x_offset)) x_offset <- 0
    
    # expected death counts
    ILink <- model$family$linkinv
    Ey <- ILink(X_prd %*% coef(model) + x_offset)
    
    ## simulate model predictions ##
    
    # simulated model coefficients
    if (isTRUE(simulate_beta)) {
      beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
    } else {
      beta_sim <- matrix(rep(coef(model), nsim), nrow = nsim, byrow = TRUE) 
    }
    # simulated expectations of the outcome distribution
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) ILink(X_prd%*%b + x_offset))
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
      # same rate and dispersion (NBI)
      phi <- summary(model)$dispersion
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))
      }
      # just return the expectation if outcome simulation is FALSE
      if (!isTRUE(simulate_y)) {
        y <- Ey
      }
      return(y)
    })
    colnames_y_sim <- paste0('simulated', 1:nsim)
    
    # add predictions and simulations to input data
    df[df_prd[['.rowid']], 'predicted'] <- Ey
    df[df_prd[['.rowid']], colnames_y_sim] <- y_sim
    
  }
  
  df[,'.rowid'] <- NULL
  return(df)
  
}
