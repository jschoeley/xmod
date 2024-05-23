install.packages(
  c('coda.base', 'MASS', 'mgcv', 'ragg', 'shiny',
    'tidyverse', 'dplyr', 'smooth', 'INLA',
    'danstat', 'ggplot2', 'ISOweek', 'purrr',
    'rlang', 'scales', 'tidyr', 'glm2', 'keras',
    'xgboost'),
  dep = TRUE
)

install.packages(
  'INLA', repos = c(getOption('repos'),
                    INLA = 'https://inla.r-inla-download.org/R/stable'),
  dep = TRUE
)

tensorflow::install_tensorflow()
