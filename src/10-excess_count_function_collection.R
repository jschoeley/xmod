# Excess deaths function collection
# 
# Jonas Schöley

# Handling of Dates -----------------------------------------------

#' Convert Date to Week of Year
#'
#' @param date Date string.
#' @param format Either 'integer', 'iso', or 'string'.
#'
#' @return Week of year.
#' 
#' @source https://en.wikipedia.org/wiki/ISO_8601
#'
#' @author Jonas Schöley
#'
#' @examples
#' # February 2020 started in the 5th week of the year
#' Date2ISOWeek('2020-02-01', format = 'integer')
#' Date2ISOWeek('2020-02-01', format = 'iso')
#' Date2ISOWeek('2020-02-01', format = 'string')
Date2ISOWeek <- function (date, format = 'integer') {
  require(ISOweek)
  iso_week_of_year <- date2ISOweek(date)
  switch(
    format,
    iso = iso_week_of_year,
    string = substr(iso_week_of_year, 6, 8),
    integer = as.integer(substr(iso_week_of_year, 7, 8)),
  )
}

#' Convert Integer Week of Year to the ISO Week of Year Format
#' 
#' @param week Integer week of year.
#' 
#' @return Week of year in ISO week format.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' Integer2ISOWeek(1:5)
Integer2ISOWeek <- function (week) {
  paste0('W', formatC(week, flag = '0', width = 2))
}

#' Convert Week of Year to Date
#'
#' @param year Year integer.
#' @param week Week of year integer (1 to 53).
#' @param weekday Weekday integer (1, Monday to 7, Sunday).
#' @param offset Integer offset added to `week` before date calculation.
#'
#' @return A date object.
#' 
#' @source https://en.wikipedia.org/wiki/ISO_8601
#'
#' @author Jonas Schöley
#'
#' @examples
#' # the first Week of 2020 actually starts Monday, December 30th 2019
#' ISOWeekDate2Date(2020, 1, 1)
ISOWeekDate2Date <- function (year, week, weekday = 1, offset = 0) {
  require(ISOweek)
  isoweek_string <-
    paste0(
      year, '-W',
      formatC(
        week+offset,
        flag = '0',
        format = 'd',
        digits = 1
      ),
      '-', weekday
    )
  ISOweek2date(isoweek_string)
}

#' Calculate Weeks Since Some Origin Date
#'
#' @param date Date string.
#' @param origin_date Date string.
#' @param week_format Either 'integer' for completed weeks or
#' 'fractional' for completed fractional weeks.
#'
#' @return Time difference in weeks.
#'
#' @author Jonas Schöley
#'
#' @examples
#' # My age in completed weeks
#' WeeksSinceOrigin(Sys.Date(), '1987-07-03')
WeeksSinceOrigin <-
  function(date, origin_date, week_format = "integer") {
    require(ISOweek)
    fractional_weeks_since_origin <-
      as.double(difftime(
        as.Date(date),
        as.Date(origin_date),
        units = "weeks"
      ))
    switch(
      week_format,
      fractional = fractional_weeks_since_origin,
      integer = as.integer(fractional_weeks_since_origin)
    )
  }

#' Convert Weeks Since Origin Date To Date
#'
#' @param origin_date Date string.
#' @param weeks_since_origin Completed weeks since origin date.
#'
#' @return A Date.
#'
#' @author Jonas Schöley
#'
#' @examples
#' date_plus_12_weeks <- WeeksSinceOrigin2Date('2010-01-01', 12)
#' WeeksSinceOrigin(date_plus_12_weeks, '2010-01-01')
WeeksSinceOrigin2Date <- function (origin_date, weeks_since_origin) {
  as.Date(origin_date) + floor(weeks_since_origin*7)
}

#' Count the Number of "Birthdays" Since Some Origin Date
#'
#' @param origin_date Date string.
#' @param current_date Date string.
#'
#' @return
#' The number of birthdays since `origin_date`.
#'
#' @examples
#' # the function handles negative birthdays and is vectorized
#' BirthdaysSinceOrigin(
#'   '1987-07-03',
#'   seq(as.Date('1987-01-01'), as.Date('1990-07-03'), by = 'months')
#' )
BirthdaysSinceOrigin <- function (origin_date, current_date) {
  date1 <- as.Date(origin_date)
  date2 <- as.Date(current_date)
  
  # non vectorized version
  CountBirthdays <- function (t1, t2) {
    if (t2 < t1) {
      birthday_sequence <- seq(t1, t2, by = '-1 year')
      number_of_birthdays <- -(length(birthday_sequence)-1)
    }
    else {
      birthday_sequence <- seq(t1, t2, by = '1 year')
      number_of_birthdays <- (length(birthday_sequence)-1)
    }
    return(number_of_birthdays)
  }
  
  # vectorized
  mapply(CountBirthdays, date1, date2, SIMPLIFY = TRUE)
  
}

#' Return a Sequence of Epi-Year Strings
#' 
#' @param from First year of Epi-Year sequence.
#' @param to Last year of Epi-Year sequence.
#' @param what Type of return value. One of 'slash', 'beginning', 'end'.
#' 
#' @return Sequence of Epi-Years.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' EpiYearSequence(2000, 2005)
#' # beginning of epi-year
#' EpiYearSequence(2000, 2005, 'beginning')
#' # end of epi-year
#' EpiYearSequence(2000, 2005, 'end')
EpiYearSequence <- function(from, to, what = 'slash') {
  years <- from:to
  switch(
    what,
    slash = paste0(head(years, -1), "/", years[-1]),
    beginning = head(years, -1),
    end = years[-1]
  )
}

#' Convert ISO-weeks to Epi-weeks
#' 
#' @param iso_week ISO-week vector.
#' @param w_start ISO-week for which Epi-week is 0.
#' @param w_53 Assume a 53 week year (default = FALSE)? 
#' 
#' @return A vector of Epi-weeks corresponding to the ISO-week input.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' # Epi-week 0 aligned with ISO-week 1
#' IsoWeek2EpiWeek(1:52)
#' # Epi-week 0 aligned with ISO-week 10
#' IsoWeek2EpiWeek(1:52, w_start = 10)
#' # Epi-week 0 aligned with ISO-week 10; 53 week year
#' IsoWeek2EpiWeek(1:52, w_start = 10, w_53 = TRUE)
IsoWeek2EpiWeek <- function (iso_week, w_start = 1, w_53 = FALSE) {
  a <- iso_week - w_start
  ifelse(a < 0, 51 + a + 1 + w_53, a)
}

#' Convert Epi-weeks to ISO-weeks
#' 
#' @param epi_week Epi-week vector.
#' @param w_start ISO-week for which Epi-week is 0.
#' @param w_53 Assume a 53 week year (default = FALSE)? 
#' 
#' @return A vector of ISO-weeks corresponding to the Epi-week input.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' epi_weeks = 0:51
#' # convert to iso week
#' iso_weeks <- EpiWeek2IsoWeek(epi_weeks, w_start = 10)
#' # convert back to epi week
#' IsoWeek2EpiWeek(iso_weeks, w_start = 10)
EpiWeek2IsoWeek <- function (epi_week, w_start = 1, w_53 = FALSE) {
  a <- epi_week + w_start
  ifelse(a > (52 + w_53), a - (52 + w_53), a)
}

# Data acquisition ------------------------------------------------

#' Download a Table from Statistics Denmark StatBank
#'
#' The default is to request the table at the maximum level
#' of cross-classification among variable levels.
#'
#' @param table_id Single table id.
#' @param total_vars Vector of variable id's for which only totals over
#' all sub-groups should be returned.
#' @param chunk_var Single variable id which is used for chunked API
#' requests. The chunks are defined by the variable values.
#' 
#' @author Jonas Schöley
#' 
#' @note This function connects to the API at http://api.statbank.dk.
#' See https://www.dst.dk/en/Statistik/statistikbanken/api for the API
#' specification.
#' 
#' This function can request a lot of data from the api, therefore, keep
#' its use to a minimum or otherwise risk the IT department at
#' Statistics Denmark to further restrict access to its API.
#' 
#' @return A data frame.
DownloadStatBankTable <- function (
  table_id, total_vars = '', chunk_var = ''
) {
  
  require(danstat)
  require(purrr)
  
  # prepare API request
  meta <- get_table_metadata(table_id)
  variable_ids <- meta$variables$id
  variable_values <- meta$variables$values
  total_loc <- sapply(total_vars, function (x) which(x == variable_ids))
  chunk_values <- ''
  if (chunk_var != '') {
    chunk_values <- variable_values[[which(variable_ids == chunk_var)]][['id']]
  }
  
  # build chunked api requests
  # 1. api request chunk
  # 1.1 variable
  # 1.1$code
  # 1.1$values
  # API request for a given value of a chunk variable
  BuildAPIRequest <-
    function (chunk_value, chunk_var, var_ids, var_values, total_vars) {
      
      api_request <-
        map2(
          var_ids, var_values,
          ~ {
            # unless otherwise specified,
            # request all variable levels
            spec <- list(code = .x, values = NA)
            # if a variable is specified as total,
            # download only totals
            if (.x %in% total_vars) {
              spec <- list(
                code = .x,
                # variable values for totals are
                # always in first position
                values = .y[['id']][1]
              )
            }
            if (.x %in% chunk_var) {
              spec <- list(
                code = .x,
                values = chunk_value
              )
            }
            spec
          }
        )
      
      return(api_request)
      
    }
  
  chunked_api_request <-
    map(
      chunk_values,
      BuildAPIRequest,
      chunk_var = chunk_var,
      var_ids = variable_ids,
      var_values = variable_values,
      total_vars = total_vars
    )
  
  DownloadTableChunks <- function (chunked_api_request, table_id) {
    cat(paste0(unlist(chunked_api_request), collapse = ' '), sep = '\n')
    
    the_data <-
      get_data(
        table_id = table_id,
        variables = chunked_api_request,
        language = 'en'
      )
    
    # give the server some time to breathe
    Sys.sleep(1)
    
    return(the_data)
  }
  
  the_table <-
    map_dfr(
      chunked_api_request,
      DownloadTableChunks,
      table_id = table_id,
      .id = 'chunk'
    )
  
  
  return(the_table)
  
}

# Model assessment ------------------------------------------------

#' Title
#'
#' @param df 
#' @param date 
#' @param expected_deaths 
#' @param expected_deaths_hi 
#' @param expected_deaths_lo 
#' @param observed_deaths 
#' @param training 
#' @param facet_row
#' @param facet_col
#' @param title 
#' @param caption_data 
#' @param caption_note 
#' @param date_breaks 
#' @param date_labels 
#'
#' @return
#'
#' @examples
#' number_of_weeks = 27
#' sequence_of_dates = as.Date('2012-01-02') + 1:number_of_weeks*7
#' df <- expand.grid(
#'   time = sequence_of_dates,
#'   region = c('A', 'B')
#' )
#' df <- within(
#'   df, {
#'     expct <- NA
#'     expct[region == 'A'] <-
#'       seq(300, 500, length.out = number_of_weeks)
#'     expct[region == 'B'] <-
#'       seq(200, 100, length.out = number_of_weeks)
#'     expct_q025 <- qpois(0.025, expct)
#'     expct_q975 <- qpois(0.975, expct)
#'     obsvd <- sapply(expct, function (x) rpois(1, x))
#'     training <- ifelse(time <= '2012-02-01', TRUE, FALSE)
#'   }
#' )
#' PlotObservedVsExpectedDeaths(
#'   df, time,
#'   expct, expct_q025, expct_q975,
#'   obsvd, training, facet_row = region
#' )
PlotObservedVsExpectedDeaths <- function (
  df,
  date = date,
  expected_deaths = pred,
  expected_deaths_hi = q975,
  expected_deaths_lo = q025,
  observed_deaths = deaths,
  training = training,
  facet_row = NULL,
  facet_col = NULL,
  title,
  caption_data,
  caption_note,
  date_breaks,
  date_labels
) {
  
  require(ggplot2)
  require(rlang)
  
  the_plot <-
    ggplot(df) +
    list(
      # prediction interval
      geom_ribbon(
        aes(
          x = {{date}},
          ymin = {{expected_deaths_lo}},
          ymax = {{expected_deaths_hi}}
        ),
        fill = 'grey90'
      ),
      # observed deaths
      geom_point(
        aes(
          x = {{date}},
          y = {{observed_deaths}},
          fill = {{training}}
        ), shape = 21, size = 1, color = 'white', stroke = 0
      ),
      # predicted deaths
      geom_line(
        aes(
          x = {{date}},
          y = {{expected_deaths}},
          color = {{training}},
          size = {{training}}
        )
      ),
      # facets
      facet_grid(
        rows = vars({{facet_row}}),
        cols = vars({{facet_col}}),
        scales = 'free_y'
      ),
      # scales
      scale_fill_manual(
        values =
          c(
            `test` = 'grey50',
            `training` = 'grey80'
          )
      ),
      scale_color_manual(
        values =
          c(
            `test` = 'black',
            `training` = 'grey50'
          )
      ),
      scale_size_manual(
        values =
          c(
            `test` = 0.5,
            `training` = 0.5
          )
      ),
      scale_x_date(
        date_breaks =
          if (missing(date_breaks)) waiver() else date_breaks,
        date_labels =
          if (missing(date_labels)) waiver() else date_labels
      ),
      scale_y_continuous(
        # ensure integers
        labels = scales::label_number(1, big.mark = ',')
      ),
      # theming
      theme_minimal(),
      theme(panel.grid.minor = element_blank(),
            panel.border = element_rect(fill = NA, colour = 'black')),
      guides(color = 'none'),
      # title
      if (!missing(title)) {
        ggtitle(title)
      },
      # labels
      labs(
        x = '',
        y = 'Deaths per week'
      ),
      # notes and data source
      if (any(!missing(caption_data), !missing(caption_note))) {
        labs(
          caption =
            paste0(
              ifelse(
                missing(caption_data),
                '',
                paste0('Data: ', caption_data, '\n')
              ),
              ifelse(
                missing(caption_note),
                '',
                paste0('Note: ', caption_note)
              )
            )
        )
      }
      
    )
  
  print(the_plot)
  
}

# Demography ------------------------------------------------------

#' Convert population estimates to population exposures
#'
#' Converts population estimates measured at discrete points
#' in time into population exposures by interpolating between
#' the data points using a cubic spline and integrating over
#' arbitrary time intervals.
#' 
#' @param df A data frame.
#' @param x Name of time variable.
#' @param P Name of population variable.
#' @param breaks_out Vector of interpolation points.
#' @param scaler Constant factor to change unit of exposures.
#' @param strata `vars()` specification of variables in df to stratify over.
#'
#' @return A data frame stratified by `strata` with population counts
#' `Px` at time `x1` and exposures `Ex` over time interval `[x1,x2)`. 
#'
#' @author Jonas Schöley, José Manuel Aburto
#'
#' @examples
#' df <-
#'   expand.grid(
#'     sex = c('Male', 'Female'),
#'     age = c('[0, 80)', '80+'),
#'     quarter_week = c(1, 14, 27, 40)
#'   )
#' df$P = rnorm(16, 1e3, sd = 100)
#' Population2Exposures(
#'   df, x = quarter_week, P = P,
#'   breaks_out = 1:57, strata = vars(sex, age)
#' )
Population2Exposures <-
  function (df, x, P, breaks_out, scaler = 1, strata = NA) {
    
    require(dplyr)
    require(tidyr)
    require(purrr)
    
    x = enquo(x); P = enquo(P)
    
    # for each stratum in the data return
    #   - interpolation function
    #   - interpolated population sizes
    #   - interpolated and integrated exposures
    group_by(df, !!!strata) %>% nest() %>%
      mutate(
        # limits of time intervals
        x1 = list(head(breaks_out, -1)),
        x2 = list(breaks_out[-1]),
        # cubic spline interpolation function with linear extrapolation
        interpolation_function = map(
          data,
          ~ splinefun(x = pull(., !!x), y = pull(., !!P),
                      method = 'natural')
        ),
        # interpolated population numbers
        Px = map(
          interpolation_function,
          ~ .(x = head(breaks_out, -1))
        ),
        # interpolated and integrated population exposures
        Ex = map(
          interpolation_function,
          # closed form expression for piece-wise polynomial
          # integral exists of course but implementation is
          # left for a later point
          ~ {
            fnct <- .
            map2_dbl(
              unlist(x1),
              unlist(x2),
              ~ integrate(fnct, lower = .x, upper = .y)[['value']]*scaler
            )
          }
        )
      ) %>%
      ungroup() %>%
      select(-data, -interpolation_function) %>%
      unnest(c(x1, x2, Px, Ex))
    
  }
