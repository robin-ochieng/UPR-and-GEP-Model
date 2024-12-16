# Define helper functions used in calculations

# Function to define the months of a year with start and end dates
define_months <- function(year) {
  months <- list()
  for (month in 1:12) {
    start_date <- lubridate::mdy(paste(month, '1', year))
    end_date <- if (month == 12) {
      lubridate::mdy(paste('12', '31', year))
    } else {
      lubridate::mdy(paste(month + 1, '1', year)) - lubridate::days(1)
    }
    month_name <- month.name[month]
    months[[month_name]] <- list(start = start_date, end = end_date)
  }
  return(months)
}

# Function to calculate earned premiums
calculate_EP_Months <- function(data, year_months, month, year, cutoff_year) {
  month_EP <- ifelse(data$Auth_year < cutoff_year, 0,
                     (pmax(0, pmin(year_months[[month]]$end, data$EndDate) -
                             pmax(year_months[[month]]$start, data$BegDate) + 1) /
                        data$Duration) * data$Premium)
  return(month_EP)
}


# Define helper functions used in calculations
define_quarters <- function(year) {
  Q1_start <- mdy(paste('1/1/', year))
  Q1_end <- mdy(paste('3/31/', year))
  Q2_start <- mdy(paste('4/1/', year))
  Q2_end <- mdy(paste('6/30/', year))
  Q3_start <- mdy(paste('7/1/', year))
  Q3_end <- mdy(paste('9/30/', year))
  Q4_start <- mdy(paste('10/1/', year))
  Q4_end <- mdy(paste('12/31/', year))
  return(list(Q1_start = Q1_start, Q1_end = Q1_end,
              Q2_start = Q2_start, Q2_end = Q2_end,
              Q3_start = Q3_start, Q3_end = Q3_end,
              Q4_start = Q4_start, Q4_end = Q4_end))
}

calculate_EP_Quarters <- function(data, year_quarters, quarter, year, cutoff_year) {
  quarter_EP <- ifelse(data$Auth_year < cutoff_year, 0,
                       (pmax(0, pmin(year_quarters[[paste0(quarter, "_end")]], data$EndDate) -
                               pmax(year_quarters[[paste0(quarter, "_start")]], data$BegDate) + 1) /
                          data$Duration) * data$Premium)
  return(quarter_EP)
}