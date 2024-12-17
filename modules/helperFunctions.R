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



calculate_EP <- function(data, periods, period, year, cutoffYear) {
  # Check if the period is a quarter (contains "Q") or a month
  if (grepl("^Q[1-4]$", period)) {
    # Quarterly Calculation
    period_key_end <- paste0(period, "_end")
    period_key_start <- paste0(period, "_start")
    period_EP <- ifelse(
      data$Auth_year < cutoffYear, 
      0,
      (pmax(0, pmin(periods[[period_key_end]], data$EndDate) -
              pmax(periods[[period_key_start]], data$BegDate) + 1) /
       data$Duration) * data$Premium
    )
  } else {
    # Monthly Calculation
    period_key_end <- "end"
    period_key_start <- "start"
    period_EP <- ifelse(
      data$Auth_year < cutoffYear, 
      0,
      (pmax(0, pmin(periods[[period]]$end, data$EndDate) -
              pmax(periods[[period]]$start, data$BegDate) + 1) /
       data$Duration) * data$Premium
    )
  }

  return(period_EP)
}


calculatePremiums <- function(data, startYear, endYear, endPeriod, timePeriod, cutoffYear) {
  operations_done <- 0
  
  # Define total operations for progress tracking
  total_operations <- if (timePeriod == "Monthly") {
    12 * (endYear - startYear + 1)
  } else {
    4 * (endYear - startYear + 1)
  }
  
  # Monthly EP Calculation
  if (timePeriod == "Monthly") {
    period_names <- c("January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December")
    
    for (yr in startYear:endYear) {
      year_months <- define_months(yr)
      months_to_iterate <- period_names
      
      if (yr == endYear && endPeriod != "All") {
        end_index <- match(endPeriod, period_names)
        months_to_iterate <- period_names[1:end_index]
      }
      
      for (month in months_to_iterate) {
        month_EP_col <- paste0(month, "_", yr, "_EP")
        data[[month_EP_col]] <- calculate_EP(data, year_months, month, yr, cutoffYear)
        operations_done <- operations_done + 1
        
        if (exists("setProgress")) {
          setProgress(operations_done / total_operations)
        }
      }
    }
  }
  
  # Quarterly EP Calculation
  if (timePeriod == "Quarterly") {
    period_names <- c("Q1", "Q2", "Q3", "Q4")
    
    for (yr in startYear:endYear) {
      year_quarters <- define_quarters(yr)
      quarters_to_iterate <- period_names
      
      if (yr == endYear && endPeriod != "All") {
        end_index <- match(endPeriod, period_names)
        quarters_to_iterate <- period_names[1:end_index]
      }
      
      for (quarter in quarters_to_iterate) {
        quarter_EP_col <- paste0(quarter, "_", yr, "_EP")
        data[[quarter_EP_col]] <- calculate_EP(data, year_quarters, quarter, yr, cutoffYear)
        operations_done <- operations_done + 1
        
        if (exists("setProgress")) {
          setProgress(operations_done / total_operations)
        }
      }
    }
  }
  
  return(data)
}

