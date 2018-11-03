# Required libraries
require("quantmod")
require("lubridate")

########## BEGIN HELPER FUNCTIONS ##########

# Function to retrieve pricing information for a vector of tickers
getPriceData <- function(tickers, maxDate){
  
  # Ensure tickers are characters
  tickers <- as.character(tickers)
  if(class(tickers) != "character") stop("Please input a character vector for the ticker arg")
  
  # Ensure date is formatted properly
  maxDate <- as.Date(maxDate)
  if(class(maxDate) != "Date") stop("Please input a properly formatted Date for the maxDate arg")
  
  # Retrieve prices
  message("Retrieving prices for ",length(tickers)," symbols")
  all_prices <- sapply(tickers, getSymbols, auto.assign = F, from=maxDate)
  message("Pricing data retrieved for ",length(all_prices)," securities")
  return(all_prices)
}

# Function to enforce a pricing cutoff date. Assumes prices is a list of XTS objects and cutoff_date must be a valid date
# Returns an updated prices object
enforcePricingCutoffDate <- function(prices, cutoff_date){
  # Check arguments
  if(sum(!sapply(prices, is.xts)) > 0) stop("Please pass all valid xts objects into the prices argument")
  if(class(cutoff_date) != "Date") stop("Please pass a valid cutoff_date Date arg")
  
  # Describe pricing information
  min_dates <- sapply(prices, function(d) as.character(min(index(d))))
  max_dates <- sapply(prices, function(d) as.character(max(index(d))))
  
  # Drop any securities which don't make the cutoff date
  valid_securities <- min_dates <= cutoff_date
  num_invalid <- sum(!valid_securities)
  if(num_invalid > 0){
    message("Dropping ",num_invalid," securitie(s) which don't make the cutoff date of ",cutoff_date,":")
    message(paste0(names(prices)[!valid_securities], collapse=", "))
    prices <- prices[valid_securities]
    prices <- lapply(prices, function(d) d[index(d) >= cutoff_date,])
  }
  return(prices)
}

########## END HELPER FUNCTIONS ##########