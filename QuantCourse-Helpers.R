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

########## END HELPER FUNCTIONS ##########