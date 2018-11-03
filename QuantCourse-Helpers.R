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

# Plot an xts of returns with a max argument
prettyReturnPlot <- function(R){
  
  # Check args
  if(!is.xts(R)) stop("Please pass a valid XTS object")
  
  # Get charting parameters
  num_series <- ncol(R)
  max_colors <- min(num_series, 8)+2
  if(num_series > max_colors*2){
    warning("Too many series to chart. Max is ",max_colors*2)
  } else {
    plot_colset <- c(brewer.pal("Dark2",n=min(max_colors, 8)),"blue","red")
    plot_lty <- ifelse(1:num_series > max_colors, 3, 1)
    plot_lwd <- ifelse(1:num_series > max_colors, 3, 1)
    plot_col_ind <- seq(1, num_series, by=1)
    plot_col_ind[plot_col_ind > max_colors] <- plot_col_ind[plot_col_ind > max_colors] - max_colors
    plot_colset <- plot_colset[plot_col_ind]
    
    # Run the plot
    chart.CumReturns(R, legend.loc = "topleft", ylab="Cumulative Return", lty=plot_lty, main="Series Cumulative Returns", colorset = plot_colset, lwd=2)
  }
}

# Function to create a combined returns object from adjusted prices
createCombinedReturns <- function(prices){
  
  # Check args
  if(!is.xts(prices)) stop("Please pass a valid XTS object")
  
  # Merge prices
  combined_prices <- do.call(merge, lapply(prices, Ad))
  colnames(combined_prices) <- names(prices)
  
  # Calculate returns
  combined_returns <- ROC(combined_prices, type="discrete", na.pad=F)
  
  return(combined_returns)
}

########## END HELPER FUNCTIONS ##########