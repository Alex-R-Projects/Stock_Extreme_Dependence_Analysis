################################################################################

# This section is covering the data wrangling for working with LOG RETURNS


# Create functions for tick marks
number_ticks <- function(n) { 
  function(limits) pretty(limits, n) 
}

date_ticks <- function(n) {
  function(limits) {
    pretty(seq(limits[1], limits[2], length.out = n), n)
  }
}

# Function to get and process stock prices
get_prices <- function(tickers, start_date, end_date, sector_name) {
  prices <- tq_get(tickers, from = start_date, to = end_date) |> 
    mutate(
      symbol = factor(symbol),
      sector = sector_name
    ) |> 
    group_by(symbol) |> 
    mutate(
      close_scaled = as.vector(scale(close)),  # Scale closing prices
      close_lgret = log(close / lag(close))  # Calculate log returns
    ) |> 
    ungroup() |> 
    drop_na() |> 
    select(date, sector, symbol, close, close_scaled, close_lgret)
  
  return(prices)
}

# Function to convert prices data to wide format for copula analysis
get_wide_prices <- function(prices) {
  wide_prices <- prices |> 
    select(date, symbol, close_lgret) |> 
    pivot_wider(names_from = symbol, values_from = close_lgret) |> 
    select(-date)
  
  return(wide_prices)
}

# Define start and end dates for the data
start_date <- "2016-01-01"
end_date <- "2023-12-31"

# Define tickers for the consumer discretionary sector
staples_tickers <- c(
  "COST", "WMT", "KO",
  "PEP", "TGT", "KR",
  "TSN", "CLX", "STZ",
  "MNST", "SYY", "PG")

# Define tickers for the communication services sector
communication_tickers <- c(
  "META", "GOOGL", "NFLX", 
  "DIS", "WBD", "PARA", 
  "CMCSA", "EA", "T", 
  "VZ", "TMUS", "OMC")

# Define tickers for the consumer staples sector
discretionary_tickers <- c(
  "AMZN", "TSLA", "HD",
  "MCD", "LOW", "NKE",
  "SBUX", "MAR", "EBAY",
  "BBY", "ETSY", "MGM")


## Filtering and Processing stocks for Consumer Staples Sector ##


# Fetch and process consumer staples sector stock prices
staples_prices <- get_prices(
  tickers = staples_tickers, 
  start_date = start_date, 
  end_date = end_date, 
  sector_name = "Consumer Staples"
)

staples_stable <- staples_prices |> 
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")
staples_stable$period <- "Stable Market"
staples_stable <- staples_stable |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

staples_pandemic <- staples_prices |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")
staples_pandemic$period <- "Pandemic and Downturn"
staples_pandemic <- staples_pandemic |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

# Combine the periods
staples_period <- rbind(
  staples_stable, staples_pandemic
) |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  ) |> 
  mutate(
    period = factor(
      period, 
      ordered = TRUE,
      levels = c("Stable Market", "Pandemic and Downturn")
    )
  )


## Filtering and Processing stocks for Consumer Discretionary Sector ##

discretionary_prices <- get_prices(
  tickers = discretionary_tickers, 
  start_date = start_date, 
  end_date = end_date, 
  sector_name = "Consumer Discretionary"
)

discretionary_stable <- discretionary_prices |> 
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")
discretionary_stable$period <- "Stable Market"
discretionary_stable <- discretionary_stable |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

discretionary_pandemic <- discretionary_prices |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")
discretionary_pandemic$period <- "Pandemic and Downturn"
discretionary_pandemic <- discretionary_pandemic |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

# Combine the periods ()
discretionary_period <- rbind(
  discretionary_stable, discretionary_pandemic
) |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  ) |> 
  mutate(
    period = factor(
      period, 
      ordered = TRUE,
      levels = c("Stable Market", "Pandemic and Downturn")
    )
  )


# Filtering and Processing stocks for Communication Services Sector

communication_prices <- get_prices(
  tickers = communication_tickers, 
  start_date = start_date, 
  end_date = end_date, 
  sector_name = "Communication Services"
)

communication_stable <- communication_prices |> 
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")
communication_stable$period <- "Stable Market"
communication_stable <- communication_stable |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

communication_pandemic <- communication_prices |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")
communication_pandemic$period <- "Pandemic and Downturn"
communication_pandemic <- communication_pandemic |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  )

# Combine the periods ()
communication_period <- rbind(
  communication_stable, communication_pandemic
) |> 
  select(
    date, period, sector, symbol, close, close_scaled, close_lgret
  ) |> 
  mutate(
    period = factor(
      period, 
      ordered = TRUE,
      levels = c("Stable Market", "Pandemic and Downturn")
    )
  )






################################################################################

# This section is covering the data wrangling for working with GARCH Residuals

#' Test for ARCH effects and fit a GARCH(1,1) model IF necessary
#'
#' This function takes a time series of log-returns (numerical vector) as input, 
#' performs an ARCH test using the FinTS::ArchTest function, and checks whether 
#' a GARCH structure is necessary. If the ARCH test indicates the need for a GARCH model, 
#' the function fits a GARCH(1,1) model using the rugarch package and returns the residuals.
#' If the ARCH test fails (i.e., no GARCH structure is necessary), it simply returns the input time series.
#'
#' @param x A numerical vector containing the time series of log-returns.
#' @param lags The number of lags for the ARCH test (default is 5).
#' @param alpha The significance level for the ARCH test (default is 0.05).
#' 
#' @return If no ARCH effects are found, the function returns the original time series. 
#' If ARCH effects are detected, it returns the residuals of the fitted GARCH(1,1) model.
#'
#' @examples
#' # Example with simulated GARCH(1,1) data:
#' n <- 1000
#' x <- as.numeric(fitted(ugarchpath(ugarchspec(), n.sim = n)))
#' result <- test_and_fit_garch(x)
#' print(result)
#'
#' @import rugarch
#' @import FinTS
#' @export
test_and_fit_garch <- function(x, lags = 5, alpha = 0.05) {
  
  # Step 1: Perform the ARCH test using FinTS::ArchTest
  arch_test <- FinTS::ArchTest(x, lags = lags)
  
  # Extract the p-value from the test
  p_value <- arch_test$p.value
  
  # Step 2: Decision rule based on p-value
  if (p_value >= alpha) {
    # If the p-value is greater than alpha, no ARCH effects are detected
    print("No significant ARCH effects. A GARCH model might not be necessary.")
    return(x)  # Return the original time series
  } else {
    # If the p-value is less than alpha, ARCH effects are detected
    print("There is evidence of ARCH effects. A GARCH model may be necessary.")
    
    # Step 3: Fit the GARCH(1,1) model to the data 'x'
    garch_spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "norm"
    )
    
    # Fit the model
    garch_fit <- ugarchfit(spec = garch_spec, data = x)
    
    # Step 4: Extract the residuals
    garch_residuals <- residuals(garch_fit)
    
    # Return the residuals of the GARCH(1,1) model
    return(as.numeric(garch_residuals))
  }
}


# List of data frames for each sector
sectors_log_returns <- list(
  communication = communication_period,
  discretionary = discretionary_period,
  staples = staples_period
)

# Initialize an empty data frame to store the GARCH results
garch_results <- data.frame(
  date = as.Date(character()),
  sector = character(),
  symbol = character(),
  residual = numeric(),
  passed_garch_test = logical()
)

# Loop through each sector
for (sector_name in names(sectors_log_returns)) {
  
  # Get the data for the current sector
  sector_data <- sectors_log_returns[[sector_name]]
  
  # Get the unique stock symbols in this sector
  stock_symbols <- unique(sector_data$symbol)
  
  # Loop through each stock
  for (stock in stock_symbols) {
    
    # Extract the log-returns and dates for the current stock
    stock_data <- sector_data[sector_data$symbol == stock, ]
    dates <- stock_data$date  # Assuming 'date' column exists
    log_returns <- stock_data$close_lgret
    
    # Run the GARCH test and fit function
    garch_result <- test_and_fit_garch(log_returns)
    
    # Determine if the GARCH model was fitted
    passed_garch_test <- !identical(log_returns, garch_result)
    
    # Add each entry as a separate row to the garch_results data frame
    for (i in seq_along(dates)) {
      garch_results <- rbind(
        garch_results,
        data.frame(
          date = dates[i],
          sector = sector_name,
          symbol = stock,
          residual = garch_result[i],
          passed_garch_test = passed_garch_test
        )
      )
    }
  }
}



# Create separate data frames for each sector
garch_communication <- garch_results |> filter(sector == "communication")
garch_discretionary <- garch_results |> filter(sector == "discretionary")
garch_staples <- garch_results |> filter(sector == "staples")


# Now we are going to separate each dataframe further for stable, and pandemic periods

# Filtering for the communication services sector
garch_communication_stable <- garch_communication |>
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")

garch_communication_pandemic <- garch_communication |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")

# Filtering for the consumer discretionary sector
garch_discretionary_stable <- garch_discretionary |> 
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")

garch_discretionary_pandemic <- garch_discretionary |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")

# Filtering for the consumer staples sector
garch_staples_stable <- garch_staples |> 
  dplyr::filter(date >= "2016-01-01" & date <= "2018-12-31")

garch_staples_pandemic <- garch_staples |> 
  dplyr::filter(date >= "2019-12-31" & date <= "2023-12-31")


# Function to arrange the data (GARCH Residuals) into a wide forma
get_wide_residuals <- function(residuals) {
  wide_residuals <- residuals |> 
    select(date, symbol, residual) |> 
    pivot_wider(names_from = symbol, values_from = residual) |> 
    select(-date)
  
  return(wide_residuals)
}


