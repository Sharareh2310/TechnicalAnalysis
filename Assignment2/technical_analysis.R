# BDA400 Assignment 2 - Technical Analysis using R
# Preliminary Stage
# Portfolio: AAPL, MSFT, AMZN, GOOGL, TSLA

# Run these installation commands once if needed:
# install.packages(c("quantmod", "TTR"))

library(quantmod)
library(TTR)

# Function to load stock symbols from portfolio.txt and download data
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- trimws(readLines(portfolio_file))
  symbols <- symbols[nzchar(symbols)]

  stock_data <- list()

  for (symbol in symbols) {
    x <- getSymbols(symbol,
                    src = "yahoo",
                    from = from,
                    to = to,
                    auto.assign = FALSE)
    stock_data[[symbol]] <- x
  }

  return(stock_data)
}

# Statistical mode for numeric data
stat_mode <- function(x) {
  x <- x[!is.na(x)]
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Function to calculate required statistics
calculate_statistics <- function(stock_xts, symbol, ma_period = 20) {
  close_price <- as.numeric(Cl(stock_xts))
  moving_average <- SMA(Cl(stock_xts), n = ma_period)

  data.frame(
    Symbol = symbol,
    Mean = mean(close_price, na.rm = TRUE),
    Mode = stat_mode(close_price),
    Median = median(close_price, na.rm = TRUE),
    Standard_Deviation = sd(close_price, na.rm = TRUE),
    Latest_20_Day_Moving_Average =
      as.numeric(tail(na.omit(moving_average), 1))
  )
}

# Display utility
display_stock_data <- function(stock_list) {
  for (symbol in names(stock_list)) {
    cat("\n==============================\n")
    cat("Stock:", symbol, "\n")
    cat("==============================\n")
    print(head(stock_list[[symbol]]))
    cat("\nMost recent observations:\n")
    print(tail(stock_list[[symbol]]))
  }
}

# Visualization utility
plot_stock <- function(stock_xts, symbol, ma_period = 20) {
  chartSeries(stock_xts,
              name = paste(symbol, "Stock Price"),
              theme = "white")
  addSMA(n = ma_period)
}

# MAIN PROGRAM
stocks <- load_stock_data()

display_stock_data(stocks)

statistics <- do.call(
  rbind,
  lapply(names(stocks), function(symbol) {
    calculate_statistics(stocks[[symbol]], symbol)
  })
)

cat("\nPortfolio Statistics:\n")
print(statistics)

# Display charts for all stocks with 20-day moving average
for (symbol in names(stocks)) {
  plot_stock(stocks[[symbol]], symbol)
}
