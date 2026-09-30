# BDA400 Assignment 6 - Technical Analysis using R
# Visualization Phase
# Portfolio: AAPL, MSFT, AMZN, GOOGL, TSLA

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# Portfolio stocks
portfolio <- c("AAPL", "MSFT", "AMZN", "GOOGL", "TSLA")

# User Interface
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "stock",
        "Select Stock:",
        choices = portfolio,
        selected = "AAPL"
      ),

      dateRangeInput(
        "dates",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),

      selectInput(
        "timeframe",
        "Select Timeframe:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),

      checkboxInput("show_sma", "Show Moving Average (SMA)", TRUE),
      checkboxInput("show_rsi", "Show RSI", FALSE),
      checkboxInput("show_macd", "Show MACD", FALSE)
    ),

    mainPanel(
      plotOutput("stockPlot"),
      conditionalPanel(
        condition = "input.show_rsi == true",
        plotOutput("rsiPlot")
      ),
      conditionalPanel(
        condition = "input.show_macd == true",
        plotOutput("macdPlot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Download selected stock data from Yahoo Finance
  stock_data <- reactive({
    req(input$stock, input$dates)

    data <- tryCatch({
      getSymbols(
        input$stock,
        src = "yahoo",
        from = input$dates[1],
        to = input$dates[2],
        auto.assign = FALSE
      )
    }, error = function(e) {
      return(NULL)
    })

    validate(
      need(!is.null(data), "Unable to download stock data. Please try again.")
    )

    # Convert timeframe
    if (input$timeframe == "Weekly") {
      data <- to.weekly(data, drop.time = TRUE)
    } else if (input$timeframe == "Monthly") {
      data <- to.monthly(data, drop.time = TRUE)
    }

    data
  })

  # Prepare stock data and technical indicators
  analysis_data <- reactive({
    data <- stock_data()
    close_price <- Cl(data)

    result <- data.frame(
      Date = index(data),
      Close = as.numeric(close_price)
    )

    validate(
      need(nrow(result) >= 2, "Not enough observations for technical analysis.")
    )

    # Simple Moving Average
    sma_n <- min(20, nrow(result))
    result$SMA20 <- SMA(result$Close, n = sma_n)

    # Relative Strength Index
    rsi_n <- min(14, nrow(result) - 1)
    result$RSI <- RSI(result$Close, n = rsi_n)

    # MACD
    if (nrow(result) >= 27) {
      macd_values <- MACD(
        result$Close,
        nFast = 12,
        nSlow = 26,
        nSig = 9
      )
      result$MACD <- as.numeric(macd_values[, 1])
      result$SignalLine <- as.numeric(macd_values[, 2])
    } else {
      result$MACD <- NA_real_
      result$SignalLine <- NA_real_
    }

    # Buy / Sell / Hold trading signal
    result$TradingSignal <- "Hold"

    buy_condition <- !is.na(result$SMA20) & !is.na(result$RSI) &
      result$Close > result$SMA20 & result$RSI < 70

    sell_condition <- !is.na(result$SMA20) & !is.na(result$RSI) &
      result$Close < result$SMA20 & result$RSI > 30

    result$TradingSignal[buy_condition] <- "Buy"
    result$TradingSignal[sell_condition] <- "Sell"

    result
  })

  # Main stock price chart
  output$stockPlot <- renderPlot({
    df <- analysis_data()

    p <- ggplot(df, aes(x = Date, y = Close)) +
      geom_line(linewidth = 0.8) +
      labs(
        title = paste(input$stock, "-", input$timeframe, "Price Chart"),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()

    # Add moving average when selected
    if (input$show_sma) {
      p <- p +
        geom_line(
          aes(y = SMA20),
          linewidth = 0.8,
          linetype = "dashed",
          na.rm = TRUE
        )
    }

    # Add Buy signals
    p <- p +
      geom_point(
        data = subset(df, TradingSignal == "Buy"),
        aes(x = Date, y = Close),
        shape = 24,
        size = 3
      )

    # Add Sell signals
    p <- p +
      geom_point(
        data = subset(df, TradingSignal == "Sell"),
        aes(x = Date, y = Close),
        shape = 25,
        size = 3
      )

    p
  })

  # RSI chart
  output$rsiPlot <- renderPlot({
    req(input$show_rsi)
    df <- analysis_data()

    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI"
      ) +
      theme_minimal()
  })

  # MACD chart
  output$macdPlot <- renderPlot({
    req(input$show_macd)
    df <- analysis_data()

    validate(
      need(
        any(!is.na(df$MACD)),
        "MACD requires more observations. Try Daily/Weekly or a longer date range."
      )
    )

    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD), linewidth = 0.8, na.rm = TRUE) +
      geom_line(
        aes(y = SignalLine),
        linewidth = 0.8,
        linetype = "dashed",
        na.rm = TRUE
      ) +
      labs(
        title = "MACD Indicator",
        x = "Date",
        y = "MACD"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
