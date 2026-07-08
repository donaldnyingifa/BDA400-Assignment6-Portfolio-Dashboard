################################################################################
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase (15%)
# Author: Donald Nyingifa
#
# This app fetches historical stock data from Yahoo Finance, lets the user
# switch between line, candlestick, and area charts, overlays Moving Averages,
# RSI, and MACD, and generates Buy / Sell / Hold signals from a Moving Average
# crossover rule. Signals are annotated directly on the price chart and listed
# in a table below it.
################################################################################

# ---- Step 1: Packages ------------------------------------------------------

required_packages <- c("shiny", "ggplot2", "quantmod", "tidyquant",
                        "dplyr", "patchwork", "scales")

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}

library(shiny)
library(ggplot2)
library(quantmod)
library(tidyquant)   # gives us geom_candlestick(), geom_ma(), geom_bbands()
library(dplyr)
library(patchwork)   # stacks the price / RSI / MACD panels
library(scales)


# ---- Step 2: UI -------------------------------------------------------------

ui <- fluidPage(

  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(

    sidebarPanel(
      textInput("stock_symbol", "Stock Symbol:", value = "AAPL"),

      dateRangeInput(
        "date_range", "Select Date Range:",
        start = "2023-01-01", end = "2023-07-01",
        max   = Sys.Date()
      ),

      selectInput(
        "time_frame", "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),

      selectInput(
        "chart_type", "Chart Type:",
        choices = c("Line", "Candlestick", "Area")
      ),

      checkboxGroupInput(
        "technical_indicators", "Technical Indicators:",
        choices  = c("Moving Averages", "RSI", "MACD"),
        selected = "Moving Averages"
      ),

      conditionalPanel(
        condition = "input.technical_indicators.includes('Moving Averages')",
        numericInput("short_ma", "Short-term MA period:", value = 20, min = 2, max = 100),
        numericInput("long_ma",  "Long-term MA period:",  value = 50, min = 5, max = 200)
      ),

      checkboxInput("show_signals", "Show Buy / Sell signals on chart", value = TRUE),

      actionButton("fetch", "Fetch / Refresh Data", class = "btn-primary"),

      hr(),
      helpText("Data source: Yahoo Finance, via quantmod::getSymbols().")
    ),

    mainPanel(
      uiOutput("error_message"),
      plotOutput("stock_chart", height = "550px"),
      h4("Trading Signals (Moving Average Crossover Rule)"),
      tableOutput("signals_table")
    )
  )
)


# ---- Step 3: Server ----------------------------------------------------------

server <- function(input, output, session) {

  # --- Data Collection and Setup -------------------------------------------
  # Fetches data only when the button is pressed (or on first load), and
  # handles missing/invalid symbols or empty date ranges gracefully.
  raw_data <- eventReactive(input$fetch, {
    tryCatch({
      getSymbols(
        input$stock_symbol,
        src        = "yahoo",
        from       = input$date_range[1],
        to         = input$date_range[2],
        auto.assign = FALSE
      )
    }, error = function(e) {
      NULL
    })
  }, ignoreNULL = FALSE)

  output$error_message <- renderUI({
    if (is.null(raw_data())) {
      div(style = "color:red;",
          paste("Could not fetch data for symbol '", input$stock_symbol,
                "'. Check the ticker and date range and try again.", sep = ""))
    }
  })

  # Resample to the requested time frame (Daily / Weekly / Monthly)
  periodized_data <- reactive({
    req(raw_data())
    switch(input$time_frame,
      "Daily"   = raw_data(),
      "Weekly"  = to.weekly(raw_data(), indexAt = "lastof", OHLC = TRUE),
      "Monthly" = to.monthly(raw_data(), indexAt = "lastof", OHLC = TRUE)
    )
  })

  # Convert the xts object into a tidy data frame that ggplot/tidyquant like
  filtered_data <- reactive({
    data <- periodized_data()
    req(data)
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

    df <- data.frame(Date = zoo::index(data), zoo::coredata(data))
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    df <- df[order(df$Date), ]
    validate(need(nrow(df) > 1, "Not enough data points in the selected range."))
    df
  })

  # --- Technical Indicators -------------------------------------------------

  ma_data <- reactive({
    df <- filtered_data()
    df$ShortMA <- as.numeric(SMA(df$Close, n = input$short_ma))
    df$LongMA  <- as.numeric(SMA(df$Close, n = input$long_ma))
    df
  })

  rsi_data <- reactive({
    df <- filtered_data()
    df$RSI <- as.numeric(RSI(df$Close, n = 14))
    df
  })

  macd_data <- reactive({
    df <- filtered_data()
    macd_vals <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
    df$MACD   <- as.numeric(macd_vals[, "macd"])
    df$Signal <- as.numeric(macd_vals[, "signal"])
    df
  })

  # --- Step 4: Trading Rules and Signals ------------------------------------
  # Moving Average Crossover Rule:
  #   short MA crosses above long MA -> Buy
  #   short MA crosses below long MA -> Sell
  #   otherwise -> Hold
  signals_data <- reactive({
    df <- ma_data()
    df$Position <- ifelse(df$ShortMA > df$LongMA, "Above", "Below")
    df$Signal   <- "Hold"

    for (i in 2:nrow(df)) {
      if (!is.na(df$Position[i]) && !is.na(df$Position[i - 1]) &&
          df$Position[i] != df$Position[i - 1]) {
        df$Signal[i] <- if (df$Position[i] == "Above") "Buy" else "Sell"
      }
    }
    df
  })

  # --- Chart building --------------------------------------------------------

  build_price_chart <- reactive({
    df <- signals_data()

    p <- ggplot(df, aes(x = Date))

    p <- switch(input$chart_type,
      "Line" = p + geom_line(aes(y = Close), color = "steelblue", linewidth = 0.8),
      "Area" = p + geom_area(aes(y = Close), fill = "steelblue", alpha = 0.3) +
                   geom_line(aes(y = Close), color = "steelblue", linewidth = 0.6),
      "Candlestick" = p + geom_candlestick(
                        aes(open = Open, high = High, low = Low, close = Close)
                      )
    )

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = ShortMA), color = "orange", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = LongMA),  color = "purple", linewidth = 0.8, na.rm = TRUE)
    }

    if (input$show_signals) {
      buy_points  <- df[df$Signal == "Buy", ]
      sell_points <- df[df$Signal == "Sell", ]

      p <- p +
        geom_point(data = buy_points,  aes(y = Close), color = "darkgreen", size = 3, shape = 24, fill = "darkgreen") +
        geom_point(data = sell_points, aes(y = Close), color = "darkred",   size = 3, shape = 25, fill = "darkred") +
        geom_text(data = buy_points,  aes(y = Close, label = "Buy"),  vjust = -1.2, color = "darkgreen", size = 3) +
        geom_text(data = sell_points, aes(y = Close, label = "Sell"), vjust = 2,    color = "darkred",   size = 3)
    }

    p +
      labs(
        title = paste0(toupper(input$stock_symbol), " Price Chart (", input$time_frame, ")"),
        x = NULL, y = "Price (USD)"
      ) +
      scale_x_date(labels = date_format("%b %Y")) +
      theme_tq()
  })

  build_rsi_chart <- reactive({
    df <- rsi_data()
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "darkblue", linewidth = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "darkgreen") +
      labs(title = "RSI (14)", x = NULL, y = "RSI") +
      ylim(0, 100) +
      theme_tq()
  })

  build_macd_chart <- reactive({
    df <- macd_data()
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD),   color = "steelblue", linewidth = 0.7, na.rm = TRUE) +
      geom_line(aes(y = Signal), color = "orange",    linewidth = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      labs(title = "MACD (12, 26, 9)", x = NULL, y = "MACD") +
      theme_tq()
  })

  output$stock_chart <- renderPlot({
    req(filtered_data())

    price_plot <- build_price_chart()
    panels <- list(price_plot)

    if ("RSI" %in% input$technical_indicators)  panels <- append(panels, list(build_rsi_chart()))
    if ("MACD" %in% input$technical_indicators) panels <- append(panels, list(build_macd_chart()))

    if (length(panels) == 1) {
      panels[[1]]
    } else {
      heights <- c(3, rep(1, length(panels) - 1))
      Reduce(`/`, panels) + plot_layout(heights = heights)
    }
  })

  output$signals_table <- renderTable({
    df <- signals_data()
    trades <- df[df$Signal %in% c("Buy", "Sell"), c("Date", "Close", "Signal")]
    trades$Close <- round(trades$Close, 2)
    if (nrow(trades) == 0) {
      data.frame(Date = character(0), Close = numeric(0), Signal = character(0))
    } else {
      trades
    }
  })
}

# ---- Step 5: Run the app -----------------------------------------------------

shinyApp(ui = ui, server = server)
