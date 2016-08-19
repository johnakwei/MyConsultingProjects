
############################################
############################################
##
## Trading Dashboard Application
##
## Version 1.1, Completed 8/15/2016
##
## All R Language programming by
## John Akwei, ECMp ERMp Data Scientist
##
## ContextBase, contextbase.github.io
## johnakwei1@gmail.com
##
############################################
############################################

library(dplyr)
library(ggplot2)
library(stats)
library(zoo)
library(xts)
library(forecast)
library(tseries)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Trading Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Search", tabName="stocksearch",
               icon=icon("dashboard")),
      menuItem("Trade Strategy Analysis", tabName="stratanalysis",
               icon=icon("dashboard")),
      menuItem("Summary Graphs", tabName="summarygraphs",
               icon=icon("th")),
      menuItem("Prediction Graphs", tabName="predictiongraphs",
               icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="stocksearch",
              fluidRow(
                box(title="Input the Stock to examine,
                and the date range of information to collect:",
                textInput("textTicker",
                label=h3("Stock Information Lookup"), value="AAPL"),
                dateRangeInput("dateRange",label="Enter date range",
                               start = "2000-01-01",
                               end = Sys.Date()-1),
                submitButton('Submit'))
              ),
              fluidRow(
                infoBoxOutput("textOutputTicker", width=6),
                box(plotOutput('plot1'), width=12)
              )
      ),
      tabItem(tabName="stratanalysis",
              fluidRow(
                infoBoxOutput("textOutputTicker2", width=6),
                box(title="",
                    textInput("buySell",
                    label=h3("Buy / Sell at:"), value="10.0"),
                    submitButton('Submit'),
                    width = 12),
                box(title="Trading Strategy Analysis - 
                    Summary Statistics:",
                    valueBoxOutput("winningTradesBox"),
                    valueBoxOutput("averageWinBox"),
                    valueBoxOutput("averageLossBox"),
                    valueBoxOutput("totalProfitsBox"),
                    width = 12),
                box(title="Trading Strategy Analysis -
                Table of Simulated Trades:"),
                box(tableOutput('table1'), width = 12)
              )
      ),
      tabItem(tabName="summarygraphs",
              fluidRow(
                infoBoxOutput("textOutputTicker3", width=6),
                box(title="Graph of the
                Trading Strategy Analysis",
                plotOutput('plot2'), width = 12)
                )
      ),
      tabItem(tabName="predictiongraphs",
              fluidRow(
                infoBoxOutput("textOutputTicker4", width=6),
                box(title="Graph of 20 Day Stock Price Forecast",
                plotOutput('plot3'), width = 12)
              )
      )
      
    )
  )
)

server <- function(input, output) {
  
  ticker <- reactive({ c(input$textTicker) })
  start_date <- reactive({ as.Date(c(input$dateRange[1])) })
  end_date <- reactive({ as.Date(c(input$dateRange[2])) })
  
  
  output$textOutputTicker <- renderInfoBox({
    infoBox("Ticker", input$textTicker, icon = icon("list"))
    })
  
  output$textOutputTicker2 <- renderInfoBox({
    infoBox("Ticker", input$textTicker, icon = icon("list"))
  })
  
  output$textOutputTicker3 <- renderInfoBox({
    infoBox("Ticker", input$textTicker, icon = icon("list"))
  })
  
  output$textOutputTicker4 <- renderInfoBox({
    infoBox("Ticker", input$textTicker, icon = icon("list"))
  })
  
  output$table1 <- renderTable({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    retSMA <- ROC(ticker_SMA) * sig
    table.Drawdowns(retSMA, top=10)
    })
  
  output$winningTradesBox <- renderValueBox({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    retSMA <- ROC(ticker_SMA) * sig
    valueBox(
      paste0(nrow(retSMA[retSMA > 0])),
      "Winning Trades",
      icon=icon("credit-card"),
      color="purple")
  })
  
  output$averageWinBox <- renderValueBox({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    retSMA <- ROC(ticker_SMA) * sig
    valueBox(
      paste0(round(sum(retSMA[retSMA > 0]) / nrow(retSMA[retSMA > 0]), 4)),
      "Average Win",
      icon=icon("credit-card"),
      color="purple")
  })
  
  output$averageLossBox <- renderValueBox({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    retSMA <- ROC(ticker_SMA) * sig
    valueBox(
      paste0(round(sum(retSMA[retSMA < 0]) / nrow(retSMA[retSMA < 0]), 4)),
      "Average Loss",
      icon=icon("credit-card"),
      color="purple")
  })
  
  output$totalProfitsBox <- renderValueBox({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    retSMA <- ROC(ticker_SMA) * sig
    valueBox(
      paste0(round(sum(na.omit(retSMA)), 4)),
      "Total Profits",
      icon=icon("credit-card"),
      color="purple")
  })
  
  output$plot1 <-renderPlot({         
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    rm(ticker_lookup2)       
    chartSeries(ticker_lookup)  
    addMACD() #Add Moving Average Convergence Divergence
  })
  
  output$plot2 <-renderPlot({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_SMA <- xts(as.numeric(Cl(ticker_data)),
                      order.by=as.Date(ticker_data$date))
    rm(ticker_lookup2)
    # Create indicator
    sma <- SMA(ticker_SMA, n=1)
    # Construct trading rule
    sig <- Lag(ifelse(sma$SMA < input$buySell, 1, -1))
    # The trading rules/equity curve
    Trading_BackTest_Returns <- ROC(ticker_SMA) * sig
    plot(Trading_BackTest_Returns)
  })
  
  output$plot3 <-renderPlot({
    ticker_lookup <- getSymbols(ticker(), auto.assign=FALSE, from = start_date(), to = end_date() )       
    ticker_lookup2<- as.data.frame(cbind(ticker_lookup[,1], ticker_lookup[,2], ticker_lookup[,3], ticker_lookup[,4], ticker_lookup[,5], ticker_lookup[,6]))
    ticker_data <-  as.data.frame(cbind(date = rownames(ticker_lookup2), ticker_lookup2))
    colnames(ticker_data) <- c("date","open","high ","low ","close ","volume ","adjusted") 
    rownames(ticker_data) <- NULL
    ticker_Garch <- zoo(ticker_data)
    rm(ticker_lookup2)
    # predict future high volatility prices
    y <- data.frame(ticker_Garch[,2])
    rownames(y) <- ticker_Garch[,1]
    spec = ugarchspec()
    nrow(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1,
                     Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1,
                     MEX = 0:1, DISTR = 1:10))
    spec = ugarchspec(variance.model = list(model = 'eGARCH',
                                            garchOrder = c(1, 1)),
                      distribution = 'std')
    fit = ugarchfit(spec, y[1:nrow(y), , drop = FALSE],
                    solver = 'hybrid')
    spec = getspec(fit)
    # 20 days Forecasting - GARCH(1,1) Model
    forc1 = ugarchforecast(fit, n.ahead = 20)
    U = uncvariance(fit)^0.5; U
    plot(forc1, which = 1)
  })

}

shinyApp(ui, server)