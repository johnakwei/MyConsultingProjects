# setwd("C:/Users/johnakwei/Dropbox/Programming/Dashboard/Venture_Capital_Dashboard")

# shinyapps::deployApp("C:/Users/johnakwei/Dropbox/Programming/Dashboard/Venture_Capital_Dashboard")

library(shiny)
library(shinydashboard)
library(ggplot2)

firmData <- read.csv("Clean_Series_A_Research_Calculations.csv", header=T)
firmData$Date <- strptime(firmData$Date, format="%m/%d/%Y")
firmData$InvestmentAmount <- as.numeric(gsub("[/|$|A-J|a-j|L-Z|l-z|()]",
                                             "", firmData$Investment))
firmData$InvestmentType <- gsub("[/|$|0-9|.|k|B|M]", "", firmData$Investment)
firms <- unique(firmData$VentureFirm)
Investments <- aggregate(firmData$InvestmentAmount~firmData$InvestmentType,
                         FUN=sum)
names(Investments) <- c("Type", "Amount")

StartUpInvestAvg <- tapply(firmData$InvestmentAmount,
                           firmData$StartUp, FUN=sum, na.rm=T)

VCInvestAvg <- tapply(firmData$InvestmentAmount,
                      firmData$VentureFirm, FUN=sum, na.rm=T)

# TimeConfig <- firmData[firmData$Date %in%
#                        c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01",
#                          "2014-05-01", "2014-06-01", "2014-07-01", "2014-08-01",
#                          "2014-09-01", "2014-10-01", "2014-11-01", "2014-12-01",
#                          "2015-01-01", "2015-02-01", "2015-03-01", "2015-04-01",
#                          "2015-05-01", "2015-06-01", "2015-07-01", "2015-08-01",
#                          "2015-09-01", "2015-10-01", "2015-11-01", "2015-12-01"),]

OutputProcess <- function(VentureFirm, StartUp, InvestType) {
  if (VentureFirm=="All") {VentureFirm <- firmData$VentureFirm}
  if (StartUp=="All") {StartUp <- firmData$StartUp}
  if (InvestType=="All") {InvestType <- firmData$InvestmentType}
  out <- firmData[firmData$VentureFirm==VentureFirm &
                    firmData$StartUp==StartUp &
                    firmData$InvestmentType==InvestType,]
  out
}

ui <- dashboardPage(  
  dashboardHeader(title="VC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard 1", tabName="dashboard1", icon=icon("dashboard")),
      menuItem("Venture Capital Search", tabName="venturesearch",
               icon=icon("dashboard")),
      menuItem("Documentation", tabName="documentation", icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName="dashboard1",
              fluidRow(
                box(plotOutput("plot1", height=400)),
                box(print(
                  paste("The average of All VC investments per type is",
                        round(mean(Investments$Amount)), " Million USD.")),
                  height=100)
                ),
              fluidRow(
                box(plotOutput("plot2", height=400)),
                box(print(
                  paste("The average total Venture Capitalization of a StartUp is",
                        round(mean(StartUpInvestAvg, na.rm=T)),
                        " Million USD.")), height=100)
                ),
              fluidRow(
                box(plotOutput("plot3", height=400)),
                box(print(paste("The average total investments of Venture Capital Firms is",
                                round(mean(VCInvestAvg, na.rm=T)),
                                " Million USD.")), height=100)
                )
#               fluidRow(
#                 box(plotOutput("plot4", height=400))
#                 )
    ),
    tabItem(tabName="venturesearch",
            fluidRow(
              box(title="Venture Capital Information Search",
                  actionButton("VCSearch", "Search"),
                  selectInput("VCSelector", "Select Venture Capital Firm:",
                              as.character(firmData$VentureFirm),
                              selected="Norwest", width="100%"),
                  uiOutput("StartUpSelector"),
                  uiOutput("InvestTypeSelector")),
                  box(textOutput('text1'))
              )
      ),
    tabItem(tabName="documentation",
            fluidRow(
              box(h4(print(paste("Introduction")), height=100))),
            fluidRow(
              box(h4(print(paste("This dashboard application
                                 presents an overview of Venture Capital
                                 Firm data, and allows for specific 
                                 venture capitalization case
                                 searching.")), height=100))
              ),
            fluidRow(
              box(h4(print(paste("Features")), height=100))),
            fluidRow(
              box(h4(print(paste("Dashboard 1: Generates graphs of
                                  Sums of Investment Types,
                                  StartUp Investments,
                                  Sums of Investment Firms.")), height=100))),
            fluidRow(
              box(h4(print(paste("Venture Capital Search:
                                 Allows searching of specific
                                 Venture Capital cases by
                                 Investment Firm, StartUp, and
                                 Investment Type.")), height=100)))
          )
      )
  )
)

server <- function(input, output) {  
  
  output$plot1 <- renderPlot({
    ggplot(data=Investments, aes(Type, Amount)) +
      geom_bar(aes(), stat="identity", fill=rainbow(27)) +
      ggtitle("All VC Investments per Type") +
      xlab("") + ylab("Investment Sum (Millions)") +
      theme(plot.title=element_text(color="blue")) +
      theme(axis.title=element_text(color="darkblue")) +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  
  output$plot2 <- renderPlot({
    StartUpInvestAvg <- tapply(firmData$InvestmentAmount,
                               firmData$StartUp, FUN=sum, na.rm=T)
    plot(StartUpInvestAvg, main="Average Investment per StartUp (2434 StartUps)",
         col.main="blue", xlab="StartUp", ylab="Investment (Millions)",
         col.lab="darkblue", col=firmData$StartUp)
  })
  
  output$plot3 <- renderPlot({
    VCInvestAvg <- tapply(firmData$InvestmentAmount,
                          firmData$VentureFirm, FUN=sum, na.rm=T)
    plot(VCInvestAvg,
         main="Average Investment per Venture Capital Firm (115 VCs)",
         col.main="blue", xlab="Venture Capital Firm",
         ylab="Investments (Millions)", col.lab="darkblue",
         col=firmData$VentureFirm)
  })
  
#   output$plot4 <- renderPlot({
#     TimeConfig <- firmData[firmData$Date %in%
#                              c("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01",
#                                "2014-05-01", "2014-06-01", "2014-07-01", "2014-08-01",
#                                "2014-09-01", "2014-10-01", "2014-11-01", "2014-12-01",
#                                "2015-01-01", "2015-02-01", "2015-03-01", "2015-04-01",
#                                "2015-05-01", "2015-06-01", "2015-07-01", "2015-08-01",
#                                "2015-09-01", "2015-10-01", "2015-11-01", "2015-12-01"),]
#     
#     ggplot(TimeConfig, aes(TimeConfig$Date, TimeConfig$InvestmentAmount)) +
#       stat_summary(geom="line", fun.y="sum", color="green") +
#       geom_point(shape=1, col="green") +
#       xlab("") + ylab("VC Investments (Millions)") +
#       ggtitle("All Venture Capital Investments 2014-2015") +
#       theme(plot.title=element_text(color="blue")) +
#       theme(axis.title=element_text(color="darkblue"))
#     })
  
  output$StartUpSelector <- renderUI({
    StartUpList <- subset(firmData$StartUp,
                          firmData$VentureFirm==input$VCSelector)
    selectInput("StartUps", "Select StartUp:",
                as.character(StartUpList))})
  
  output$InvestTypeSelector <- renderUI({
    InvestList <- subset(firmData$InvestmentType,
                         firmData$VentureFirm==input$VCSelector)
    InvestList <- subset(firmData$InvestmentType,
                         firmData$StartUp==input$StartUps)
    selectInput("InvestType", "Select Investment Type:",
                as.character(InvestList))})

  output$text1 <- renderPrint({
    input$VCSearch
    isolate({
      OutputProcess <- function(VentureFirm, StartUp, InvestType) {
        if (VentureFirm=="All") {VentureFirm <- firmData$VentureFirm}
        if (StartUp=="All") {StartUp <- firmData$StartUp}
        if (InvestType=="All") {InvestType <- firmData$InvestmentType}
        out <- firmData[firmData$VentureFirm==VentureFirm &
                          firmData$StartUp==StartUp &
                          firmData$InvestmentType==InvestType,]
        out
      }
    OutputProcess(input$VCSelector, input$StartUps,
                            input$InvestType)})
  })
  
}

shinyApp(ui, server)
