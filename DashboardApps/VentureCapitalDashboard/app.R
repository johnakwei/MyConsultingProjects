setwd("C:/Users/johnakwei/Dropbox/Programming/Dashboard/Venture_Capital_Dashboard")

# shinyapps::deployApp("C:/Users/johnakwei/Dropbox/Programming/Dashboard/Venture_Capital_Dashboard")

# install.packages("htmlwidgets")
# install.packages("curl")
library(htmlwidgets)
library(curl)
# DataTables
# devtools::install_github("rstudio/DT")
library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)

# Venture Capital Firm data is read in:
firmData <- read.csv("Clean_Series_A_Research_Calculations.csv", header=T)

# The Date column is formatted as time data:
firmData$Date <- strptime(firmData$Date, format="%m/%d/%Y")

# Monetary data is populated into a separate column:
firmData$InvestmentAmount <- as.numeric(gsub("[/|$|A-J|a-j|L-Z|l-z|()]","",
                                             firmData$Investment))

# Investment Type data is populated into s separate column:
firmData$InvestmentType <- gsub("[/|$|0-9|.|k|B|M]", "",
                                firmData$Investment)

# The names of the firms are assigned a variable:
firms <- unique(firmData$VentureFirm)

# Investment Types, and the monetary sums, are assigned a data table:
Investments <- aggregate(firmData$InvestmentAmount~firmData$InvestmentType,
                         FUN=sum)
names(Investments) <- c("Type", "Amount")

# Investment sums per StartUp:
StartUpInvestAvg <- tapply(firmData$InvestmentAmount,
                           firmData$StartUp, FUN=sum, na.rm=T)

# Investment sums per VC Firm:
VCInvestAvg <- tapply(firmData$InvestmentAmount,
                      firmData$VentureFirm, FUN=sum, na.rm=T)

# Not available data is removed
firmData <- na.omit(firmData)

ui <- dashboardPage(  
  dashboardHeader(title="VC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Graphs", tabName="dashboard1",
               icon=icon("dashboard")),
      menuItem("Venture Capital Search", tabName="venturesearch",
               icon=icon("dashboard")),
      menuItem("Data Table", tabName="DataTable",
               icon=icon("th")),
      menuItem("Documentation", tabName="documentation",
               icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard1",
              fluidRow(
                box(plotOutput("plot1", height=400)),
                valueBoxOutput("allInvestmentsBox")
                ),
              fluidRow(
                box(plotOutput("plot2", height=400)),
                valueBoxOutput("startupCapitalizationBox")
                ),
              fluidRow(
                box(plotOutput("plot3", height=400)),
                valueBoxOutput("firmInvestmentsBox")
                )
    ),
    tabItem(tabName="venturesearch",
            fluidRow(
              # Hide error messages
              tags$style(type="text/css", ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              box(title="Venture Capital Information Search",
                  actionButton("VCSearch", "Search"),
                  selectInput("VCSelector", "Select Venture Capital Firm:",
                              as.character(firmData$VentureFirm), width="100%"),
                  uiOutput("StartUpSelector"),
                  uiOutput("InvestTypeSelector")),
              valueBoxOutput("text1Box", width=6),
              valueBoxOutput("text2Box", width=6),
              valueBoxOutput("text3Box", width=6)
              )
            ),
    tabItem(tabName="DataTable",
            fluidRow(box(title="Venture Capital Firm Data Table",
                         width=12, datatable(firmData)))
            ),
    tabItem(tabName="documentation",
            fluidRow(box(h4(print(paste("Introduction")), height=100))),
            fluidRow(
              box(h4(print(paste("This dashboard application
                                 presents an overview of Venture
                                 Capital Firm data, and allows for
                                 specific venture capitalization
                                 case searching.")), height=100))
              ),
            fluidRow(box(h4(print(paste("Features")), height=100))),
            fluidRow(
              box(h4(print(paste("Summary Graphs: Generates graphs of
                                  Sums of Investment Types,
                                  StartUp Investments,
                                  Sums of Investment Firms.")), height=100))),
            fluidRow(
              box(h4(print(paste("Venture Capital Search:
                                 Allows searching of specific
                                 Venture Capital cases by
                                 Investment Firm, StartUp, and
                                 Investment Type. Beneath the
                                 Search area is the Data Table
                                 of the entire dataset.")), height=100))),
            fluidRow(box(h4(print(paste("About")), height=100))),
            fluidRow(
              box(h4(print(paste("Programmed in R/ShinyDashboard
                                 by John Akwei, Data Scientist.")), height=100))),
            fluidRow(
              box(h4(print(paste("ContextBase,
                                 http://contextbase.github.io")), height=100))),
            fluidRow(
              box(h4(print(paste("Source Code: https://github.com/johnakwei/
                                 MyConsultingProjects/blob/master/
                                 DashboardApps/app.R")), height=100)))
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
  
  output$allInvestmentsBox <- renderValueBox({
    valueBox(
      paste0(round(mean(Investments$Amount))),
      "The average of All VC investments per type, (in Million USD)",
      icon=icon("credit-card"),
      color="purple")
  })
  
  output$plot2 <- renderPlot({
    StartUpInvestAvg <- tapply(firmData$InvestmentAmount,
                               firmData$StartUp, FUN=sum, na.rm=T)
    plot(StartUpInvestAvg, main="Average Investment per StartUp (2434 StartUps)",
         col.main="blue", xlab="StartUp", ylab="Investment (Millions)",
         col.lab="darkblue", col=firmData$StartUp)
  })
  
  output$startupCapitalizationBox <- renderValueBox({
    valueBox(
      paste0(round(mean(StartUpInvestAvg, na.rm=T))),
      "The average total Venture Capitalization of a StartUp,
       (in Million USD)",
      icon=icon("credit-card"),
      color="purple")
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
  
  output$firmInvestmentsBox <- renderValueBox({
    valueBox(
      paste0(round(mean(VCInvestAvg, na.rm=T))),
      "The average total investments of Venture Capital Firms,
      (in Million USD)",
      icon=icon("credit-card"),
      color="purple")
  })
  
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

#   output$text1 <- renderPrint({
#     input$VCSearch
#     isolate({
#       OutputProcess <- function(VentureFirm, StartUp, InvestType) {
#         out <- firmData[firmData$VentureFirm==VentureFirm &
#                           firmData$StartUp==StartUp &
#                           firmData$InvestmentType==InvestType,]
#         as.character(out$StartUp)
#       }
#     OutputProcess(input$VCSelector, input$StartUps,input$InvestType)
#     })
#   })
  
  output$text1Box <- renderValueBox({
    input$VCSearch
    valueBox(
      isolate({
        OutputProcess <- function(VentureFirm, StartUp, InvestType) {
          out <- firmData[firmData$VentureFirm==VentureFirm &
                            firmData$StartUp==StartUp &
                            firmData$InvestmentType==InvestType,]
          as.character(out$StartUp)
        }
        OutputProcess(input$VCSelector, input$StartUps,input$InvestType)
      }),
      " ", color="purple")
  })
  
output$text2Box <- renderValueBox({
  input$VCSearch
  valueBox(
    isolate({
      OutputProcess <- function(VentureFirm, StartUp, InvestType) {
        out <- firmData[firmData$VentureFirm==VentureFirm &
                          firmData$StartUp==StartUp &
                          firmData$InvestmentType==InvestType,]
        out$InvestmentAmount
      }
      OutputProcess(input$VCSelector, input$StartUps,input$InvestType)
    }),
    "in Millions USD", color="purple")
})

output$text3Box <- renderValueBox({
  input$VCSearch
  valueBox(
    isolate({
      OutputProcess <- function(VentureFirm, StartUp, InvestType) {
        out <- firmData[firmData$VentureFirm==VentureFirm &
                          firmData$StartUp==StartUp &
                          firmData$InvestmentType==InvestType,]
        out$InvestmentType
      }
      OutputProcess(input$VCSelector, input$StartUps,input$InvestType)
    }),
    " ", color="purple")
})

}

shinyApp(ui, server)
