setwd("C:/Users/johnakwei/Dropbox/Programming/Dashboard/ContextBase_2nd_Dashboard")

# install.packages("shiny")
# install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)
absorbance1 <- read.csv("Absorbances1.csv", header=T)

ui <- dashboardPage(  
  dashboardHeader(title="Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard 1", tabName="dashboard", icon=icon("dashboard")),
      menuItem("Dashboard 2", tabName="dashboard2", icon=icon("th")),
      dropdownMenu(type = "messages",
                   messageItem(
                     from = "Marketing Dept",
                     message = "Sales are steady this month."
                   ),
                   messageItem(
                     from = "New Client",
                     message = "Request new project",
                     icon = icon("question"),
                     time = "13:45"
                   ),
                   messageItem(
                     from = "Project Management",
                     message = "Completion of new Milestone",
                     icon = icon("life-ring"),
                     time = "2014-12-01"
                   )
      )
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName="dashboard",
              fluidRow(
                box(plotOutput("plot1", height=300)),
                box(plotOutput("plot2", height=300))
                ),
              fluidRow(
                box(plotOutput("plot3", height=250)),
                box(
                  title="Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              fluidRow(
                box(plotOutput("plot4", height=300)),
                box(
                  title="Controls",
                  radioButtons("radioButtons", "Number of observations:",
                                     c("1"=1, "50"=50, "100"=100),
                                     selected="50", width="100%")
                )
              )
      ),
      # Second tab content
      tabItem(tabName="dashboard2",
              h2("Dashboard 2"),
              fluidRow(
                box(plotOutput("plot5", height=400)),
                box(
                  title="Controls",
                  radioButtons("radioButtons2", "Number of observations:",
                               c("100"=100, "500"=500, "1000"=1000),
                               selected="500", width="100%")
                )
              ),
              fluidRow(
                box(plotOutput("plot6", height=400)),
                box(
                  title="Absorbance Graph Controls",
                  radioButtons("radioButtons3", "Select Sample 1-7:",
                               c("Sample 1"="absorbance1$Sample_1",
                                 "Sample 2"="absorbance1$Sample_2",
                                 "Sample 3"="absorbance1$Sample_3",
                                 "Sample 4"="absorbance1$Sample_4",
                                 "Sample 5"="absorbance1$Sample_5",
                                 "Sample 6"="absorbance1$Sample_6",
                                 "Sample 7"="absorbance1$Sample_7"),
                               selected="absorbance1$Sample_1", width="100%")
                )
              )
              )
    )
  )
)

server <- function(input, output) {  
  
  output$plot1 <- renderPlot({
    dataset <- matrix(nrow=20, ncol=4)
    
    colnames(dataset) <- c("Year", "Sales", "Profits", "Costs")
    
    dataset[3,] <- c(2002, 490, 85.46, 404.5)
    dataset[4,] <- c(2003, 425, 76.84, 348.2)
    dataset[5,] <- c(2004, 480, 89.86, 390.1)
    dataset[6,] <- c(2005, 480, 82.18, 397.8)
    dataset[7,] <- c(2006, 405, 71.93, 333.1)
    dataset[8,] <- c(2007, 430, 66.74, 363.3)
    dataset[9,] <- c(2008, 485, 83.03, 402.0)
    dataset[10,] <- c(2009, 475, 88.16, 386.8)
    dataset[11,] <- c(2010, 425,  55.76, 369.2)
    dataset[12,] <- c(2011, 430,  69.49, 360.5)
    
    dataset <- data.frame(dataset)
    
    plot_sales <- ggplot(dataset,aes(x=factor(Year),y=Sales,fill=Sales)) +
      xlab('Year') + ylab('Sales ($)') +
      geom_bar(stat='identity',colour='black') +
      scale_fill_gradient(low='red',high='green') +
      ggtitle('Yearly sales') +
      theme(axis.text.x = element_text(angle = -35))
    
    plot_sales
  })
  
  output$plot2 <- renderPlot({
    dataset <- matrix(nrow=20, ncol=4)
    
    colnames(dataset) <- c("Year", "Sales", "Profits", "Costs")
    
    dataset[3,] <- c(2002, 490, 85.46, 404.5)
    dataset[4,] <- c(2003, 425, 76.84, 348.2)
    dataset[5,] <- c(2004, 480, 89.86, 390.1)
    dataset[6,] <- c(2005, 480, 82.18, 397.8)
    dataset[7,] <- c(2006, 405, 71.93, 333.1)
    dataset[8,] <- c(2007, 430, 66.74, 363.3)
    dataset[9,] <- c(2008, 485, 83.03, 402.0)
    dataset[10,] <- c(2009, 475, 88.16, 386.8)
    dataset[11,] <- c(2010, 425,  55.76, 369.2)
    dataset[12,] <- c(2011, 430,  69.49, 360.5)
    
    dataset <- data.frame(dataset)
    
    plot_profits <- ggplot(dataset,aes(x=factor(Year),y=Profits,fill=Profits)) +
      xlab('Year') + ylab('Profits ($)') +
      geom_bar(stat='identity',colour='black') +
      scale_fill_gradient(low='red',high='green') +
      ggtitle('Yearly profits') +
      theme(axis.text.x = element_text(angle = -35))
    
    plot_profits
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot3 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot4 <- renderPlot({
    data2 <- histdata[seq_len(input$radioButtons)]
    hist(data2)
  })
  
  output$plot5 <- renderPlot({
    set.seed(100)
    d <- diamonds[sample(nrow(diamonds), input$radioButtons2), ]
    
    ggplot(data=d, aes(x=carat, y=price)) +
      geom_point(aes(text=paste("Clarity:", clarity)), size=4) +
      geom_smooth(aes(colour=cut, fill=cut)) + facet_wrap(~cut)
  })
  
  output$plot6 <- renderPlot({
    absorbance1$Powder_mg_vs_Absorbance_E_coli <-
      as.numeric(absorbance1$Powder_mg_vs_Absorbance_E_coli)    
    ggplot(absorbance1,
           aes_string(x=as.factor(absorbance1$Powder_mg_vs_Absorbance_E_coli),
                            y=input$radioButtons3)) +
      geom_boxplot(aes(fill=as.factor(absorbance1$Powder_mg_vs_Absorbance_E_coli))) +
      ggtitle("E.Coli Absorbance per MG - Medians") +
      xlab("Quantities in Milligrams") + ylab("E.Coli Absorbance") +
      labs(fill="Milligrams")
  })
}

shinyApp(ui, server)
