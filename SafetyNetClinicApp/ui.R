library(shiny)
library(ggplot2)
library(ggmap)
library(leaflet)

# Subset Safety Net Clinic data for application.
dt <- read.csv("New_Safety_Net_Clinics.csv")
SNCAddress <- cbind(dt[1], dt[2], dt[5:6], dt[4])

shinyUI(navbarPage("Safety Net Clinic Map",
            tabPanel("Application",
                sidebarLayout(
                    sidebarPanel(
                        # CSS UI formatting
                        tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap.css")),
                        
                        # Hide error messages
                        tags$style(type="text/css", ".shiny-output-error {visibility: hidden;}",
                                    ".shiny-output-error:before {visibility: hidden;}"),
                        
                        # List Boxes
                        selectInput("State", label=h5("Select State"), 
                                    choices=as.character(SNCAddress$State), selected=as.character(1)),
                        uiOutput("CountySelector"),
                        uiOutput("clinicSelector"),
                        actionButton("myExec", "Display Map")
                                ),
                      mainPanel(
                        h3('Clinic Address:', style="color:red"),
                        h4(textOutput('text1')),
                        leafletOutput("mymap")
                      )
                   )),
            tabPanel("Documentation",
                    mainPanel(
                        strong("Introduction"),
                        br(),
                              
                        p("The Application will plot Safety Net Clinics, (from 'https://catalog.data.gov/dataset/safety-net-clinics'),
                            based upon the State that the User selects."),
                        br(),
                              
                        strong("Features"),
                        br(),
                              
                        strong(em("Map Type")),
                        p("The Application generates a zoomable Road Map, of the location set."),
                        br(),
                              
                        strong(em("Zoom")), 
                        p("The zoom controls are on the top left corner of the Map."),
                        br(),
                              
                        strong(em("Display Map")),
                        p("Press the 'Display Map' button to load the Map.")
                            )
                    )
    ))