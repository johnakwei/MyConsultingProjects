library(shiny)
library(ggplot2)
library(ggmap)
library(leaflet)

# Subset Safety Net Clinic data for application.
dt <- read.csv("New_Safety_Net_Clinics.csv")
SNCAddress <- cbind(dt[1], dt[2], dt[5:6], dt[4])

shinyServer(
  function(input, output) {
    # County Selector
    output$CountySelector <- renderUI({
    CountyList <- subset(SNCAddress$County, SNCAddress$State==input$State)
    selectInput("County", label=h5("Select County"),
                choices=as.character(CountyList))})
    
    # Clinic Selector
    output$clinicSelector <- renderUI({
      clinicList <- subset(SNCAddress$Clinic,
                           SNCAddress$County==input$County)
        selectInput("Clinic", label=h5("Select Clinic"),
                    choices=as.character(clinicList))})
    
    # Print Clinic Name, and Address above Map:
    output$text1 <- renderPrint({
      clinicAddr <- subset(SNCAddress, SNCAddress$Clinic==input$Clinic)
      rownames(clinicAddr) <- " "
      colnames(clinicAddr) <- c(" ", " ", " ", " ", " ")
      clinicAddr})
    
    # Create Map
    output$mymap <- renderLeaflet({
      input$myExec
      isolate({
      clinicAddr <- subset(SNCAddress, SNCAddress$Clinic==input$Clinic)
      clinicAddr[] <- lapply(clinicAddr, as.character)
      clinicAddr <- as.character(clinicAddr)
      location1 <- geocode(c(clinicAddr[2], clinicAddr[4:5]), source="google")
      coord <- location1[3,]
      leaflet(coord) %>% addTiles() %>% addMarkers()
        })
      })
})