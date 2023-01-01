## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

#data 

overview_data <- read.csv("overview_data.csv")
overview_data$Day <- ordered(overview_data$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))



ui <- dashboardPage(
  dashboardHeader(title="FAS Timetable"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview",
               icon = icon(name = "eye-open", lib="glyphicon")),
      selectInput("Department", label = h4("Select Department"),
                  choices = sort(unique(data$Department)))
  )),
  dashboardBody(
    box(plotlyOutput("plot1", height=300),width=12)
    
    
    
  )
)

server <- function(input, output){
  output$plot1<- renderPlotly({
    overview_data<-filter(overview_data,Department==input$Department)
    plot_ly(overview_data, x = ~Day, y = ~Total.Number.of.Lectures, type = 'bar',
            marker = list(color = '#CF1A7A')) %>%
      layout(
        title = "Distribution of Lectures by Day",
        xaxis = list(title = "Day",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
        yaxis = list(title = "Total Number of Lectures") 
      )
  })
}

shinyApp(ui, server)