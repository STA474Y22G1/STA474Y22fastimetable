## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

#data 

overview_data1 <- read.csv("overview_data1.csv")
overview_data1$Day <- ordered(overview_data1$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))



ui <- dashboardPage(
  dashboardHeader(title="FAS Timetable"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview",
               icon = icon(name = "eye-open", lib="glyphicon")),
      selectInput("Department", label = h4("Select Department"),
                  choices = sort(unique(overview_data1$Department)))
  )),
  dashboardBody(
    box(plotlyOutput("plot1", height=300),width=6)
    
    
    
  )
)

server <- function(input, output){
  output$plot1<- renderPlotly({
    overview_data1<-overview_data1 %>% filter(Department==input$Department)
    plot_ly(overview_data1, x = ~Day, y = ~Total.Number.of.Lectures, type = 'bar',
            marker = list(color = '#CF1A7A')) %>%
      layout(
        title = "Distribution of Lectures by Day",
        xaxis = list(title = "Day",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
        yaxis = list(title = "Total Number of Lectures") 
      )
  })
}

shinyApp(ui, server)