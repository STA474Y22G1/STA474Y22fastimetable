## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

#data 

overview_data <- read.csv("overview_data.csv")
overview_data$Day <- ordered(overview_data$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data1<-overview_data %>%filter(Department=="Äll")


ui <- dashboardPage(
  dashboardHeader(title="FAS Timetable"),
  dashboardSidebar(),
  dashboardBody(
    box(plotlyOutput("plot1"), width=12, height=400)
    
    
  )
)

server <- function(input, output){
  output$plot1<- renderPlotly({
    plot_ly(data1, x = ~Day, y = ~Total.Number.of.Lectures, type = 'bar',
            marker = list(color = '#CF1A7A')) %>%
      layout(
        title = "Distribution of Lectures by Day",
        xaxis = list(title = "Day",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
        yaxis = list(title = "Total Number of Lectures") 
      )
  })
}

shinyApp(ui, server)