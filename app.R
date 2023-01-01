## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(plotly)
library(readr)

#data 

data <- read_csv("overview_data.csv")
data$Day <- ordered(data$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview",
               icon = icon(name = "user", lib="glyphicon"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview")),
    fluidRow(
      (box(width=4,
           selectInput("Department", label = h4("Select Department"),
                       choices = sort(unique(data$Department))))),
      box(plotlyOutput("plot1", height = 200),width=8)
      
    ),
  
    ))
server <- function(input, output) {
  data <- reactive({
    filter(data(),
           Department == input$Department)
           
  })
  
  output$plot1<- renderPlotly({
    fig <- plot_ly(data(), x = ~Day, y = ~'Total Number of Lectures', type = 'bar',
                   marker = list(color = '#CF1A7A')) %>%
      layout(
        title = "Distribution of Lectures by Day",
        xaxis = list(title = "Day",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
        yaxis = list(title = "Total Number of Lectures") 
      )
    fig
  })
}
    shinyApp(ui, server)