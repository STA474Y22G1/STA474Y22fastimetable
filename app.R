

## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(plotly)
library(readr)



##data - Tab4

#Plot 1 Data
lecturer_data<-read.csv("lecturer_data.csv") %>% rename(`Lecturer in Charge`=Lecturer.in.Charge)

lecturer_data$Starting.Time<-substring(lecturer_data$Starting.Time, 1,5)
lecturer_data$Ending.Time<-substring(lecturer_data$Ending.Time, 1,5)
lecturer_data$Starting.time <- as.POSIXct(lecturer_data$Starting.Time, format = "%H:%M")
lecturer_data$Ending.time<- as.POSIXct(lecturer_data$Ending.Time, format = "%H:%M")


#Plot 2 Data
lecturer_stat_data<-read_csv("lecturer_stat_data.csv")
###################################################################################################

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecturer View", tabName = "Lecturer View",
               icon = icon(name = "user", lib="glyphicon"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Lecturer View")),
    fluidRow(
      (box(width=4,
           selectInput("Day", label = h4("Select Day (Only for Lecturer Availability)"),
                       choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
           selectInput("Department", label = h4("Select Department"),
                       choices = sort(unique(lecturer_data$Department))))),
      box(plotlyOutput("plot1", height = 200),width=8)
      
    ),
    fluidRow(
      box(plotlyOutput("plot2", height = 400), width = 12)
    )))





server <- function(input, output) {
  data <- reactive({
    filter(lecturer_data,
           Department == input$Department,
           Day == input$Day)
  })
  output$plot1<- renderPlotly({
    fig <- plot_ly(data() , color = I("grey38")) %>%
      add_segments(x = ~Starting.time, xend = ~Ending.time, y = ~`Lecturer in Charge`, yend = ~`Lecturer in Charge`, showlegend = FALSE) %>%
      add_markers(x = ~Starting.time, y = ~`Lecturer in Charge`, name = "Starting Time", color = I("#882255"),
                  hovertemplate=paste("Course :", data()$Course,
                                      "<br> Starting Time :", data()$Starting.Time,
                                      "<br> Location :" , data()$Location)) %>%
      add_markers(x = ~Ending.time, y = ~`Lecturer in Charge`, name = "Ending Time", color = I("#0072B2"),
                  hovertemplate=paste("Course :", data()$Course,
                                      "<br> Ending Time :", data()$Ending.Time,
                                      "<br> Location :" , data()$Location)) %>%
      layout(
        title = "Lecturer Availability",
        xaxis = list(title = "Lecture Time",categoryorder = "category ascending", type = 'date',tickformat = "%H:%M ",
                     rangebreaks=
                       list(bounds=list(18, 8),
                            pattern="hour"),dtick=60*60*1000),
        yaxis = list(title = "Lecturer in Charge"),
        margin = list(l = 65), legend=list(x =0.95, y= 0.95) 
      )
    fig
    
  })
  output$plot2 <- renderPlotly({
    lecturer_stat_data %>% filter(Department==input$Department) %>%
      plot_ly(x = ~`Lecturer in Charge`, y = ~`Total Number of Courses`, name = 'Courses', type = 'scatter', mode = 'lines+markers', line = list(color ="#FF1764"), marker = list(color ="#FF1764")) %>%
      add_trace(y = ~`Total Lecture Hours`, name = 'Lecture Hours', mode = 'lines+markers', line = list(color ="#51f1e3"), marker = list(color ="#51f1e3")) %>%
      layout(hovermode = "x unified") %>%
      layout(legend = list (x =0.95, y= 0.95, title=list(text='Total Number of '))) %>%
      layout(title=list(text="Lecturer Drivers"),
             xaxis = list(title = "Lecturer", tickangle=45) ,
             yaxis = list(title = "Total Count"))
  })
}


shinyApp(ui, server)


