
## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(plotly)
library(readr)



#data - Tab4 
lecturer_data<-read_csv("lecturer_data.csv") 
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
    box(width=12,
        selectInput("Day", label = h4("Select Day (Only for Lecturer Availability)"), 
                    choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
        selectInput("Department", label = h4("Select Department"), 
                    choices = unique(lecturer_data$Department))),
    fluidRow(
      box(plotlyOutput("plot1", height = 400))
      ,
      box((plotlyOutput("plot2", height = 400))))
  ))     
      
  
 


server <- function(input, output) {
  output$plot1<- renderPlotly({
    lecturer_data <- lecturer_data %>%  filter(Department==input$Department) %>% filter(Day==input$Day) 
    fig <- plot_ly(data = lecturer_data, color = I("gray80")) %>% add_segments(x = ~Starting.Time, xend = ~Ending.Time, y = ~`Lecturer in Charge`, yend = ~`Lecturer in Charge`, showlegend = FALSE) %>%
      add_markers(x = ~Starting.Time, y = ~`Lecturer in Charge`, name = "Starting Time", color = I("pink")) %>%
      add_markers(x = ~Ending.Time, y = ~`Lecturer in Charge`, name = "Ending Time", color = I("blue")) %>%
      layout(
        title = "Lecturer Availability",
        xaxis = list(title = "Lecture Time",categoryorder = "category ascending"),
        yaxis = list(title = "Lecturer in Charge"),
        margin = list(l = 65)
      )
    fig
  })
  output$plot2 <- renderPlotly({
    lecturer_stat_data %>% filter(Department==input$Department) %>%
      plot_ly(x = ~`Lecturer in Charge`, y = ~`Total Number of Courses`, name = 'Courses', type = 'scatter', mode = 'lines+markers', line = list(color ="#FF1764"), marker = list(color ="#FF1764")) %>%
      add_trace(y = ~`Total Lecture Hours`, name = 'Lecture Hours', mode = 'lines+markers', line = list(color ="#51f1e3"), marker = list(color ="#51f1e3")) %>%
      layout(hovermode = "compare") %>%
      layout(legend = list (x =0.06, y= 0.95, title=list(text='Total Number of '))) %>%
      layout(title=list(text="Lecturer Drivers", size=14),
             xaxis = list(title = "Lecturer", tickangle=45, dtick = "M1") ,
             yaxis = list(title = "Total Count")) 
  })
}


shinyApp(ui, server)


