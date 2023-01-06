## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

#data 

overview_data1 <- read.csv("overview_data1.csv")
overview_data1$Day <- ordered(overview_data1$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# data set for KPIs
overview_kpi_data <- read.csv("Course_Data2.csv")
academic_kpi_data <- overview_kpi_data %>%
  filter(Lecture.Type == "Lecture") %>%
  group_by(Department, Academic.Year) %>%
  select(Department, Academic.Year, Course.Code) %>%
  summarise(Course.Count = n_distinct(Course.Code))

all_kpi <- academic_kpi_data %>% group_by(Academic.Year) %>%
  summarise(Course.Count = sum(Course.Count)) 

all_department <- data.frame(Department = c("All", "All", "All", "All"),
                             Academic.Year = c(1,2,3,4))

all_kpi <- left_join(all_department, all_kpi, by = "Academic.Year")

cs_df <- data.frame(Department = "Computer Science", Academic.Year = 4, 
                    Course.Count = 0)

academic_kpi_data <- bind_rows(academic_kpi_data, cs_df)
academic_kpi_data <- bind_rows(academic_kpi_data, all_kpi)




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
    fluidRow( 
      #kp1
      valueBoxOutput("overview_kpi_1", width=3),
      
      #kpi2
      valueBoxOutput("overview_kpi_2", width=3),
      
      #kpi3
      valueBoxOutput("overview_kpi_3", width=3), 
      
      #kpi4
      valueBoxOutput("overview_kpi_4", width=3), 
      
      box(plotlyOutput("plot1", height=450),width=6))
   
    
    
    
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
  
  # KPIs
  # KPI 1
  # Number of lectures for 1st year
  output$overview_kpi_1 <- renderValueBox({
    option_kpi1 <- academic_kpi_data %>%
      filter(Department == input$Department & Academic.Year == 1) 
    
    valueBox(value = option_kpi1$Course.Count,
             subtitle = "Number of lectures for 1st year",
             color = "purple")
  })
  
  # KPI 2
  # Number of lectures for 2nd year
  output$overview_kpi_2 <- renderValueBox({
    option_kpi2 <- academic_kpi_data %>%
      filter(Department == input$Department & Academic.Year == 2) 
    
    valueBox(value = option_kpi2$Course.Count,
             subtitle = "Number of lectures for 2nd year",
             color = "purple")
  })
  
  # KPI 3
  # Number of lectures for 3rd year
  output$overview_kpi_3 <- renderValueBox({
    option_kpi3 <- academic_kpi_data %>%
      filter(Department == input$Department & Academic.Year == 3) 
    
    valueBox(value = option_kpi3$Course.Count,
             subtitle = "Number of lectures for 3rd year",
             color = "purple")
  })
  
  # KPI 4
  # Number of lectures for 4th year
  output$overview_kpi_4 <- renderValueBox({
    option_kpi4 <- academic_kpi_data %>%
      filter(Department == input$Department & Academic.Year == 4) 
    
    valueBox(value = option_kpi4$Course.Count,
             subtitle = "Number of lectures for 4th year",
             color = "purple")
  })
    
}

shinyApp(ui, server)