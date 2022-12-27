## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(randomcoloR)


# Read data files
timetable_data <- read_csv("timetable_data.csv")
lecture_hall_data <- read_csv("lecture_hall_data.csv")
availabilty_data <- read_csv("DataSet_for_Lecture_Hall_View.csv")

# Lecture halls character vector
lecture_halls_names <- unique(availabilty_data$Location)

# Dataset for KPIs
# Number of lectures per week
lecture_counts <- timetable_data %>% 
  filter(!is.na(Location) & Location != "Online" & Location != "O" & Location != "P" & Location != "T") %>%
  group_by(Location) %>%
  summarise(`Lecture count` = n_distinct(`Course Code`)) %>%
  rename(`Lecture Hall` = Location)

# Combine lecture_counts data set and data_halls data set
lecture_hall_data <- full_join(lecture_hall_data, lecture_counts, by = "Lecture Hall")

# Creating the color palette
set.seed(34) # Set random seed
color_palette <- distinctColorPalette(34)
color_palette


# Shiny app
ui <- dashboardPage(
  dashboardHeader(title="FAS Timetable 2022"),
  dashboardSidebar(width=200,
                   sidebarMenu(h3(HTML("Lecture Halls")))
  ),
  dashboardBody(
      # Treemap 
      box(plotlyOutput("treemap", height = 1000)),
      
      fluidRow(
        box(
          selectInput("opt", label = h2("Select Lecture Hall:"),
                      choices = lecture_halls_names)
        ),
        
      # KPI 1  
      # Number of courses per week
      valueBoxOutput("kpi_1", width = 6),
        
      # KPI 2
      valueBoxOutput("kpi_2", width = 6),
        
      # Heetmap
      box(plotlyOutput("heatmap", height = 588))  
      )
  )
)


server <- function(input, output, session) {
  # Treemap
  output$treemap <- renderPlotly({
    plot_ly(
          data = lecture_hall_data,
          type="treemap",
          source = "treemapplot",
          labels = ~ `Lecture Hall`,
          parents = ~ "Lecture Hall",
          values = ~ `Seating Capacity`,
          domain = list(column = 0),
          textinfo = "label+value",
          colors = color_palette)
      })
  
  # Heetmap
  output$heatmap <- renderPlotly({
   availabilty_data %>%
      filter(Location == input$opt) %>%
      plot_ly(
        x = ~TimeSlot, 
        y = ~Day,
        z = ~Availability,
        colors = colorRamp(c("#d5ebdd", "#a7aba9")), 
        type = "heatmap", 
        showscale = FALSE) %>% 
      layout(yaxis = list(categoryorder="trace")) %>% 
      layout(title="Lecture Hall Availability", 
             xaxis=list(title="Time Slot"), yaxis=list(title="Day of the Week")) %>%
      layout(hoverlabel = list(bgcolor = "white",
                               font = list(color = "black"))) 

      }) 
  
  # KPI 1
  # Number of courses per week
  output$kpi_1 <- renderValueBox({
    option_kpi1 <- lecture_hall_data %>%
       filter(`Lecture Hall` == input$opt) 
    
    valueBox(value = option_kpi1$`Lecture count`,
             subtitle = "Number of lectures per week",
             color = "blue")
  })
  
  # KPI 2
  # Busiest Days of the Lecture Hall
  output$kpi_2 <- renderValueBox({
    option_kpi2 <- availabilty_data %>%
      filter(Location == input$opt) %>%
      group_by(Day) %>% 
      summarise(count_slots = sum(Availability)) %>% 
      filter(count_slots == max(count_slots)) %>%
      select(Day)
    
    if(length(option_kpi2$Day == 1)){
      string_value = option_kpi2$Day
    }else{
      string_value = "More than one busy dates" 
    }
    
    valueBox(value = string_value,
             subtitle = "Busiest Days of the Lecture Hall",
             color = "navy")
  })
  
}

shinyApp(ui, server)































