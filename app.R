## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(randomcoloR)


# Read data files
availabilty_data <- read_csv("availability_data.csv")
lecture_hall_data <- read_csv("lecture_hall_data.csv")

# timetable_data <- read_csv("timetable_data.csv")
# lecture_hall_data <- read_csv("lecture_hall_data.csv")
# availabilty_data <- read_csv("hall_availability_data.csv")
# 
# # Lecture halls character vector
# lecture_halls_names <- unique(availabilty_data$Location)
# 
# availabilty_data$Day <- ordered(availabilty_data$Day,
#                                 c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
# 
# # Dataset for KPIs
# # Number of lectures per week
# lecture_counts <- timetable_data %>% 
#   filter(!is.na(Location) & Location != "Online" & Location != "O" & Location != "P" & Location != "T") %>%
#   group_by(Location) %>%
#   summarise(`Lecture count` = n_distinct(`Course Code`)) %>%
#   rename(`Lecture Hall` = Location) 
# 
# # Combine lecture_counts data set and data_halls data set
# lecture_hall_data <- full_join(lecture_hall_data, lecture_counts, by = "Lecture Hall")
# 
# # Bussiest Day/s of the lecture hall
# Bussiest <- availabilty_data %>%
#   group_by(Location, Day) %>% 
#   summarise(count_slots = sum(Availability)) %>% 
#   filter(count_slots == max(count_slots))  %>%
#   mutate(`Bussiest Day` = Day) 

# lecture_hall_data data set was updated using the above data


# Creating the color palette
set.seed(34) # Set random seed
color_palette <- distinctColorPalette(34)
color_palette


# Shiny app
ui <- dashboardPage(
  dashboardHeader(title="FAS Timetable 2022"),
  dashboardSidebar(
    width=200,
    sidebarMenu(
      menuItem("Lecture Halls", tabName = "Lecture Hall View",
               icon = icon(name = "home", lib="glyphicon"))
              # icon = icon(name ="fa-thin fa-landmark", "font-awesome"))
    )),

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
      box(plotOutput("heatmap", height = 588))
      )
  )
)


server <- function(input, output, session) {
  data <- reactive({
    filter(availabilty_data,
           Location == input$opt)
  })
  
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
  output$heatmap <- renderPlot({
    
   availabilty_data %>%
      filter(Location == input$opt) %>%
      mutate(Day = as.factor(Day)) %>%
      ggplot(aes(TimeSlot, Day, fill = Availability)) +
      geom_tile(color = "white", lwd = 1.0, linetype = 1) + 
      coord_fixed() +
      scale_fill_continuous(breaks = 0:1, labels = c("Vacant", "Occupied")) +
      coord_fixed() +
      labs(title = "Lecture Hall Availability")
      }) 
  
  # KPI 1
  # Number of courses per week
  output$kpi_1 <- renderValueBox({
    option_kpi1 <- lecture_hall_data %>%
       filter(`Lecture Hall` == input$opt) 
    
    valueBox(value = option_kpi1$`Lecture count`,
             subtitle = "Number of lectures per week",
             # color = "blue")
             color = "teal")
    # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
  })
  
  # KPI 2
  # Busiest Days of the Lecture Hall
  output$kpi_2 <- renderValueBox({
    option_kpi2 <- lecture_hall_data %>%
      filter(`Lecture Hall` == input$opt) 
    
    valueBox(value = option_kpi2$`Bussiest Day`,
             subtitle = "Busiest Day/s of the Lecture Hall",
             color = "olive")
            
    })
  
}

shinyApp(ui, server)
