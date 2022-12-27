## app.R ##

## loading libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)


## loading data in github
mainData <- read_csv("Final_TimeTable_Data.csv")

## preparing data sets for 1st visualization
dataSet1 <- mainData

# converting Day as factor variable
dataSet1$Day <- factor(dataSet1$Day, levels = c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday",
                                                "Sunday"))

# converting time variables as POSIXct
dataSet1$Starting.Time <- as.POSIXct(dataSet1$Starting.Time)
dataSet1$Ending.Time <- as.POSIXct(dataSet1$Ending.Time)

# creating a new column for lecture time
dataSet1 <- dataSet1 %>% mutate("Time Started" = format(as.POSIXct(dataSet1$Starting.Time,
                                                                 format = '%m/%d/%Y %H:%M:%S'),
                                                      format = '%H:%M'))
dataSet1 <- dataSet1 %>% mutate("Time Ended" = format(as.POSIXct(dataSet1$Ending.Time,
                                                               format = '%m/%d/%Y %H:%M:%S'),
                                                    format = '%H:%M'))
dataSet1 <- dataSet1 %>% unite("Lecture.Time", c("Time Started", "Time Ended"), sep = "-")


## user interface
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Course View", tabName = "course_view",
                           icon = icon(name = "book", lib="glyphicon"))
        )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "course_view",
                fluidRow(
                  box(title = "Select Filters",
                      solidHeader = TRUE,
                      width = 4,
                      
                      # user input for subject
                      checkboxGroupInput("Subject.Code", "Subject (You can choose more than one subject)", 
                                         choices = sort(unique(dataSet1$Subject.Code)), 
                                         inline = TRUE),
                      
                      # user input for academic year
                      selectInput("Academic.Year", "Academic Year", choices = c(1,2,3,4))
                  ),
                  
                      # 1st visualization
                      box(plotlyOutput("viz1", height = 300), width = 8) 
        )
      )
    )
  )
)


## server function
server <- function(input, output){
  
  # data set used for 1st visualization
dataSet2 <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code != "", "Please Select a Subject and Academic Year"))
    
    # filtering data based on user inputs
    dataSet1 %>% filter(Subject.Code %in% input$Subject.Code) %>%
                 filter(Academic.Year == input$Academic.Year)
 })
  

    # Visualization 1
  output$viz1 <- renderPlotly({
    
    plot1 <- dataSet2() %>% ggplot(aes(label1 = Course, 
                                       label2 = Lecturer.in.charge,
                                       label3 = Lecture.Time,
                                       label4 = Location)) + 
      geom_linerange(aes(x = Starting.Time, xmin = Starting.Time, 
                         xmax = Ending.Time, y = Day, color = Subject.Code), 
                   linewidth = 2, position = position_dodge(0.5)) +
      geom_point(aes(Starting.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      geom_point(aes(Ending.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 45)) + 
      labs(title = "Lecture Hours", color = "Subject") 
      
    
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2", "label3", "label4")) 
  })
  
}


## connect ui and server
shinyApp(ui, server)