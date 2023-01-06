## app.R ##

## loading libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(shinyWidgets)
library(DT)

## loading data in github
mainData <- read_csv("Course_Data.csv")

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

# creating new column for degree type
dataSet1 <- dataSet1 %>% pivot_longer(c("General", "Special"), "Degree.Type", "value") %>%
            filter(value == 1)

# creating new column for stream
dataSet1 <- dataSet1 %>% mutate(Stream = ifelse(Physical == 1 & Bio == 0 & Food == 0 & Sports.Science == 0, "Physical",
                                           ifelse(Physical == 0 & Bio == 1 & Food == 0 & Sports.Science == 0, "Biology",
                                           ifelse(Physical == 0 & Bio == 0 & Food == 1 & Sports.Science == 0, "Food Science",
                                            ifelse(Physical == 0 & Bio == 0 & Food == 0 & Sports.Science == 1, "Sports Science",
                                            ifelse(Physical == 1 & Bio == 1 & Food == 0 & Sports.Science == 0, "Physical/Biology", "No Stream"))))))

## creating final data set for course view
courseViewData <- dataSet1 %>% select(-c(Index, Physical, Bio, Food, Sports.Science,
                                         Number.of.Credits, Semester.Half, value))

# to remove select all option in pickerInput function 
my_css <- "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  width: 100%;
}
"

## user interface
ui <- dashboardPage(
  
    dashboardHeader(),
    
    dashboardSidebar(
        sidebarMenu(
          menuItem(" Course View", tabName = "course_view",
                   icon = icon(name = "book", lib="glyphicon"))
                 )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "course_view",
                fluidRow(
                  box(width = 3,
                      height = 270,
                      
                      ## user input 1 degree
                      selectInput(
                        inputId = "Degree.Type",
                        label = "Select Degree", 
                        choices = unique(courseViewData$Degree.Type)),
                      
                      ## user input 2 year
                      selectInput(
                        inputId = "Academic.Year",
                        label = "Select Academic Year", 
                        choices = NULL),
                      
                      ## user input 3 subject
                      tags$head(tags$style(HTML(my_css))),
                      
                      pickerInput(
                        inputId = "Subject.Code",
                        label = "Select Subject/s", 
                        choices = NULL,
                        options = list(`actions-box` = TRUE, size = 5),
                        multiple = TRUE
                      )
                      
                    ),
                  
                  # 1st visualization
                  box(plotlyOutput("viz1", height = 250), width = 9, height = 270),
                  
                  # Course finder table
                  box(dataTableOutput("CourseData"), width = 12))
           )
         )
      )
)




## server function
server <- function(input, output, session){
  
  # updating filters
  degree <- reactive({
    req(input$Degree.Type)
    filter(courseViewData, Degree.Type == input$Degree.Type)
  })
  
  year <- reactive({
    req(input$Academic.Year)
    filter(degree(), Academic.Year == input$Academic.Year)
  })
  
  subject <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code)
    filter(year(), Subject.Code %in% input$Subject.Code)
  })
  
  # observing event to update next filter
  observeEvent(degree(), {
    updateSelectInput(session, "Academic.Year", 
                      choices = sort(unique(degree()$Academic.Year)), selected = 1)
  })
  
  observeEvent(year(), {
    updatePickerInput(session, "Subject.Code", 
                      choices = sort(unique(year()$Subject.Code)), selected = c("STA","FST"))
  })
  
  # Visualization 1
  output$viz1 <- renderPlotly({
    
    plot1 <- subject() %>% ggplot(aes(label1 = Course, 
                                      label2 = Lecture.Time)) + 
      geom_linerange(aes(x = Starting.Time, xmin = Starting.Time, 
                         xmax = Ending.Time, y = Day, color = Subject.Code), 
                     linewidth = 2, position = position_dodge(0.5)) +
      geom_point(aes(Starting.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      geom_point(aes(Ending.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) + 
      theme_bw() +
      labs(title = "Lecture Hours", color = "Subject") 
    
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))
      
  })
  
  # course finder
  # Data set for course finder
  course <- reactive({
    subject() %>% filter(Lecture.Type == "Lecture") %>% 
      select(Department, Stream, Day, Lecture.Time,  Course.Code, Course.Title, 
             Lecturer.in.charge, Location) 
  })
  
  
  # Course finder table
  output$CourseData <- renderDataTable(course(),
                                       options = list(paging = TRUE,
                                                      pageLength = 5,
                                                      dom = 'ftip'),
                                       colnames = c('Lecture Time' = 'Lecture.Time',
                                                    'Course Code' = 'Course.Code',
                                                    'Course Title' = 'Course.Title',
                                                    'Lecturer in Charge' = 'Lecturer.in.charge'),
                                       caption = htmltools::tags$caption(
                                         style = 'caption-side: top; text-align: left; font-style: normal; 
                                         color: black; font-family: arial; font-size: 1.8rem;',
                                         htmltools::em('Course Finder'))
  )
  
  
}


## connect ui and server
shinyApp(ui, server)