## app.R ##

## loading libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(magrittr)


## loading data in github
mainData <- read_csv("https://raw.githubusercontent.com/STA474Y22G1/STA474Y22fastimetable/T3/Final_TimeTable_Data.csv")


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
dataSet1 <- dataSet1 %>% unite("Lecture Time", c("Time Started", "Time Ended"), sep = "-")


# user interface
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
        tabItem(tabName = "course_view")
        )
      
    )
)

# server
server <- function(input, output){}

# connect ui and server
shinyApp(ui, server)