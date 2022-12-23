## app.R ##

# loading libraries
library(shinydashboard)
library(shiny)

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