library(readr)

# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(plotly)
library(readr)
data<-read.csv("overview_data.csv")
data$Day <- ordered(data$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
View(data)
unique(data)
#barplot
dep <- data %>% filter(Department=="All") 
fig <- plot_ly(dep, x = ~Day, y = ~Total.Number.of.Lectures, type = 'bar') %>%
  layout(
    title = "Distribution of number of lectures by day",
    xaxis = list(title = "Lecture Time",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
    yaxis = list(title = "Total number of lectures") 
  )
fig
