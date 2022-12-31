library(tidyverse)
library(readr)

course_data<-read_csv("Final_TimeTable_Data.csv")
View(course_data)
course_data %>% filter(Lecture.Type=="Lecture")


