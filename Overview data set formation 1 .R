library(tidyverse)
library(readr)

course_data<-read_csv("Course_Data.csv")
View(course_data)
course_data<-course_data %>% filter(Lecture.Type=="Lecture")%>%drop_na(Day)


## Making time slot variable
course_data$`Time Slot`<-paste(course_data$Starting.Time, "-", course_data$Ending.Time)


## Grouping data for departments
grouped_data<-course_data %>% group_by(Day,Lecturer.in.charge, Department, `Time Slot`) %>%
  summarise(a=length(Course))%>%
  mutate(Course.Number=ifelse(a==1,a,1)) 

View(grouped_data)

overview_dept_data<-grouped_data %>% group_by(Day, Department) %>%
  summarise(`Total Number of Lectures`=sum(Course.Number))

View(overview_dept_data)

## Grouping data for faculty
grouped_fac_data<-course_data %>% group_by(Day,Lecturer.in.charge, `Time Slot`) %>%
  summarise(a=length(Course))%>%
  mutate(Course.Number=ifelse(a==1,a,1)) 

View(grouped_fac_data)

overview_fac_data<-grouped_fac_data %>% group_by(Day) %>%
  summarise(`Total Number of Lectures`=sum(Course.Number))

overview_fac_data$Department<-rep("All",7)

overview_fac_data<-overview_fac_data %>% relocate(Department, .before = `Total Number of Lectures`)

View(overview_fac_data)

## Overview data
overview_data<-rbind(overview_fac_data,overview_dept_data)

View(overview_data)

write_csv(overview_data,"overview_data.csv")
