library(tidyr) 
library(dplyr)
library(readr)
library(tidyverse)

course_data <- read_csv("Course_Data2.csv")

View(course_data) 

##############################################################################

## Fixing Time slot variable
course_data$Start.Time <- format(as.POSIXct(course_data$Starting.Time, tz = "GMT"), format = "%H:%M") 
course_data$End.Time <- format(as.POSIXct(course_data$Ending.Time, tz = "GMT"), format = "%H:%M") 


course_data$"08:00-08:50"<- factor(ifelse(course_data$Start.Time <= "08:00" & course_data$End.Time >= "08:50", 1,0))
course_data$"08:55-09:45"<- factor(ifelse(course_data$Start.Time <= "08:55" & course_data$End.Time >= "09:45", 1,0))
course_data$"10:15-11:05"<- factor(ifelse(course_data$Start.Time <= "10:15" & course_data$End.Time >= "11:05", 1,0))
course_data$"11:10-12:00"<- factor(ifelse(course_data$Start.Time <= "11:10" & course_data$End.Time >= "12:00", 1,0))
course_data$"13:00-14:00"<- factor(ifelse(course_data$Start.Time <= "13:00" & course_data$End.Time >= "14:00", 1,0))
course_data$"14:00-15:00"<- factor(ifelse(course_data$Start.Time <= "14:00" & course_data$End.Time >= "15:00", 1,0))
course_data$"15:00-16:00"<- factor(ifelse(course_data$Start.Time <= "15:00:" & course_data$End.Time >= "16:00", 1,0))
course_data$"16:00-17:00"<- factor(ifelse(course_data$Start.Time <= "16:00" & course_data$End.Time >= "17:00", 1,0))
course_data$"17:00-17:45"<- factor(ifelse(course_data$Start.Time <= "17:00" & course_data$End.Time >= "18:00", 1,0))


course_data <- course_data %>% pivot_longer(cols=c("08:00-08:50", "08:55-09:45", "10:15-11:05", "11:10-12:00", 
                               "13:00-14:00", "14:00-15:00", "15:00-16:00", "16:00-17:00", "17:00-17:45"),
                    names_to="Time Slot", values_to= "Yes.No") 


## Filtering data
course_data<-course_data %>% filter(Yes.No=="1") %>% filter(Lecture.Type=="Lecture")%>%drop_na(Day)

## Grouping data for departments
grouped_data2<-course_data%>% group_by(Day,Lecturer.in.charge, Department, `Time Slot`) %>%
  summarise(a=length(Course))%>%
  mutate(Course.Number=ifelse(a==1,a,1)) 

View(grouped_data2)

overview_dept_data2<-grouped_data2 %>% group_by(Department, Day, `Time Slot`) %>%
  summarise(`Total Number of Lectures`=sum(Course.Number))

View(overview_dept_data2)

## Grouping data for faculty
grouped_fac_data2<-course_data %>% group_by(Day,Lecturer.in.charge, `Time Slot`) %>%
  summarise(a=length(Course))%>%
  mutate(Course.Number=ifelse(a==1,a,1)) 

View(grouped_fac_data2)

overview_fac_data2<-grouped_fac_data2 %>% group_by(Day,`Time Slot`) %>%
  summarise(`Total Number of Lectures`=sum(Course.Number))

n<-nrow(overview_fac_data2)
overview_fac_data2$Department<-rep("All",n)

overview_fac_data2<-overview_fac_data2 %>% relocate(Department, .before = Day)

View(overview_fac_data2)

## Overview data
overview_data2<-rbind(overview_fac_data2,overview_dept_data2)

View(overview_data)

write_csv(overview_data2,"overview_data2.csv")

