## Packages
library(tidyverse)
library(lubridate)
library(chron)
library(hms)

## Data 
timetable_cleaned_data<-read.csv("timetable_cleaned_data.csv", na.strings=c("","NA")) %>% as_tibble()
View(timetable_cleaned_data)

## Data Wrangling
lecturer_data<-timetable_cleaned_data %>% drop_na(Lecturer.in.Charge)  %>% 
  filter(Lecturer.in.Charge!="All") %>%rename(`Lecturer in Charge`=Lecturer.in.Charge)

View(lecturer_data)
######################################################################################

## Converting time variables

# Starting time
lecturer_data$Starting.Time<-hms(hours = as.numeric(substr(lecturer_data$Starting.Time, start = 1, stop = 2)),
                                 minutes=as.numeric(substr(lecturer_data$Starting.Time, start = 4, stop = 5)))


#Ending time
lecturer_data$Ending.Time<-hms(hours = as.numeric(substr(lecturer_data$Ending.Time, start = 1, stop = 2)),
                               minutes=as.numeric(substr(lecturer_data$Ending.Time, start = 4, stop = 5)))

## Creating lecture duration variable

lecturer_data$`Lecturing Hours`<-difftime(lecturer_data$Ending.Time, lecturer_data$Starting.Time, units="hours") %>%
  as.numeric() %>% round(2)





