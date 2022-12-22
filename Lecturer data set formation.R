
## Packages
library(tidyverse)
library(lubridate)
library(chron)
library(hms)

## Data
timetable_data<-read.csv("timetable_data.csv", na.strings=c("","NA")) %>% as_tibble()


## Data Wrangling
lecturer_data<-timetable_data %>% separate(Lecturer.in.Charge, into=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), sep = "-") %>%
  pivot_longer(cols=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), names_to="Lecturer Number",values_to = "Lecturer in Charge") %>%
  select(-c("Lecturer Number")) %>% drop_na(`Lecturer in Charge`)  %>% filter(`Lecturer in Charge`!="All") 


lecturer_data$Left_strip<- trimws(lecturer_data$`Lecturer in Charge`, which = c("left"))

lecturer_data <-subset(lecturer_data, select = -c(`Lecturer in Charge`)) %>%
  rename(`Lecturer in Charge`=Left_strip)

View(lecturer_data)
######################################################################################

## Converting time variables

# Starting time
lecturer_data$Starting.Time<-hms(hours = as.numeric(substr(lecturer_data$Starting.Time, start = 1, stop = 2)),
    minutes=as.numeric(substr(lecturer_data$Starting.Time, start = 4, stop = 5)))


#Ending time
lecturer_data$Ending.Time<-hms(hours = as.numeric(substr(lecturer_data$Ending.Time, start = 1, stop = 2)),
                                 minutes=as.numeric(substr(lecturer_data$Ending.Time, start = 4, stop = 5)))




