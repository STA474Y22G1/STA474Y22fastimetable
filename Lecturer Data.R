## Packages
library(tidyverse)
library(chron) # to convert data to time

## Data
timetable_data<-read.csv("timetable_data.csv", na.strings=c("","NA")) %>% as_tibble()


## Data Wrangling
lecturer_data<-timetable_data %>% separate(Lecturer.in.Charge, into=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), sep = "-") %>%
  pivot_longer(cols=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), names_to="Lecturer Number",values_to = "Lecturer in Charge") %>%
  select(-c("Lecturer Number")) %>% drop_na(`Lecturer in Charge`)  %>% filter(`Lecturer in Charge`!="All") %>%
  mutate(Course = paste(Course.Code, "", Course.Title))

lecturer_data$Left_strip<- trimws(lecturer_data$`Lecturer in Charge`, which = c("left"))

lecturer_data <-subset(lecturer_data, select = -c(`Lecturer in Charge`)) %>%
  rename(`Lecturer in Charge`=Left_strip)

## Converting time slots to time variables
View(lecturer_data)

lecturer_data[['Starting.Time']] <- strptime(lecturer_data[['Starting.Time']],
                                 format = "%H:%M")

lecturer_data[['Starting.Time']] <- format(lecturer_data$Starting.Time, format = "%H:%M")
class(lecturer_data$Starting.Time)
