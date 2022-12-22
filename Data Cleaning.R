
## Packages
library(tidyverse)

## Data
timetable_data<-read.csv("timetable_data.csv", na.strings=c("","NA")) %>% as_tibble()


## Data Wrangling
timetable_cleaned_data<-timetable_data %>% separate(Lecturer.in.Charge, into=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), sep = "-") %>%
  pivot_longer(cols=c("Lecturer 1", "Lecturer 2", "Lecturer 3"), names_to="Lecturer Number",values_to = "Lecturer in Charge") %>%
  select(-c("Lecturer Number")) 


timetable_cleaned_data$Strip<- trimws(timetable_cleaned_data$`Lecturer in Charge`, which = c("both")) 

timetable_cleaned_data<-subset(timetable_cleaned_data, select = -c(`Lecturer in Charge`)) %>%
  rename(`Lecturer in Charge`=Strip)

View(timetable_cleaned_data)

## Data Set exporting
write.csv(timetable_cleaned_data, "timetable_cleaned_data.csv")
