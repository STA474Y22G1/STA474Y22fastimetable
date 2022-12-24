library(readr)

timetable_data<-read_csv("timetable_data.csv")

timetable_data$Day[timetable_data$Day=="Wednsday"]<-"Wednesday"

unique(timetable_data$Day)

write_csv(timetable_data, "timetable_data.csv")
