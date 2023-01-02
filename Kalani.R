library(tidyr) 
library(dplyr)
library(readr)
library(tidyverse)

mydata <- read_csv("Course_Data2.csv")
mydata
View(mydata) 

mydata$Start.Time <- format(as.POSIXct(mydata$Starting.Time, tz = "GMT"), format = "%H:%M") 
mydata$End.Time <- format(as.POSIXct(mydata$Ending.Time, tz = "GMT"), format = "%H:%M") 
View(mydata) 

mydata$"08:00-08:50"<- factor(ifelse(mydata$Start.Time <= "08:00" & mydata$End.Time >= "08:50", 1,0))
mydata$"08:55-09:45"<- factor(ifelse(mydata$Start.Time <= "08:55" & mydata$End.Time >= "09:45", 1,0))
mydata$"10:15-11:05"<- factor(ifelse(mydata$Start.Time <= "10:15" & mydata$End.Time >= "11:05", 1,0))
mydata$"11:10-12:00"<- factor(ifelse(mydata$Start.Time <= "11:10" & mydata$End.Time >= "12:00", 1,0))
mydata$"13:00-14:00"<- factor(ifelse(mydata$Start.Time <= "13:00" & mydata$End.Time >= "14:00", 1,0))
mydata$"14:00-15:00"<- factor(ifelse(mydata$Start.Time <= "14:00" & mydata$End.Time >= "15:00", 1,0))
mydata$"15:00-16:00"<- factor(ifelse(mydata$Start.Time <= "15:00:" & mydata$End.Time >= "16:00", 1,0))
mydata$"16:00-17:00"<- factor(ifelse(mydata$Start.Time <= "16:00" & mydata$End.Time >= "17:00", 1,0))
mydata$"17:00-17:45"<- factor(ifelse(mydata$Start.Time <= "17:00" & mydata$End.Time >= "18:00", 1,0))
View(mydata) 

mydata2 <- mydata %>% pivot_longer(cols=c("08:00-08:50", "08:55-09:45", "10:15-11:05", "11:10-12:00", 
                               "13:00-14:00", "14:00-15:00", "15:00-16:00", "16:00-17:00", "17:00-17:45"),
                    names_to="Time Slot", values_to= "Yes.No") 

View(mydata2) 
mydata3 <- subset(mydata2, select = -25)

View(mydata3)

mydata4 <- mydata3[mydata3$Yes.No == '1',]
View(mydata4) 

