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

mydata4 <- mydata3[mydata3$Yes.No == '1',]
View(mydata4) 

#### What Thissy did ###

devtools::install_github("ropensci/plotly")
library(dplyr)
library(tidyverse) 

overview_2 <- read_csv("overview_data2.csv")
View(overview_2) 

# converting Day as factor variable 
overview_2$Day <- factor(overview_2$Day, levels = c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday", "Saturday", "Sunday")) 

# changing column names 
names(overview_2)[names(overview_2) == "Time Slot"] <- "time_slot"
names(overview_2)[names(overview_2) == "Total Number of Lectures"] <- "lectures"
View(overview_2) 

# Filtering 'All' varables under dapeartment 
all_dep <- overview_2[overview_2$Department == "All",]
View(all_dep) 

fig <- all_dep %>%
  group_by(Day) %>%
  plot_ly(x = ~time_slot, y = ~lectures, 
          type = 'scatter', color = ~Day, mode="lines+markers") %>%
  layout(title = "Number of Lectures by Time Slot",
         xaxis = list(title = "Time Slot", tickangle = -45),
         yaxis = list(title = "Number of Lectures"), 
         hovermode = "x unified") 

fig

