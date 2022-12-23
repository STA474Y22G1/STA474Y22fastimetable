# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

library(tidyverse)
data <- lecturer_data %>% filter(Department=="Sports Science") %>% filter(Day=="Monday")

data %>%
  ggplot(aes(x = `Lecturer in Charge`)) +
  geom_linerange(aes(ymin = Starting.Time, ymax = Ending.Time, x = `Lecturer in Charge`),
                 size = 1.5, alpha = 0.25) +
  geom_point(aes(y = Starting.Time), colour = "#CB5416") +
  geom_point(aes(y = Ending.Time), colour = "#267266") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  ylab("Income") +
  theme_bw(base_size = 16) +
  theme(axis.title.y = element_blank())


data %>%
  ggplot(aes(x = `Lecturer in Charge`)) +
  geom_linerange(aes(ymin = Starting.Time, ymax = Ending.Time, x = `Lecturer in Charge`),
                 size = 1.5, alpha = 0.25) +
  geom_point(aes(y = Starting.Time), colour = "#CB5416") +
  geom_point(aes(y = Ending.Time), colour = "#267266") +
  coord_flip()   


