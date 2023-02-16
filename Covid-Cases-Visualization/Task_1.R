library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
require(scales)
library(ggthemes)
library(ggrepel)


setwd("C:/Users/naiki/OneDrive - LA TROBE UNIVERSITY/Desktop/La Trobe/Semester 3/Visual Analytics/Assignment 2")
covid_au <- read.csv("covid_au_state.csv")
covid_au$date <- as.Date(parse_date_time(covid_au$date, "dmy"))

#Task-1 - Figure-1
confirmed <- covid_au[,c(1,2,4)]
date_group <- confirmed %>%
  group_by(date) %>%
  summarise(total_new_cases = sum(confirmed))

date_group %>%
  ggplot(aes(date, total_new_cases)) +
  geom_line() + xlab("Month") + ylab("New Cases") + ggtitle("Daily new cases throughout Australia") +
  geom_point(data = subset(date_group, total_new_cases > 610), aes(x=date, y=total_new_cases),color="red") +
  geom_text_repel(data = subset(date_group, total_new_cases > 610), aes(label = total_new_cases), size = 3) +
  #gghighlight(total_new_cases >610) +
  theme_bw()

#Task-1 - Figure-2
state_group <- covid_au %>%
  #group_by(state) %>%
  mutate(growth = confirmed/lag(confirmed)) %>%
  filter(date > as.Date("2020-03-16"))
state_group$growth[state_group$growth == "Inf"] <- 0
state_group$growth[is.na(state_group$growth)] <- 0
state_group$growth[state_group$growth < 0] <- 0

state_group %>%  
  ggplot(aes(x=date, y=growth, fill=state)) +
  geom_col(na.rm = TRUE) +
  theme_fivethirtyeight() +
  ggtitle("Growth rate comparison between states")


