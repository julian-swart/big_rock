# import ----

library(tidyverse)
library(lubridate)
library(stringr)
library(leaflet)
library(maps)
library(ggrepel)

activity_raw <- 
  read_csv("/Users/Julian/Documents/projects/big_rock_2019/data/activity.csv") %>% 
  select(-X1) # remove X1 column of row numbers

participants_raw <- 
  read_csv("/Users/Julian/Documents/projects/big_rock_2019/data/participants.csv") %>% 
  select(-X1) # remove X1 column of row numbers

cities_raw <-
  read_csv("/Users/Julian/Documents/projects/big_rock_2019/data/us_cities.csv") %>% 
  select(city, state_id, lat, lng) %>% 
  unite(col = "port", c("city", "state_id"), sep = " ") # all cities in the US database

states <-
  map("state", fill = TRUE, plot = FALSE) # US states database 
