# import ----

library(tidyverse)
library(lubridate)
library(stringr)
library(leaflet)
library(maps)
library(ggrepel)

get_activity_data <- 
  function(annual, df_name){
    
      read_csv(paste("/Users/Julian/Documents/projects/big_rock/data/activity/activity", annual, ".csv", sep = "")) 
    
  }

get_participants_data <- 
  function(annual, df_name){
    
    read_csv(paste("/Users/Julian/Documents/projects/big_rock/data/participants/participants", annual, ".csv", sep = ""))
    
  }

activity_raw <- 
  get_activity_data(annual = "59th") %>% 
  mutate(annual = "59th") %>% 
  bind_rows(get_activity_data(annual = "60th") %>% 
                mutate(annual = "60th")) %>% 
  bind_rows(get_activity_data(annual = "61st") %>% 
              mutate(annual = "61st")) %>% 
  bind_rows(get_activity_data(annual = "62nd") %>% 
              mutate(annual = "62nd")) %>% 
  bind_rows(get_activity_data(annual = "63rd") %>% 
              mutate(annual = "63rd"))

participants_raw <- 
  get_participants_data(annual = "59th") %>% 
  mutate(annual = "59th") %>% 
  bind_rows(get_participants_data(annual = "60th") %>% 
              mutate(annual = "60th")) %>% 
  bind_rows(get_participants_data(annual = "61st") %>% 
              mutate(annual = "61st")) %>% 
  bind_rows(get_participants_data(annual = "62nd") %>% 
              mutate(annual = "62nd")) %>% 
  bind_rows(get_participants_data(annual = "63rd") %>% 
              mutate(annual = "63rd"))

cities_raw <-
  read_csv("/Users/Julian/Documents/projects/big_rock/data/us_cities.csv") %>% 
  select(city, state_id, lat, lng) %>% 
  unite(col = "port", c("city", "state_id"), sep = " ") # all cities in the US database

states <-
  map("state", fill = TRUE, plot = FALSE) # US states database 








# 


activity_raw %>% 
  filter(str_detect(string = activity, pattern = "blue marlin"), str_detect(string = activity, pattern = "Weighed")) %>% 
  mutate(weight = as.numeric(str_sub(string = activity, start = 11, end = 15))) %>% 
  drop_na() %>% 
  select(boat_name, annual, weight) %>% 
  arrange(annual, desc(weight)) %>% 
  group_by(annual) %>%
  mutate(rank = row_number()) %>% 
  group_by(annual) %>% 
  summarise(winning_weight = max(weight), 
            num_marlins_weighed = n()) %>% 
  inner_join(participants %>% 
               group_by(annual) %>% 
               summarise(num_boats_entered = n_distinct(boat_name)), by = "annual") %>% 
  mutate(ntrials = num_boats_entered * 4, 
         total_marlins_weighed_per_boat_perc = num_marlins_weighed / num_boats_entered * 100, 
         success_rate = num_marlins_weighed / ntrials * 100) %>% 
  inner_join(activity_raw %>% 
               filter(str_detect(string = activity, pattern = "blue marlin"), 
                      str_detect(string = activity, pattern = "Weighed")) %>% 
               mutate(weight = as.numeric(str_sub(string = activity, start = 11, end = 15))) %>% 
               drop_na() %>% 
               arrange(annual, desc(weight)) %>% group_by(annual) %>% mutate(rank = row_number()) %>% filter(rank == 1) %>% 
               select(boat_name) %>% 
               inner_join(participants %>% 
                            select(boat_name, boat_length, boat_brand) %>% 
                            distinct()) %>% 
               slice(c(1, 3, 4, 5)))
