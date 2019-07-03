# import

library(tidyverse)
library(lubridate)
library(stringr)

activity_raw <- 
  read_csv("/Users/Julian/Documents/projects/big_rock_2019/data/activity.csv") %>% 
  select(-X1) # remove X1 column of row numbers

participants_raw <- 
  read_csv("/Users/Julian/Documents/projects/big_rock_2019/data/participants.csv") %>% 
  select(-X1) # remove X1 column of row numbers