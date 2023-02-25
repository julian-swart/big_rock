### import 

library(tidyverse)
library(purrr)

import_data <- function(dir) { 
  
  files <- list.files(dir)
  
  data_list <- list()
  
  for (i in 1:length(files)) { 
    
    data_list[[i]] <- 
      read_csv(paste(dir, files[i], sep = "")) %>% 
      select(-...1) %>% 
      mutate(year = files[i])
    
  }
  
  return(bind_rows(data_list))
  
}

df_partic <- 
  import_data(dir = '/Users/JSWART/Documents/big_rock/data/participants/') %>% 
  mutate(year = stringr::str_sub(string = year, start = 13, end = 14))

df_act <- 
  import_data(dir = '/Users/JSWART/Documents/big_rock/data/activity/') %>% 
  mutate(year = stringr::str_sub(string = year, start = 9, end = 10))
  

###  explore

df_partic %>% 
  group_by(boat_name) %>% 
  summarise(n = n_distinct(year)) %>% 
  arrange(desc(n))


df_partic %>% 
  group_by(year) %>% 
  summarise(num_partic = n_distinct(boat_name)) %>% 
  ggplot(aes(x = as.numeric(year), y = n)) +
  geom_point() + 
  geom_line()

df_act %>% 
  #filter(stringr::str_detect(activity, pattern = 'Released a blue marlin|released a blue marlin|Boated|boated')) %>% 
  filter(stringr::str_detect(activity, pattern = 'Boated|boated')) %>% 
  count(year) %>% 
  rename(blues_caught = n) %>% 
  inner_join(
    df_partic %>% 
      group_by(year) %>% 
      summarise(num_partic = n_distinct(boat_name))
  ) %>% 
  mutate(blues_caught_normalized = blues_caught / num_partic * 100) %>% 
  ggplot(aes(x = as.numeric(year), y = blues_caught_normalized)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(0, 10))



# get weights (I might already have this in activity) and the prize money 
# do something on the winners 
















