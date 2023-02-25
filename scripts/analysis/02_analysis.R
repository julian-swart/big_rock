
# 1) Boat Lengths ----

add_theme <- 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14), 
    axis.title.x = element_text(size = 16, face = 'bold'),
    axis.title.y = element_text(size = 16, face = 'bold'),
    title = element_text(size = 18)
  )

participants %>% 
  ggplot(aes(x = annual, y = boat_length)) +
  geom_boxplot(width = .4) +
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  xlab("Annual") + 
  ylab("Boat Length in Feet") + 
  add_theme

# 2) Boat length performance ----
# 3) Boat brands ----
# 4) Boat brand performance ----
# 5) Participant's ports ----
# 6) Participant's overlap ----
# 7) Winning weights ----
# 8) Hook-up times ----
# 9) Annual catch-rate ----
# Other ----

# let's look at performance by year for number of marlins released (blue or otherwise), normalized for the number of participants 
activity_raw %>% 
  filter(str_detect(string = activity, pattern = "Released|released")) %>% 
  group_by(annual) %>% 
  summarise(release_count = n()) %>% 
  arrange(annual) %>% 
  inner_join(participants_raw %>% 
               group_by(annual) %>% 
               summarise(participant_count = n()) %>% 
               arrange(annual)
  ) %>% 
  mutate(ratio = release_count/participant_count) %>% 
  ggplot(aes(x = as.integer(str_sub(annual, end = 2)), y = ratio)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(0, 1))



# let's look at performance by year for number of blue marlins killed and weighed, normnalized for the number of participants 
activity_raw %>% 
  filter(str_detect(string = activity, pattern = "Weighed"), str_detect(string = activity, pattern = "blue marlin")) %>% 
  group_by(annual) %>% 
  summarise(release_count = n()) %>% 
  arrange(annual) %>% 
  inner_join(participants_raw %>% 
               group_by(annual) %>% 
               summarise(participant_count = n()) %>% 
               arrange(annual)
  ) %>% 
  mutate(ratio = release_count/participant_count) %>% 
  ggplot(aes(x = as.integer(str_sub(annual, end = 2)), y = ratio)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(0, .12))


activity_raw %>% 
  filter(str_detect(string = activity, pattern = "Weighed"), str_detect(string = activity, pattern = "blue marlin"), annual == '61st')



  
  
  
  
  
  
    
           