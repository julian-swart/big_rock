participants <- 
  participants_59 %>% 
  mutate(year = ymd("2017-01-01")) %>% 
  bind_rows(participants_60 %>% 
              mutate(year = ymd("2018-01-01"))) %>% 
  bind_rows(participants_61 %>% 
              mutate(year = ymd("2019-01-01"))) %>%
  bind_rows(participants_62 %>% 
              mutate(year = ymd("2020-01-01"))) %>% 
  separate(type, into = c("boat_length", "boat_brand"), sep = "' ", remove = F
  ) %>% 
  separate(port, into = c("city", "state"), sep = ",", remove = F
  ) %>% 
  mutate(boat_length = as.integer(boat_length))

participants %>% 
  group_by(year, boat_length) %>% 
  summarise(n = n()) %>% 
  mutate(n = as.numeric(n)) %>% 
  ggplot(aes(x = year, y = n, color = factor(boat_length))) +
  geom_line() +
  geom_text(aes(label = n), vjust = -1) + 
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~boat_length)
  

participants %>% 
  group_by(year, boat_length) %>% 
  summarise(n = n()) %>% 
  mutate(n = as.numeric(n)) %>% 
  ggplot(aes(x = boat_length,  fill = factor(year))) +
  geom_density(alpha = .3)


activity <- 
  activity_raw %>%
  filter(!str_detect(string = activity, pattern = "dolphin|wahoo|tuna|Daily 1st Release|1st|61st|dq'd|daily|unverified|1st")
  ) %>%
  separate(col = time, into = c("weekday", "time"), sep = " @"
  ) %>%
  mutate(time = str_trim(time),
         date = case_when(
           weekday == "Monday" ~ "2020-06-08",
           weekday == "Tuesday" ~ "2020-06-09",
           weekday == "Wednesday" ~ "2020-06-10",
           weekday == "Thursday" ~ "2020-06-11",
           weekday == "Friday" ~ "2020-06-12",
           weekday == "Saturday" ~ "2020-06-13")
  ) %>% 
  unite(col = "date_time", c("date", "time"), sep = " "
  ) %>%
  mutate(date_time = ymd_hm(date_time), 
         floor_time = floor_date(x = date_time, unit = "15 minutes"),
         blue_marlin = ifelse(str_detect(string = activity, pattern = "blue marlin"), 1, 0), 
         white_marlin = ifelse(str_detect(string = activity, pattern = "white marlin"), 1, 0), 
         sailfish = ifelse(str_detect(string = activity, pattern = "sailfish"), 1, 0), 
         spearfish = ifelse(str_detect(string = activity, pattern = "spearfish"), 1, 0),
         hooked_up = ifelse(str_detect(string = activity, pattern = "Hooked"), 1, 0),
         released = ifelse(str_detect(string = activity, pattern = "Released"), 1, 0), 
         boated = ifelse(str_detect(string = activity, pattern = "Boated"), 1, 0),
         weighed = ifelse(str_detect(string = activity, pattern = "Weighed"), 1, 0), 
         fish = ifelse(released == 1 | boated == 1 | weighed == 1, 1, 0), 
         species = case_when(
           blue_marlin == 1 ~ "blue marlin", 
           white_marlin == 1 ~ "white marlin", 
           sailfish == 1 ~ "sailfish", 
           spearfish == 1 ~ "spearfish", 
           TRUE ~ "NA")
  )



df %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours, y = n)) +
  geom_col(fill = "seagreen3", alpha = .6, width = .7) + 
  geom_text(aes(label = n), vjust = -1, fontface = "bold", size = 5) + 
  ggtitle("Frequency of hook-ups by hour") + 
  xlab("Hours") + 
  ylab("Number of hook-ups") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18)) + 
  scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, 45))


activity_combined <- 
  activity_59 %>% 
  mutate(year = ymd("2017-01-01")) %>% 
  bind_rows(activity_60 %>% 
              mutate(year = ymd("2018-01-01"))) %>% 
  bind_rows(activity_61 %>% 
              mutate(year = ymd("2019-01-01"))) %>% 
  bind_rows(activity_62 %>% 
              mutate(year = ymd("2020-01-01")))




# frequency of hook-ups by 15 minute intervals ----

activity %>% 
  bind_cols(time
  ) %>% 
  drop_na() %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours_minutes) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours_minutes, y = n)) +
  geom_col(fill = "seagreen3", alpha = .7) + 
  geom_text(aes(label = n), vjust = -1, fontface = "bold") + 
  scale_y_continuous(breaks = seq(0, 40, 5)) + 
  ggtitle("Frequency of hook-ups by 15 minute intervals") + 
  xlab("15 minute interval start times") + 
  ylab("Number of hook-ups") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18))



activity %>% 
  bind_cols(time
  ) %>% 
  drop_na() %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours, y = n)) +
  geom_col(fill = "seagreen3", alpha = .6, width = .7) + 
  geom_text(aes(label = n), vjust = -1, fontface = "bold", size = 5) + 
  ggtitle("Frequency of hook-ups by hour") + 
  xlab("Hours") + 
  ylab("Number of hook-ups") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18))




