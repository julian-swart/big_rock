# tidy 

# tidy participants data frame ----

head(participants_raw)

participants <- 
  participants_raw %>% 
  separate(type, into = c("boat_length", "boat_brand"), sep = "' ", remove = F
           ) %>% 
  separate(port, into = c("city", "state"), sep = ",", remove = F
           ) %>% 
  mutate(boat_length = as.integer(boat_length))

head(participants)

str(participants)

# check state names 

participants %>%
  count(state, sort = T) %>% 
  print(n = 23)

# check boat brands 

participants %>% 
  pull(boat_brand) %>% 
  unique() %>% 
  sort()

# fix states and boat brands 

participants <- 
  participants %>%
  mutate(state = case_when(
         str_detect(string = state, pattern = "NC|North") == TRUE ~ "NC", 
         str_detect(string = state, pattern = "FL|Fl") == TRUE ~ "FL", 
         str_detect(string = state, pattern = "MD|Maryland") == TRUE ~ "MD",
         str_detect(string = state, pattern = "VA") == TRUE ~ "VA")
         ) %>% 
  mutate(boat_brand = case_when(
         str_detect(string = boat_brand, pattern = "BCBuddy Cannady|BC|Canady") == TRUE ~ "Buddy Cannady", 
         str_detect(string = boat_brand, pattern = "Grady White") == TRUE ~ "Grady White",
         str_detect(string = boat_brand, pattern = "Cabo") == TRUE ~ "Cabo",
         str_detect(string = boat_brand, pattern = "BuddyDavis") == TRUE ~ "Buddy Davis",
         str_detect(string = boat_brand, pattern = "Custom Carolina") == TRUE ~ "Custom Carolina",
         str_detect(string = boat_brand, pattern = "Ocean") == TRUE ~ "Ocean Yacht",
         str_detect(string = boat_brand, pattern = "Winter") == TRUE ~ "Winter")
         )

# check state names again 

participants %>%
  count(state, sort = T) # There are 57 NA's because 57 boats did not input the state they are from 

# tidy activity data frame ----

head(activity_raw)
str(activity_raw)

# find activity to filter out 

activity_raw %>% 
  pull(activity) %>% 
  unique()

# clean up the time column, filter out the meat fish (like dolphin, tuna, wahoo) and other irrelevant activity 
# make a dummy matrix of hooked up, released, lost, boated, weighed, and fish species 

activity <- 
  activity_raw %>%
  filter(!str_detect(string = activity, pattern = "dolphin|wahoo|tuna|Daily 1st Release|1st|61st|dq'd|daily|unverified|1st")
         ) %>%
  mutate(time = replace(time, time == "Monday @ 1:37 AM", "Monday @ 1:37 PM"), # found these two times were mislabelled as AM time
         time = replace(time, time == "Monday @ 1:14 AM", "Monday @ 1:14 PM")
         ) %>% 
  separate(col = time, into = c("weekday", "time"), sep = " @"
           ) %>%
  mutate(time = str_trim(time),
         date = case_when(
         weekday == "Monday" ~ "2019-06-10",
         weekday == "Tuesday" ~ "2019-06-11",
         weekday == "Wednesday" ~ "2019-06-12",
         weekday == "Thursday" ~ "2019-06-13",
         weekday == "Friday" ~ "2019-06-14",
         weekday == "Saturday" ~ "2019-06-15")
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
         weighed = ifelse(str_detect(string = activity, pattern = "Weighed"), 1, 0)
         )

# look at activity again 

activity %>% 
  pull(activity) %>% 
  unique() # looks good 

# get hours and minutes by itself for frequency plots of hook-up times ----

hours <- data.frame(hours = hour(activity$floor_time)) 

minutes <- data.frame(minutes = minute(activity$floor_time))

time <- 
  cbind(hours, minutes) %>% 
  mutate_if(is.numeric, as.character) %>% 
  unite(col = "hours_minutes", c("hours", "minutes"), sep = ":", remove = F) 

# relevel the hours in order 

unique(time$hours)

time$hours <- 
  factor(time$hours, 
         levels = c("8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "23")
         )

# relevel the 15 minute intervals that are in the data in order 

unique(time$hours_minutes)

time$hours_minutes <- 
  factor(time$hours_minutes, 
         levels = c("8:0", "8:15", "8:45", "9:0", "9:15", "9:30", "9:45", "10:0", "10:15", "10:30", "10:45", "11:0", 
                    "11:15", "11:30", "11:45", "12:0", "12:15", "12:30", "12:45", "13:0", "13:15", "13:30", 
                    "13:45", "14:0", "14:15", "14:30", "14:45", "15:0", "15:15", "16:15", "16:30", "17:15",
                    "18:30", "23:0")
         )


# combine participants and activity to make final data frame ----

df <- 
  activity %>% 
  inner_join(participants, by = "boat_name"
             ) %>% 
  bind_cols(time
            ) %>% 
  select(boat_name, boat_length, boat_brand, owner, city, state, activity, weekday, date_time,
         floor_time, hours_minutes, hours, blue_marlin, white_marlin, sailfish, spearfish, hooked_up, 
         released, boated, weighed
         )



















