### Import ----

library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)


boatswebpage <- read_html('https://widgets.reeltimeapps.com/live/tournaments/61st-annual-big-rock-blue-marlin-tournament/participants')

boats_html <- html_nodes(boatswebpage, '.post-title')

boat_name <- html_text(boats_html, trim = T)
boat_name <- boat_name[1:184] # Only boat names, not angler names

boats_description_html <- html_nodes(boatswebpage, '#tabs-boats li')

boats_description <- html_text(boats_description_html, trim = T)

participants_raw <- data.frame(boat_name, boats_description)

activitystreamwebpage <- read_html('https://widgets.reeltimeapps.com/live/tournaments/61st-annual-big-rock-blue-marlin-tournament/activities?day=0&per=10000&type=all', 
                                   options = "HUGE"
                                   )
activity_boat_html <- html_nodes(activitystreamwebpage, 'article h4')
activity_boat <- html_text(activity_boat_html, trim = T)
activity_boat <- gsub('[\\\n].*', '', activity_boat)

activity <- html_nodes(activitystreamwebpage, 'p strong') %>% 
  html_text(trim = T)
activity <- gsub('[\\\n].*', '', activity)


activityresult <- data.frame(activity_boat, activity)

activity_time_html <- html_nodes(activitystreamwebpage, 'article p')
activity_time <- html_text(activity_time_html, trim = T)
activity_time <- activity_time[substr(activity_time, 1, 3) == 'Jun' | substr(activity_time, 10, 10) == '@'] 
activity_time <- gsub('[\\\n].*', '', activity_time)


activityfeed_raw <- data.frame(cbind(activity_boat, activity, activity_time))


### Tidy ----

fished <- 
  tibble(days = 1:6,
         day_of_week = c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'),
         num_boats_fished = c(181, 162, 37, 1, NA, NA), 
         total_boats = 184,
         perc_fished = num_boats_fished / total_boats * 100,
         weather = c('good', 'good', 'bad', 'bad', 'good', 'good'), 
         wind = c('W 10-15 kt', 'W 10-15 kt, becoming N 10-20 kt', 'E 15-25 kt, becoming S', 'SW 20-30 kt', 'N 10-20 kt, diminishing to 5-15 kt', 'SE 5-15 kt, becoming S'), 
         seas = c('3-5 ft', '3-5 ft', '3-5 ft', '5-9 ft', '3-5 ft', '2-4 ft')
  )


activityfeed <- 
  activityfeed_raw %>% 
  mutate(
    activity_boat = as.character(activity_boat)
  )

participants <- 
  participants_raw %>% 
  separate(boats_description, into = c("boat_length", "boat_brand"), sep = "' ") %>% 
  mutate(
    boat_length = as.numeric(boat_length), 
    boat_name = as.character(boat_name)
  )

str(participants)


df <-
  activityfeed %>% 
  inner_join(participants, by = c("activity_boat" = "boat_name")) %>% 
  filter(!grepl("dolphin|wahoo|tuna|Daily 1st Release|61st|dq'd|daily|unverified|1st", activity)) %>% 
  mutate(
    blue_marlin = ifelse(grepl("blue marlin", activity), 1, 0), 
    whtie_marlin = ifelse(grepl("white marlin", activity), 1, 0), 
    sailfish = ifelse(grepl("sailfish", activity), 1, 0), 
    hookedup = ifelse(grepl("Hooked|hooked", activity), 1, 0),
    released = ifelse(grepl("Released|released", activity), 1, 0), 
    boated = ifelse(grepl("Boated|boated", activity), 1, 0), 
    weighed = ifelse(grepl("Weighed", activity), 1, 0), 
    activity_time = str_replace(string = activity_time, pattern = "@", replacement = "2019 ")
  ) %>% 
  mutate(
    activity_time = gsub(activity_time, pattern = "AM|PM", replacement = "")
  ) %>% 
  separate(activity_time, into = c("date", "time"), sep = "(?<=\\d{3})\\s") %>% 
  mutate(date = gsub(date, pattern = " ", replacement = "/")) %>% 
  unite(col = activity_time, c("date", "time"), sep = " ") %>% 
  mutate(activity_time = mdy_hm(activity_time)) %>% 
  separate(activity_time, into = c("day", "time"), sep = " ") %>% 
  mutate_if(is.factor, as.character) 

str(df)

rm(list=setdiff(ls(), c("participants", "activityfeed", "df")))

releases_boat_length <- 
  df %>% 
  filter(released == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = sum(released))

releases_and_boated_boat_length <- 
  df %>% 
  filter(released == 1 | blue_marlin == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = n())

releases_boat_length_and_species <- 
  df %>% 
  filter(released == 1) %>% 
  group_by(boat_length, activity) %>% 
  summarise(n = sum(released))

releases_boat_length_and_brand <- 
  df %>% 
  filter(released == 1, activity != "Released a dq'd.") %>% 
  group_by(boat_brand) %>% 
  summarise(n = sum(released))

releases_boat_length_and_species_and_brand <- 
  df %>% 
  filter(released == 1, activity != "Released a dq'd.") %>% 
  group_by(boat_brand, activity) %>% 
  summarise(n = sum(released))

boated_a_blue <- 
  df %>% 
  filter(boated == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = sum(boated))























### Explore and Visualize ----

summary(participants$boat_length)

participants %>% 
  ggplot(aes(x = boat_length)) + 
  geom_density() + 
  geom_abline(intercept = median(participants$boat_length))

df %>% 
  group_by(activity_time) %>% 
  count(activity) %>% 
  ggplot(aes(x = activity_time, y = n, color = factor(activity))) + 
  geom_line() + 
  facet_wrap(~activity)

participants %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col(fill = 'cornsilk2', color = 'black') + 
  geom_vline(xintercept = mean(participants$boat_length), color = 'red', size = 1, linetype = 'dashed') + 
  geom_text(aes(mean(participants$boat_length), 15, label = paste('Average length = ', round(mean(participants$boat_length)), 'ft.' ), hjust = -.1)) + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  scale_y_continuous(breaks = seq(0, 15, 1)) + 
  ggtitle("Big Rock Boat Lengths") + 
  xlab("Length of Boat") + 
  ylab("Number of Boats") + 
  geom_vline(xintercept = min(participants$boat_length), color = 'blue', size = 1, linetype = 'dashed') + 
  geom_vline(xintercept = max(participants$boat_length), color = 'blue', size = 1, linetype = 'dashed') + 
  geom_text(aes(min(participants$boat_length), 15, label = paste('Minimum length = ', round(min(participants$boat_length)), 'ft.' ), hjust = -.1)) + 
  geom_text(aes(max(participants$boat_length), 15, label = paste('Max length = ', round(max(participants$boat_length)), 'ft.' ), hjust = 1.1)) +
  geom_text(aes(label = n), vjust = -.5, fontface = 'bold')

df %>% 
  filter(released == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = sum(released)) %>%
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col()


releases_and_boated_boat_length %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col(fill = 'cornsilk2', color = 'black') +  
  scale_x_continuous(breaks = seq(min(releases_and_boated_boat_length$boat_length), max(releases_and_boated_boat_length$boat_length), 2)) + 
  scale_y_continuous(breaks = seq(0, max(releases_and_boated_boat_length$n), 1)) + 
  geom_text(aes(label = n), vjust = -.5, fontface = 'bold') +  ggtitle("Big Rock", subtitle =  "Number of Released/Boated Fish vs Boat Length") + 
  xlab("Length of Boat") + 
  ylab("Number of Released/Boated Fish")  + 
  geom_vline(xintercept = min(releases_and_boated_boat_length$boat_length), color = 'blue', size = .5, linetype = 'dashed') + 
  geom_vline(xintercept = max(releases_and_boated_boat_length$boat_length), color = 'blue', size =.5, linetype = 'dashed') + 
  geom_text(aes(min(releases_and_boated_boat_length$boat_length), (max(releases_and_boated_boat_length$n) + 1.5), label = paste('Minimum length = ', round(min(releases_and_boated_boat_length$boat_length)), 'ft.' ), hjust = -.1)) + 
  geom_text(aes(max(releases_and_boated_boat_length$boat_length), (max(releases_and_boated_boat_length$n) + 1.5), label = paste('Max length = ', round(max(releases_and_boated_boat_length$boat_length)), 'ft.' ), hjust = 1.1)) +
  geom_vline(xintercept = mean(releases_and_boated_boat_length$boat_length), color = 'red', size = .5, linetype = 'dashed') + 
  geom_text(aes(mean(releases_and_boated_boat_length$boat_length), (max(releases_and_boated_boat_length$n) + 1.5), label = paste('Average length = ', round(mean(releases_and_boated_boat_length$boat_length)), 'ft.' ), hjust = -.1))


releases_boat_length_and_species %>%
  ggplot(aes(x = boat_length, y = n, fill = factor(activity))) + 
  geom_col() + 
  scale_x_continuous(breaks = seq(min(releases_boat_length_and_species$boat_length), max(releases_boat_length_and_species$boat_length), 2)) + 
  scale_y_continuous(breaks = seq(0, max(releases_boat_length_and_species$n), 1)) + 
  geom_text(aes(label = n), vjust = -.5, fontface = 'bold') +  ggtitle("Big Rock", subtitle =  "Number of Releases vs Boat Length") + 
  xlab("Length of Boat") + 
  ylab("Number of Releases") + 
  facet_wrap(~ activity) 

releases_boat_length_and_brand %>% 
  ggplot(aes(x = reorder(boat_brand, n), y = n)) + 
  geom_col(fill = 'cornsilk2', color = 'black') + 
  geom_text(aes(label = n), hjust = -1, fontface = 'bold') +  ggtitle("Big Rock", subtitle =  "Number of Releases vs Boat Length") + 
  xlab("Length of Boat") + 
  ylab("Number of Releases") + 
  coord_flip()

releases_boat_length_and_species_and_brand %>% 
  ggplot(aes(x = reorder(boat_brand, n), y = n, fill = factor(activity))) + 
  geom_col() + 
  geom_text(aes(label = n), hjust = -1, fontface = 'bold') +  ggtitle("Big Rock", subtitle =  "Number of Releases vs Boat Length") + 
  xlab("Length of Boat") + 
  ylab("Number of Releases") + 
  coord_flip() +
  facet_wrap(~ activity)

boated_a_blue %>% 
  ggplot(aes(x = boat_length, y = n)) +
  geom_col(fill = 'cornsilk2', color = 'black') + 
  geom_text(aes(label = n), vjust = -1, fontface = 'bold') +  
  ggtitle("Big Rock", subtitle =  "Number of Blue Marlin Boated vs Boat Length") + 
  xlab("Length of Boat") + 
  ylab("Number of Blue Marlin Boated") + 
  geom_vline(xintercept = min(boated_a_blue$boat_length), color = 'red', size = .5, linetype = 'dashed') + 
  geom_vline(xintercept = max(boated_a_blue$boat_length), color = 'red', size = .5, linetype = 'dashed') + 
  geom_vline(xintercept = mean(boated_a_blue$boat_length), color = 'blue', size = .5, linetype = 'dashed') + 
  geom_text(aes(min(boated_a_blue$boat_length), (max(boated_a_blue$n) + 1), label = paste('Minimum length = ', round(min(boated_a_blue$boat_length)), 'ft.' ), hjust = -.1)) + 
  geom_text(aes(max(boated_a_blue$boat_length), (max(boated_a_blue$n) + 1), label = paste('Max length = ', round(max(boated_a_blue$boat_length)), 'ft.' ), hjust = 1.1)) +
  geom_text(aes(mean(boated_a_blue$boat_length), (max(boated_a_blue$n) + 1), label = paste('Average length = ', round(mean(boated_a_blue$boat_length)), 'ft.' ), hjust = -.1))

# normalized scatter plot of releases and boated by boat length

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_length) %>% 
  summarise(
    n = n()
  ) %>% 
  inner_join(
    participants %>%
      group_by(boat_length) %>% 
      summarise(n_boats = n()), 
    by = 'boat_length'
  ) %>% 
  mutate(
    normalized = n/n_boats
  ) %>% 
  ggplot(aes(x = reorder(boat_length, normalized), y = normalized)) + 
  geom_col() + 
  coord_flip()
# geom_point() + 
# geom_smooth(method = 'lm', formula = 'y~x')

# does boat brand have an effect on fish caught 

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_brand) %>% 
  summarise(
    n = n()
  ) %>% 
  inner_join(
    participants %>%
      group_by(boat_brand) %>% 
      summarise(n_boats = n()), 
    by = 'boat_brand'
  ) %>% 
  mutate(
    normalized = n/n_boats
  ) %>% 
  ggplot(aes(x = reorder(boat_brand, normalized), y = normalized)) + 
  geom_col() + 
  coord_flip() 


# does location of boat have an effect of fish caught


