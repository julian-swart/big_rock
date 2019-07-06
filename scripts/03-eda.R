# eda

# particpants distribution of boat length ----

participants %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col(fill = 'deepskyblue2',alpha = .6) + 
  geom_text(aes(label = n), vjust = -.5, fontface = 'bold') + 
  geom_text(aes(min(participants$boat_length), 15, label = paste('Minimum length = ', round(min(participants$boat_length)), 'ft.' ), hjust = -.1, vjust = -.5)) + 
  geom_text(aes(max(participants$boat_length), 15, label = paste('Max length = ', round(max(participants$boat_length)), 'ft.' ), hjust = 1.1, vjust = -.5)) +
  geom_text(aes(mean(participants$boat_length), 15, label = paste('Average length = ', round(mean(participants$boat_length)), 'ft.' ), hjust = -.1, vjust = -.5)) + 
  geom_vline(xintercept = mean(participants$boat_length), color = 'red', size = .5, linetype = 'dashed') + 
  geom_vline(xintercept = c(min(participants$boat_length), max(participants$boat_length)), color = 'black', size = .5, linetype = 'dashed') + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  scale_y_continuous(breaks = seq(0, 15, 1)) + 
  ggtitle("Participants of The Big Rock Blue Marlin Fishing Tournament 2019", subtitle = "Distribution of boat lengths") + 
  xlab("Length of boat") + 
  ylab("Number of boats") 


# frequency of hook-ups by hour ----

df %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours, y = n)) +
  geom_col(fill = 'seagreen3', alpha = .6) + 
  geom_text(aes(label = n), vjust = -1, fontface = 'bold') + 
  ggtitle("Frequency of hook-ups by hour") + 
  xlab("Hours") + 
  ylab("Number of hook-ups")


# frequency of hook-ups by 15 minute intervals ----

df %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours_minutes) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours_minutes, y = n)) +
  geom_col(fill = 'seagreen3', alpha = .7) + 
  geom_text(aes(label = n), vjust = -1, fontface = 'bold') + 
  scale_y_continuous(breaks = seq(0, 14, 2)) + 
  ggtitle("Frequency of hook-ups by 15 minute intervals") + 
  xlab("15 minute interval start times") + 
  ylab("Number of hook-ups")


# trying to show the differences in state ----

df %>%
  filter(released == 1, !is.na(city)) %>% 
  group_by(boat_length, state) %>% 
  summarise(num_released = sum(released)) %>% 
  inner_join(participants %>% 
               group_by(state) %>% 
               summarise(num_boats = n()), 
             by = 'state'
  ) %>% 
  mutate(ratio = num_released / num_boats) %>% 
  ggplot(aes(x = boat_length, y = num_released, color = state)) + 
  geom_point() + 
  facet_wrap(~state)#+ 
# scale_x_continuous(breaks = seq(0, 118, 2)) + 
# scale_y_continuous(breaks = seq(0, 100, 2))


# distribution of releases by boat length ----

df %>% 
  filter(released == 1) %>% 
  group_by(boat_length) %>% summarise(n = n()) %>% 
  arrange(boat_length) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col() # looks to follow the central limit theorum


# trying to show how boat brands performed by doing 'fish caught per boat entered in the tournament' ----

boat_brand_agg <- 
  participants %>% 
  group_by(boat_brand) %>% 
  summarise( #TODO change BCBuddy Cannady to BC
    num_boats = n()) %>% 
  mutate(perc = num_boats / sum(num_boats) * 100) %>% 
  arrange(desc(perc))

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_brand) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  inner_join(
    boat_brand_agg
  ) %>% 
  mutate(normalized = n /num_boats) %>% 
  ggplot(aes(x = num_boats, y = n)) + 
  geom_text(aes(label = boat_brand)) + #position = position_jitter(width = 1))
  geom_smooth()

#TODO ----
# show how viking is the overwhelming favorite for boat brand 
# and show that over 50% of boats in the tournament have 1 brand entered 
# get lag -1 time of releases and hookups

# for lag, possibly use this complicated solution https://stackoverflow.com/questions/32999460/how-to-calculate-time-difference-with-previous-row-of-a-data-frame-by-group/32999651

df %>% 
  filter(boat_name == 'Top Dog', weighed != 1) %>% 
  select(activity, weekday, date_time) %>% 
  mutate(status = ifelse(activity == 'Hooked up', 'start', 'end')) 
