# eda

# particpants distribution of boat length ----

participants %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col(fill = "deepskyblue2", alpha = .6) + 
  geom_text(aes(label = n), vjust = -.5, fontface = "bold") + 
  geom_text(aes(min(participants$boat_length), 15, label = paste("Minimum length = ", round(min(participants$boat_length)), "ft." ), hjust = -.1, vjust = -.5)) + 
  geom_text(aes(max(participants$boat_length), 15, label = paste("Max length = ", round(max(participants$boat_length)), "ft." ), hjust = 1.1, vjust = -.5)) +
  geom_text(aes(mean(participants$boat_length), 15, label = paste("Average length = ", round(mean(participants$boat_length)), "ft." ), hjust = -.1, vjust = -.5)) + 
  geom_vline(xintercept = mean(participants$boat_length), color = "red", size = .5, linetype = "dashed") + 
  geom_vline(xintercept = c(min(participants$boat_length), max(participants$boat_length)), color = "black", size = .5, linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  scale_y_continuous(breaks = seq(0, 15, 1)) + 
  ggtitle("Boat Lengths of All Participants") + 
  xlab("Length of boat") + 
  ylab("Number of boats")  + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18))


# top boat brands ----

boat_brand_agg %>%
  arrange(desc(perc)) %>% 
  filter(row_number() %in% c(1:25)) %>% 
  ggplot(aes(x = reorder(boat_brand, perc), y = perc)) + 
  geom_col(fill = "deepskyblue2", alpha = .7) + 
  geom_text(aes(label = num_boats), size = 5, hjust = -.5, fontface = "bold") + 
  coord_flip() + 
  ggtitle("Top 25 Boat Brands") +
  ylab("Percent of Total") + 
  scale_y_continuous(breaks = seq(0, 18, 1)) + 
  theme(axis.text = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        title = element_text(size = 18)) 
  

# top 25 cities ----

cities_count %>% 
  arrange(desc(n)) %>% 
  select(port, n) %>% 
  filter(row_number() %in% c(1:25)) %>% 
  ggplot(aes(x = reorder(port, n), y = n)) + 
  geom_bar(stat = "identity", fill = "deepskyblue2", alpha = .6) + 
  geom_text(aes(label = n), fontface = "bold", size = 5, hjust = -.5) + 
  coord_flip() + 
  ggtitle("Top 25 Cities of Participants") + 
  ylab("Count") + 
  scale_y_continuous(breaks = seq(0, 13, 1), limits = c(0, 13)) + 
  theme(axis.text = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 16), 
        title = element_text(size = 18), 
        axis.title.y = element_blank()
        ) 


# map of port cities ----

leaflet(data = cities) %>% 
  addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png", attribution = "Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>") %>% 
  addCircleMarkers(~lng, ~lat, color = "red", stroke = FALSE, fillOpacity = .6, radius = 3) %>% 
  addLegend("bottomright", colors= "red", labels="participants")

leaflet(data = cities_count) %>% 
  addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png", attribution = "Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>") %>% 
  addCircleMarkers(~lng, ~lat, color = "red", stroke = FALSE, fillOpacity = .6, clusterOptions = markerClusterOptions())

leaflet(data = cities_count) %>% 
  addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png", attribution = "Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>") %>% 
  addCircleMarkers(~lng, ~lat, color = "red", stroke = FALSE, fillOpacity = .6, radius = ~sqrt(n)*3)


# frequency of hook-ups by hour ----

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


# frequency of hook-ups by 15 minute intervals ----

df %>% 
  filter(hooked_up == 1) %>% 
  group_by(hours_minutes) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = hours_minutes, y = n)) +
  geom_col(fill = "seagreen3", alpha = .7) + 
  geom_text(aes(label = n), vjust = -1, fontface = "bold") + 
  scale_y_continuous(breaks = seq(0, 14, 2)) + 
  ggtitle("Frequency of hook-ups by 15 minute intervals") + 
  xlab("15 minute interval start times") + 
  ylab("Number of hook-ups") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18))


# this needs work. trying to show the differences in state ----

df %>%
  filter(released == 1, !is.na(city)) %>% 
  group_by(boat_length, state) %>% 
  summarise(num_released = sum(released)) %>% 
  inner_join(participants %>% 
               group_by(state) %>% 
               summarise(num_boats = n()), 
             by = "state"
  ) %>% 
  mutate(ratio = num_released / num_boats) %>% 
  ggplot(aes(x = boat_length, y = num_released, color = state)) + 
  geom_point() + 
  facet_wrap(~state) #+ 
# scale_x_continuous(breaks = seq(0, 118, 2)) + 
# scale_y_continuous(breaks = seq(0, 100, 2))


# distribution of released/boated billfish by boat length ----

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_length) %>% #, type) %>% 
  summarise(n = n()) %>% 
  arrange(boat_length) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_bar(stat = "identity", alpha = .6, fill = "deepskyblue2") + # exhibits the central limit theorum?
  geom_text(aes(label = n), fontface = "bold", vjust = -1) + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  ggtitle("Number of Released & Boated Billfish by Boat Size") + 
  xlab("Length of Boat") + 
  ylab("Number of Billfish Caught/Boated") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18))

# NEEDS WORK: distribution of released/boated billfish by boat length and species ----

df %>% 
  filter(released == 1 | boated == 1) %>% 
  mutate(species = case_when(
    blue_marlin == 1 ~ "blue marlin", 
    white_marlin == 1 ~ "white marlin", 
    sailfish == 1 ~ "sailfish", 
    spearfish == 1 ~ "spearfish"
  )) %>% 
  group_by(boat_length, species) %>% #, type) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = boat_length, y = n, fill = as.factor(species))) + 
  geom_bar(stat = "identity", alpha = .6) + # exhibits the central limit theorum?
  geom_text(aes(label = n), fontface = "bold") + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  ggtitle("Number of Released & Boated Billfish by Boat Size") + 
  xlab("Length of Boat") + 
  ylab("Number of Billfish Caught/Boated") + 
  theme(axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18)) + 
  facet_wrap(~species)

# distribution of released billfish by boat length ----

df %>% 
  filter(released == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  arrange(boat_length) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_bar(stat = "identity", alpha = .6, fill = "deepskyblue2") + # exhibits the central limit theorum?
  geom_text(aes(label = n), fontface = "bold", vjust = -1) + 
  scale_x_continuous(breaks = seq(0, 100, 2)) + 
  ggtitle("Number of Released Billfish by Boat Size") + 
  xlab("Length of Boat") + 
  ylab("Number of Billfish Caught/Boated") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18), 
        strip.text = element_text(size = 16))

# distribution of released billfish by boat length ----

df %>% 
  filter(boated == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  arrange(boat_length) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_bar(stat = "identity", alpha = .6, fill = "salmon1") + # exhibits the central limit theorum?
  geom_text(aes(label = n), fontface = "bold", vjust = -1) + 
  scale_x_continuous(breaks = seq(0, 100, 2), limits = c(22, 100)) + 
  scale_y_continuous(limits = c(0, 22)) +
  ggtitle("Number of Boated Billfish by Boat Size") + 
  xlab("Length of Boat") + 
  ylab("Number of Billfish Caught/Boated") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18), 
        strip.text = element_text(size = 16))

# trying to show how boat length performed by doing 'fish caught per boat entered in the tournament' ----

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_length) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  right_join(
    participants %>% group_by(boat_length) %>% summarise(num_boats = n())
  ) %>% 
  mutate(n = ifelse(is.na(n), 0, n), 
         normalized = n /num_boats, 
         sqrt_num_boats = sqrt(num_boats), 
         sqrt_fish_caught = sqrt(n)) %>% 
  ggplot(aes(x = sqrt_num_boats, y = sqrt_fish_caught, color = normalized)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = boat_length)) +
  geom_abline(intercept = c(0, 0), slope = 1) + 
  expand_limits(x = 0, y = 0) +
  scale_color_gradient2(mid = "grey70", high = "red") +
  labs(color = "Fish per Boat") + 
  xlab("square_root(Number of Boats)") + 
  ylab("square_root(Number of Billfish Released/Boated)") + 
  ggtitle("Comparing Performance of Different Boat Lengths") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18), 
        strip.text = element_text(size = 16))

# also by species: trying to show how boat length performed by doing 'fish caught per boat entered in the tournament' ----

df %>% 
  filter(released == 1 | boated == 1) %>%
  mutate(species = case_when(
    blue_marlin == 1 ~ "blue marlin", 
    white_marlin == 1 ~ "white marlin", 
    sailfish == 1 ~ "sailfish", 
    spearfish == 1 ~ "spearfish"
  )) %>% 
  group_by(boat_length, species) %>% 
  summarise(n = n()) %>%
  right_join(
    participants %>% group_by(boat_length) %>% summarise(num_boats = n())
  ) %>% 
  mutate(n = ifelse(is.na(n), 0, n), 
         normalized = n /num_boats, 
         sqrt_num_boats = sqrt(num_boats), 
         sqrt_fish_caught = sqrt(n)) %>% 
  ggplot(aes(x = sqrt_num_boats, y = sqrt_fish_caught, color = normalized)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = boat_length)) +
  geom_abline(intercept = c(0, 0), slope = 1) + 
  expand_limits(x = 0, y = 0) +
  scale_color_gradient2(mid = "grey70", high = "red") +
  labs(color = "Fish per Boat") + 
  xlab("square_root(Number of Boats)") + 
  ylab("square_root(Number of Billfish Released/Boated)") + 
  ggtitle("Comparing Performance of Different Boat Lengths") + 
  facet_wrap(~species)

# trying to show how boat brands performed by doing 'fish caught per boat entered in the tournament' ----

df %>% 
  filter(released == 1 | boated == 1) %>% 
  group_by(boat_brand) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  right_join(
    boat_brand_agg %>% select(boat_brand, num_boats)
  ) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         normalized = n /num_boats, 
         sqrt_num_boats = sqrt(num_boats), 
         sqrt_fish_caught = sqrt(n)) %>% 
  ggplot(aes(x = sqrt_num_boats, y = sqrt_fish_caught, color = normalized)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = boat_brand)) +
  geom_abline(intercept = c(0, 0), slope = 1) + 
  expand_limits(x = -0, y = -0) +
  scale_color_gradient2(mid = "grey70", high = "red") +
  labs(color = "Fish per Boat") + 
  xlab("square_root(Number of Boats)") + 
  ylab("square_root(Number of Billfish Released/Boated)") + 
  ggtitle("Comparing Performance of Different Boat Brands") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        title = element_text(size = 18), 
        strip.text = element_text(size = 16))
  
# boats that didn't have any activity ----

df %>% 
  filter(is.na(activity)) %>% 
  group_by(boat_length) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = boat_length, y = n)) + 
  geom_col()


#TODO ----
# get lag -1 time of releases and hookups

# for lag, possibly use this complicated solution https://stackoverflow.com/questions/32999460/how-to-calculate-time-difference-with-previous-row-of-a-data-frame-by-group/32999651

df %>% 
  filter(boat_name == "Top Dog", weighed != 1) %>% 
  select(activity, weekday, date_time) %>% 
  mutate(status = ifelse(activity == "Hooked up", "start", "end")) 
