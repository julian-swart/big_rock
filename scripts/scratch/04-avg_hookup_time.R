# get average hook-up time 

# some boats don't have an equal amount of hook up times to released/lost times like they should (bad data), 
# we need to filter those out in order to get the average time of fighting a fish

odd_boats <- 
  df %>% 
  filter(weighed == 0) %>% 
  group_by(boat_name) %>% 
  summarise(n = n()) %>% 
  filter(n %% 2 != 0) %>% 
  pull(boat_name) %>% 
  unique()

df %>% 
  filter(boat_name %in% odd_boats) %>% 
  mutate(status = ifelse(str_detect(string = activity, pattern = "Hooked"), 'start', 'end')) %>% 
  group_by(boat_name, weekday, status) %>% 
  summarise(n = n()) %>% 
  spread(status, n) %>% 
  mutate(equal = end == start) %>% 
  pull(equal) %>% 
  table(useNA = 'ifany')

# only 45 rows have unequal starts and ends, I'm just going to filter those out even though
# they might contain just a few extra good start and end times

exclusions <- 
  df %>% 
  filter(boat_name %in% odd_boats) %>% 
  mutate(status = ifelse(str_detect(string = activity, pattern = "Hooked"), 'start', 'end')) %>% 
  group_by(boat_name, weekday, status) %>% 
  summarise(n = n()) %>% 
  spread(status, n) %>% 
  mutate(equal = end == start) %>% 
  filter(is.na(equal) | equal == FALSE) %>% 
  select(boat_name, weekday) %>% 
  unique() %>% 
  unite(col = 'combined', c('boat_name', 'weekday'), sep = ' ') %>% 
  pull(combined)

avg_fight_df <- 
  df %>% 
  mutate(status = ifelse(str_detect(string = activity, pattern = "Hooked"), 'start', 'end')) %>% 
  unite(col = 'combined', c('boat_name', 'weekday'), sep = ' ', remove = F) %>% 
  filter(!combined %in% exclusions) %>% 
  select(boat_name, weekday, date_time, status)

avg_fight_df <- 
  avg_fight_df %>% 
  arrange(boat_name, date_time) %>% 
  mutate(hook_id = rep(seq(1, nrow(avg_fight_df) /2), each = 2))

avg_fight_df %>% 
  filter(!row_number() %in% c(89, 90, 105, 106, 163, 164, 223, 224, 241, 242, 317, 318, 331, 332, 335, 336, 365, 366, 49, 50, 273, 274, 329, 330)
  ) %>% # these rows came from a duplicate rows error
  spread(status, date_time
  ) %>% 
  filter(complete.cases(.)
  ) %>% 
  select(boat_name, weekday, hook_id, start, end
  ) %>% 
  mutate(is_before = start < end, 
         is_equal = start == end 
  ) %>% 
  filter(is_before == TRUE,
         is_equal == FALSE
  ) %>%
  mutate(diff = difftime(time1 = end, time2 = start)) %>%
  inner_join(df %>% filter(status == 'end', weighed == 0) %>% select(boat_name, weekday, date_time, activity) %>% unique(),
             by = c("boat_name", "weekday", "end" = "date_time")
  ) %>% 
  # pull(diff) %>% 
  # median()
  ggplot(aes(x = diff, fill = activity)) +
  geom_histogram() + 
  facet_wrap(~activity, scales = 'free')
