# prerequisites : run the codes in trips_01, trips_02, trips_03.

# remove previous dfs for cleaning. 
rm(new_df_11, new_df_2, new_df_3)

# remove unnecessary rows. usertype "Dependent". 

t_2013_to_2019 <- t_2013_to_2019 %>% 
  filter(usertype != "Dependent") 

###################
mday() # returns the day of the month. 
wday() # returns the day of the week as a decimal number or
# an ordered factor if argument label = TRUE.

temp_22 %>% 
  mutate(trip_day = format(as.Date(starttime), "%d"), .before = 1) %>% 
  mutate(trip_month = format(as.Date(starttime), "%m"), .before = 2) %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y"), .before = 3)

# You can condense the four lines (mean, median, min, max): 
summary(t_2020_to_2022$tripduration)
######################################################################
# TREND
######################################################################
# first scatterplot - day of week 
# this chart shows that, casual users tend to rent bikes for Sat and Sun. 
# whereas, subscribers tend to rent bikes during Mon-Fri more so than Sat andSun. 

# chart 1) total number of trips on each day of the week, compare usertypes.
# double bar chart 
t_2020_to_2022 %>% 
  mutate(day_of_week = wday(t_2020_to_2022$starttime, label = TRUE)) %>% 
  group_by(usertype, day_of_week) %>% 
  ggplot() + 
  geom_bar(position = "dodge2", 
           mapping = aes(x = day_of_week, fill = usertype)) + 
  labs(title = "Total number of trips on each day of the week", 
       subtitle = "trip data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Day of the week", 
       y = "Total number of trips")

# stacked bars - better than the above. 
t_2020_to_2022 %>% 
  mutate(day_of_week = wday(t_2020_to_2022$starttime, label = TRUE)) %>% 
  group_by(usertype, day_of_week) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = day_of_week, fill = usertype)) + 
  labs(title = "Total number of trips on each day of the week", 
       subtitle = "trip data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Day of the week", 
       y = "Total number of trips")

# Chart 2) plot total number of trips per year, for each usertype. 
annual_number_of_trips <- t_2020_to_2022 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y")) %>% 
  select(trip_year, usertype, trip_id) %>% 
  group_by(trip_year, usertype)
View(annual_number_of_trips)

ggplot(data = annual_number_of_trips) + 
  geom_bar(mapping = aes(x = trip_year, fill = usertype)) +
  labs(title = "Total number of trips per year", 
       subtitle = "trip data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Year", 
       y = "Annual number of trips")

# Chart 3)  (Redundant) compare the total tripduration of the the two usertypes. 
# this is giving duplicate information as chart 2 does. 
annual_tripduration <- t_2020_to_2022 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y")) %>%  
  group_by(usertype, trip_year) %>% 
  summarize(
    annual_tripduration = sum(tripduration)
  ) 

ggplot(data = annual_tripduration) + 
  geom_point(mapping = aes(x = trip_year, y = annual_tripduration, 
                           color = usertype, size = annual_tripduration))

# Chart 4) compare the median tripduration of the the two usertypes. 
median_tripduration <- t_2020_to_2022 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y")) %>%  
  group_by(usertype, trip_year) %>% 
  summarize(
    med_duration = median(tripduration)
  ) 

ggplot(data = median_tripduration) + 
  geom_point(mapping = aes(x = trip_year, y = med_duration, 
                           color = usertype, size = med_duration))

# Chart 5) compare the number of trips by month, of the two usertypes.

num_trips_by_month <- t_2020_to_2022 %>% 
  mutate(trip_month = format(as.Date(starttime), format="%Y/%m")) %>% 
  group_by(usertype, trip_month)

ggplot(data = num_trips_by_month) +
  geom_bar(mapping = aes(x = trip_month, fill = usertype)) + 
  labs(x = "Month", y = "Number of trips per month") + 
  theme(
    axis.text.x = element_text(angle = 70, 
                               size = 9, 
                               hjust = 1, 
                               vjust = 1)
    )

# try facet the above. One for 2021, another for 2020. 
# One for customer, anotehr for subscriber. 

# Chart 6) Monthly trend of number of trips. facet wrap usertype and year

  t_2020_to_2022 %>% 
  mutate(trip_month = format(as.Date(starttime), format="%m"), 
         trip_year = format(as.Date(starttime), format = "%Y")) %>% 
  
  group_by(trip_year, trip_month, usertype) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = trip_month, fill = usertype)) +  
  
  facet_grid(usertype ~ trip_year)+  
    
  labs(x = "Month", y = "Number of trips per month") + 
  theme(
    axis.text.x = element_text(angle = 70, 
                               size = 9, 
                               hjust = 1, 
                               vjust = 1)
  )
  
## Chart 7 - 2 line graphs in one chart - monthly trips, for each usertype.
t_2020_to_2022 %>% 
  mutate(trip_month = format(as.Date(starttime), format="%Y/%m"), 
         trip_year = format(as.Date(starttime), format = "%Y")) %>% 
  group_by(trip_month, usertype) %>%  # removed usertype. 
  summarize( 
    trip_count = n()
    ) %>% 
  ggplot() +
  geom_line(mapping = aes(x = trip_month, y = trip_count, group = usertype, color = usertype)) + # added group
  
  labs(x = "Month", y = "Number of trips per month") + 
  theme(
    axis.text.x = element_text(angle = 70, 
                               size = 9, 
                               hjust = 1, 
                               vjust = 1)
  )
