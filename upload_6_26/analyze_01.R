# this file works with 2020 - 2022 trips data analysis. 
# prerequisites : run the codes in trips_01, trips_02, trips_03.

# remove previous dfs for cleaning. 
rm(new_df_11, new_df_2, new_df_3, d2, d3, d4, difference_calc_vs_data, calc_duration_13_to_19)

# Below is, for analyze_02.R - which analyzes the data from 2017 to 2019. 
# My computer doesn't have enough memory to work with all 2013 to 2019 data.

t_2017_to_2019 <- t_2013_to_2019 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y"), 
         trip_month = format(as.Date(starttime), format="%m"), 
         trip_hour = format(as.POSIXct(starttime), format = "%H"),
         day_of_week = wday(t_2013_to_2019$starttime, label = TRUE)
  )%>% 
  filter(usertype != "Dependent", 
         (trip_year == "2017" | trip_year == "2018" | trip_year == "2019")
         )  %>% 
  select(-c(bikeid, from_station_id, from_station_name, 
            to_station_id, to_station_name)
         )
###################
mday() # returns the day of the month. 
wday() # returns the day of the week as a decimal number or
# an ordered factor if argument label = TRUE.

######################################################################
# TREND
######################################################################
# first scatterplot - day of week 
# this chart shows that, casual users tend to rent bikes for Sat and Sun. 
# whereas, subscribers tend to rent bikes during Mon-Fri more so than Sat and Sun. 

# chart 1) total number of trips on each day of the week, compare usertypes.
# double bar chart 
t_2020_to_2022 %>% 
  mutate(day_of_week = wday(t_2020_to_2022$starttime, label = TRUE)) %>% 
  group_by(usertype, day_of_week) %>% 
  ggplot() + 
  geom_bar(position = "dodge2", 
           mapping = aes(x = day_of_week, fill = usertype)) + 
  labs(title = "Total number of trips on each day of the week", 
       subtitle = "trips data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Day of the week", 
       y = "Total number of trips")

# chart 1B) stacked bars. Not as good as above. 
t_2020_to_2022 %>% 
  mutate(day_of_week = wday(t_2020_to_2022$starttime, label = TRUE)) %>% 
  group_by(day_of_week) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = day_of_week)) + 
  labs(title = "Total number of trips on each day of the week", 
       subtitle = "trips data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Day of the week", 
       y = "Total number of trips")

# Chart 2) plot total number of trips per year, for each usertype. 
annual_number_of_trips <- t_2020_to_2022 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y")) %>% 
  select(trip_year, usertype, trip_id) %>% 
  filter(trip_year != "2022") %>% 
  group_by(trip_year, usertype)

ggplot(data = annual_number_of_trips) + 
  geom_bar(mapping = aes(x = trip_year, fill = usertype)) +
  labs(title = "Total number of trips per year", 
       subtitle = "trips data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Year", 
       y = "Annual number of trips")

# Chart 4) compare the median tripduration of the the two usertypes. 
median_tripduration <- t_2020_to_2022 %>% 
  mutate(trip_year = format(as.Date(starttime), "%Y")) %>%  
  group_by(usertype, trip_year) %>% 
  summarize(
    med_duration = median(tripduration)
  ) 

ggplot(data = median_tripduration) + 
  geom_point(mapping = aes(x = trip_year, y = med_duration, 
                           color = usertype, size = med_duration)) + 
  labs(title = "median tripduration of the two usertypes", 
       subtitle = "trips data of 2020 to 2022", 
       caption = "data collected by Divvy", 
       x = "Year", 
       y = "median tripduration (secs)")

# # Chart 5) compare the number of trips by month, of the two usertypes.
# num_trips_by_month <- t_2020_to_2022 %>% 
#   mutate(trip_month = format(as.Date(starttime), format="%Y/%m")) %>% 
#   group_by(usertype, trip_month)
# 
# ggplot(data = num_trips_by_month) +
#   geom_bar(mapping = aes(x = trip_month, fill = usertype)) + 
#   labs(x = "Month", y = "Number of trips per month", 
#        title = "monthly number of trips by usertypes", 
#        subtitle = "trips data of 2020 to 2022", 
#        caption = "data collected by Divvy") + 
#   theme(
#     axis.text.x = element_text(angle = 70, 
#                                size = 9, 
#                                hjust = 1, 
#                                vjust = 1)
#     )

# Chart 6) Monthly trend of number of trips. facet_wrap usertype and year.
# compare the number of trips with same period but in different year. 
  t_2020_to_2022 %>% 
  mutate(trip_month = format(as.Date(starttime), format="%m"), 
         trip_year = format(as.Date(starttime), format = "%Y")) %>% 
  
  group_by(trip_year, trip_month, usertype) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = trip_month, fill = usertype)) +
  facet_grid(trip_year~usertype) +
  labs(x = "Month", y = "Number of trips per month", 
       title = "monthly number of trips - yearly trend by usertypes", 
       subtitle = "trips data of 2020 to 2022", 
       caption = "data collected by Divvy") + 
  theme(
    axis.text.x = element_text(angle = 70, 
                               size = 9, 
                               hjust = 1, 
                               vjust = 1)
  )
  
## Chart 7 - monthly number of trips over time, by usertype.
# t_2020_to_2022 %>% 
#   mutate(trip_month = format(as.Date(starttime), format="%Y/%m"), 
#          trip_year = format(as.Date(starttime), format = "%Y")) %>% 
#   group_by(trip_month, usertype) %>%  # removed usertype.   
#   summarize( 
#     trip_count = n()
#     ) %>% 
#   ggplot() +
#   geom_line(mapping = aes(x = trip_month, y = trip_count, 
#                           group = usertype, color = usertype, size = 0.5)) +
#   
#   labs(x = "Month", y = "Number of trips per month",
#        title = "monthly number of trips over time - by usertypes", 
#        subtitle = "trips data of 2020 to 2022", 
#        caption = "data collected by Divvy") + 
#   theme(
#     axis.text.x = element_text(angle = 70, 
#                                size = 9, 
#                                hjust = 1, 
#                                vjust = 1)
#   )

# chart 8 -
# the number of trips across 24 hours of a day, comparing the 2 usertypes, 
# from 2020 to 2021. 
temp_22 <- t_2020_to_2022 %>% 
  mutate(trip_hour = format(as.POSIXct(starttime), format = "%H"),
         trip_year = format(as.Date(starttime), format = "%Y"), 
         trip_month = format(as.Date(starttime), format = "%Y/%m")
         ) %>% 
  filter(trip_year!= "2022") %>% 
  group_by(trip_hour, usertype, trip_year) %>% 
  summarize(
    ct = n()
    ) 

# chart 8A
ggplot(data = temp_22) + 
  geom_bar(mapping = aes(x = trip_hour, y = ct, fill = usertype), stat = "identity",
  ) + 
  facet_wrap(~trip_year) + 
  labs(title = "number of trips (by the hour) across the usertypes", 
       subtitle = "trips data of 2020 to 2021", 
       caption = "data collected by Divvy", 
       x = "Hour of the day", 
       y = "Total number of trips that started during each hour")

# chart 8B
ggplot(data = temp_22) + 
  geom_bar(mapping = aes(x = trip_hour, y = ct, fill = trip_year), 
           stat = "identity", position = "dodge2") + 
  facet_wrap(~usertype) + 
  labs(title = "number of trips (by the hour) across the usertypes", 
       subtitle = "trips data of 2020 to 2021", 
       caption = "data collected by Divvy", 
       x = "Hour of the day", 
       y = "Total number of trips that started during each hour")