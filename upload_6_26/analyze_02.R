# this file analyzes the subscribers only. 
# start out with a filtered dataset of t_2013_to_2019

# add a column "age_group"
t_2017_to_2019$age_group <- NA
t_2017_to_2019$age_group[t_2017_to_2019$age <= 15] <- "0-15" 
t_2017_to_2019$age_group[(t_2017_to_2019$age <= 24) & (t_2017_to_2019$age >=16)] <- "16-24"
t_2017_to_2019$age_group[(t_2017_to_2019$age <= 34) & (t_2017_to_2019$age >=25)] <- "25-34"
t_2017_to_2019$age_group[(t_2017_to_2019$age <= 44) & (t_2017_to_2019$age >=35)] <- "35-44"
t_2017_to_2019$age_group[(t_2017_to_2019$age <= 54) & (t_2017_to_2019$age >=45)] <- "45-54"
t_2017_to_2019$age_group[(t_2017_to_2019$age <= 64) & (t_2017_to_2019$age >=55)] <- "55-64"
t_2017_to_2019$age_group[t_2017_to_2019$age >= 64] <- "64+" 

t_2017_to_2019 <- t_2017_to_2019 %>% 
  filter(trip_id!=14193264)

t_2017_to_2019_subsc <- t_2017_to_2019 %>% 
  filter(usertype == "subscriber")

t_2017_to_2019_cust <- t_2017_to_2019 %>% 
  filter(usertype == "customer")

#Chart 1 - total number of trips across each day of week - by gender
chart_1 <- t_2017_to_2019_subsc %>% 
  filter(!is.na(gender)) %>% 
  group_by(day_of_week, gender) 
  
ggplot(data = chart_1) + 
  geom_bar(mapping = aes(x = day_of_week, fill = gender)) + 
  labs(title = "total number of trips across each day of week - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 2 - total yearly number of trips - across gender. 
chart_2 <- t_2017_to_2019_subsc %>% 
  filter(!is.na(gender)) %>% 
  group_by(trip_year, gender)

ggplot(data = chart_2) + 
  geom_bar(mapping = aes(x = trip_year, fill = gender)) +
  labs(title = "total yearly number of trips - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")
  
# chart 3 - total number of trips by the hour of the day - across gender. 
chart_3 <- t_2017_to_2019_subsc %>% 
  filter(!is.na(gender)) %>% 
  group_by(trip_hour, gender)  #  %>% 
  # summarize(
  #  ct = n()
  #)

ggplot(data = chart_3) + 
  geom_bar(mapping = aes(x = trip_hour, fill = gender)) + 
  labs(title = "total number of trips by the hour - across gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 4 - total number of trips across age - by gender. 
chart_4 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender) & !is.na(age)), age < 80) %>% 
  group_by(gender, age)

ggplot(data = chart_4) + 
  geom_bar(mapping = aes(x = age, fill = gender)) + 
  labs(title = "total number of trips by age - across gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")
  
# chart 4B - i want to facet_wrap() the gender. 
chart_4B <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender) & !is.na(age)), age < 80) %>% 
  group_by(gender, age)  

ggplot(data = chart_4B) + 
  geom_bar(mapping = aes(x = age, fill = gender)) + 
  facet_wrap(~gender) +
  labs(title = "total number of trips by age - across gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 5 - monthly number of trips - by gender.

chart_5 <- t_2017_to_2019_subsc %>% 
  filter(!is.na(gender)) %>% 
  group_by(trip_month, gender)

ggplot(data = chart_5) +
  geom_bar(mapping = aes(x = trip_month, fill = gender)) + 
  labs(title = "total monthly number of trips - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# 5B with facet wrap 
ggplot(data = chart_5) +
  geom_bar(mapping = aes(x = trip_month, fill = gender)) +
  facet_wrap(~gender) + 
  labs(title = "total monthly number of trips - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 6 - average trip duration, by gender. 
chart_6 <- t_2017_to_2019_subsc %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, trip_year) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_6, label = gender) + 
  geom_bar(mapping = aes(x = trip_year, y = avg_trip_duration, fill = gender),
           position = 'dodge',
           stat = "identity") + 
  labs(title = "yearly average trip duration - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy") + 
  geom_text(aes(label = round(avg_trip_duration), x = trip_year, y = avg_trip_duration))
    #position = position_dodge(1),
    #size = 5, 
    #vjust = -1)

# chart 7 - trip duration by age and gender
chart_7 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender)), age < 80) %>% 
  group_by(gender, age) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_7) + 
  geom_bar(mapping = aes(x = age, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "trip duration by age and gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 8-  average tripduration by day of the week, by gender
chart_8 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))) %>% 
  group_by(gender, day_of_week) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_8) + 
  geom_bar(mapping = aes(x = day_of_week, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "average trip duration by day of week - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 9 - average tripduration by month of the year, by gender
chart_9 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))) %>% 
  group_by(gender, trip_month) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_9) + 
  geom_bar(mapping = aes(x = trip_month, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "average trip duration by month - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 10 - average tripduration by the hour, by gender
chart_10 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))) %>% 
  group_by(gender, trip_hour) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_10) + 
  geom_bar(mapping = aes(x = trip_hour, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "average trip duration by hour - by gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 10B - average tripduration by the hour, by month (facet wrap)

chart_10B <- t_2017_to_2019_subsc %>% 
  #filter((!is.na(gender))) %>% 
  group_by(trip_month, trip_hour) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_10B) + 
  geom_bar(mapping = aes(x = trip_hour, y = avg_trip_duration), 
           
           stat = "identity") + 
  facet_wrap(~trip_month) +
  labs(title = "average trip duration by hour, by month", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 10 C - with bar side by side
chart_10C <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))) %>% 
  group_by(trip_month, trip_hour) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_10C) + 
  geom_bar(mapping = aes(x = trip_hour, y = avg_trip_duration, fill = trip_month), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "average trip duration by hour - by month", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 11-  average tripduration by age group and gender
chart_11 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))& (!is.na(age_group))) %>% 
  group_by(age_group, gender) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )

ggplot(data = chart_11) + 
  geom_bar(mapping = aes(x = age_group, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  labs(title = "average trip duration by age group and gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 11B-  average tripduration by day of the week, by age group, and gender
chart_11B <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))& (!is.na(age_group))) %>% 
  group_by(age_group, gender, day_of_week) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )
View(chart_11B)

ggplot(data = chart_11B) + 
  geom_bar(mapping = aes(x = age_group, y = avg_trip_duration, fill = gender), 
           position = 'dodge',
           stat = "identity") + 
  facet_wrap(~day_of_week) + 
  labs(title = "average trip duration by age group and gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 11C - try different faceting. 
chart_11C <- t_2017_to_2019_subsc %>% 
  filter((!is.na(gender))& (!is.na(age_group))) %>% 
  group_by(age_group, gender, day_of_week) %>% 
  summarize(
    avg_trip_duration = mean(tripduration)
  )
View(chart_11C)

ggplot(data = chart_11C) + 
  geom_bar(mapping = aes(x = day_of_week, y = avg_trip_duration, fill = age_group), 
           position = 'dodge',
           stat = "identity") + 
  facet_wrap(~gender) + 
  labs(title = "average trip duration by age group and gender", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")
  
# chart 11D - total number of trips by age group
chart_11D <- t_2017_to_2019_subsc %>% 
  filter((!is.na(age_group))) %>% 
  group_by(age_group) %>% 
  arrange(age_group)


ggplot(data = chart_11D) + 
  geom_bar(mapping = aes(x = age_group, fill = age_group))+
           
  labs(title = "total number of trips by each age group", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 12- tripduration by month of the year, by age group
chart_12 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(age_group))) %>% 
  group_by(age_group, trip_month) %>% 
  summarize(
    avg_tripduration = mean(tripduration)
  )

ggplot(data = chart_12) + 
  geom_bar(mapping = aes(x = trip_month, y = avg_tripduration, 
                         fill = age_group), stat = "identity") + 
  
  labs(title = "avg tripduration in each month by age group", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

# chart 13 - average tripduration by the hour, by age group
chart_13 <- t_2017_to_2019_subsc %>% 
  filter((!is.na(age_group)) & trip_id!=14193264) %>%  # remove one outlier. 
  group_by(age_group, trip_hour) %>% 
  summarize(
    avg_tripduration = mean(tripduration)
  )

ggplot(data = chart_13) + 
  geom_bar(mapping = aes(x = trip_hour, y = avg_tripduration,
                         fill = age_group), 
           stat = "identity") +
  facet_wrap(~age_group) +
  labs(title = "average tripduration by the hour, by each age group", 
       subtitle = "trip data of 2017 - 2019", 
       caption = "data collected by Divvy")

####
# found some "casual" useres with gender info. Need to look into them. user filter()
t_2017_to_2019_cust %>% 
  filter(!is.na(gender)) %>% 
  nrow 

t_2017_to_2019_cust %>% 
  filter(!is.na(age)) %>% 
  nrow

#i found, 24% of the total number of trips of "customer" type, do actually have gender and age info. 
