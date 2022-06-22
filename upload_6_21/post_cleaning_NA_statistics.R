##################################################################
# Right before analysis begins, final check on NA values. 

# How do we define a valid trip? 
# known starttime and stoptime (i.e. tripduration not NA. )

t_2013_to_2019 %>%   # empty dataset. Good sign.  
  filter((is.na(from_station_id) & is.na(to_station_id)) | 
           (is.na(from_station_name) & is.na(to_station_name)) | 
           (is.na(starttime) & is.na(stoptime)))

dirty_data_with_na <- t_2020_to_2022 %>%   # 549,978  rows with missing to and from stations - are all electric bike. 
  filter((is.na(from_station_id) & is.na(to_station_id)) | 
           (is.na(from_station_name) & is.na(to_station_name)) | 
           (is.na(starttime) & is.na(stoptime)))

# do they have gps data the same? 
distinct_1 <- dirty_data_with_na %>% 
  filter(start_lat == end_lat & start_lng == end_lng & 
           (!is.na(from_station_id) | !is.na(from_station_name) |
           !is.na(to_station_id) | !is.na(to_station_name))
         ) %>% 
  select(c(3,4,6, 7,8,9,10,11,12,13,14)) %>% 
  distinct() %>% 
  arrange(starttime)

unique(distinct_1$from_station_name)
unique(distinct_1$to_station_name)

#among these rows with, origin and destination id and names all NA, how many are of "subscriber" usertype? 
#It's a problem in only 2020 to 2022 data. 
# total number of trips with missing  origin and destination data (station id and name): 
# subscribers : 309587. Customer : 240220. Total trips of all usertypes : 9838603
t_2020_to_2022 %>% 
  filter(
    is.na(from_station_name) & is.na(from_station_id) & is.na(to_station_name) & is.na(to_station_id) &
      usertype == "customer"
  ) %>% 
  nrow

#found some from_station_name / to_station_name being "hubbard_test_lws" 
# how many station names contain the string "test" ??

count_test <- str_count(t_2020_to_2022$from_station_name, "test")
count_test[count_test != 0 & !is.na(count_test)]

count_test_to <- str_count(t_2020_to_2022$to_station_name, "test")
count_test_to[count_test_to != 0 & !is.na(count_test_to)]

# (2013-2019 trips - found  no station names containing the string "test")

## how many contains the underscores?
count_test <- str_count(t_2020_to_2022$from_station_name, "_")
count_test[count_test != 0 & !is.na(count_test)]

count_test_to <- str_count(t_2020_to_2022$to_station_name, "_")
count_test_to[count_test_to != 0 & !is.na(count_test_to)]

# (2013-2019 trips - found no rows with underscores in the station names.)

# usertype NA?
any(is.na(t_2020_to_2022$usertype)) # none. 
any(is.na(t_2013_to_2019$usertype)) # none. 

# it is found that, some trips have no info on origin and destination data. Just GPS. 
# maybe, some customers abandoned the bike just randomly on the street, not in the docking station. 
# that might explain why there is no station data. So, I won't delete those rows. 

t_2020_to_2022 %>% 
  filter( is.na(from_station_name) & is.na(from_station_id) & is.na(to_station_name) & is.na(to_station_id))

# it is found that, some destination station id & name & GPS are just completely missing. 
rows_with_missing_destination_data <- t_2020_to_2022 %>% 
  filter(  (is.na(start_lat) & is.na(start_lng) & is.na(from_station_name) & is.na(from_station_id)) |
           (is.na(end_lat) & is.na(end_lng) & is.na(to_station_name) & is.na(to_station_id))
  ) 

# tripduration - no NA. 
t_2020_to_2022 %>%  # empty dataset. good. 
  filter(is.na(tripduration) | is.na(starttime) | is.na(stoptime))

t_2013_to_2019 %>%  # empty dataset. good. 
  filter(is.na(tripduration) | is.na(starttime) | is.na(stoptime))

# how many % of our subscriber trip data has missing gender? 0.2%.
t_2013_to_2019 %>%  #33052 rows of subscribers trips, with missing gender
  filter(is.na(gender) & usertype == "subscriber") %>% 
  nrow
t_2013_to_2019 %>%  # nrow returns 15910997 rows of subscribers trips. 
  filter(usertype == "subscriber") %>% 
  nrow

# how many % of our subscriber trip data has missing birthyear?  0.07%
rows_with_missing_birthyear <- t_2013_to_2019 %>%  #11920 rows. 
  filter(is.na(birthyear) & usertype == "subscriber")

