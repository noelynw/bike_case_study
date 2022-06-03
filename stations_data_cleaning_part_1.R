#read_me: environment - run the complete source file Case_study_1_stations_01.R first. 

### 6.0 clean data in the station name column
### 6.1 whitespace removal: station_name

stations_data$station_name <- trimws(stations_data$station_name)

### 6.21 subset data for investigation 
# to look into the relationship bewteen station_id and its station name(s).
# ideally, there should be one station id corresponding to a name.
# but during the study period, some stations got relocated. 
# there are also some typos.

# To return a subset of data with only two columns: station_id and station_name, 
# grouped by station_id. It shows that a small subset of station_id is associated with multiple station_name.

id_name_mapping <- stations_data %>%   
  select(station_id, station_name) %>% 
  distinct() %>% 
  arrange(station_id, station_name)

### 6.22 to return "mapping table" for further investigation - 
# First, count how many station names are associated with each station_id. 
name_ct <- id_name_mapping %>% 
  count(station_id)

# Get a vector of station_id that are associated with more than one station_name. 
# Will be used for subsetting df. 
id_with_multi_names <- name_ct$station_id[name_ct$n != 1]

# subset the stations_data - only include station_id that are related to multiple station names. 
station_id_and_multi_names <- stations_data %>% 
  filter(station_id %in% id_with_multi_names) %>% 
  group_by(station_id) %>% 
  distinct(station_name) %>% 
  arrange(station_id)

### 6.23 investigate why there's > 1 station names for each station_id. 
# subset by station_id. 
# manually plug in the station_id. All columns are viewed. 
# Findings: some stations names have variations, some coordinates have changed over the years. 
# There are some obvious typos in the station names too. 

single_station_id_data <- stations_data %>% 
  filter(station_id == 163)
View(single_station_id_data)

# to manually view typos in their context: GPS and year info. 
# Plug in the station_id number.
# Below code uses station_id 613 as an example. 
# source of station_ids to work on : vector id_with_multi_names 
subset_for_typo_spot <- stations_data %>% 
  filter(station_id == 613 ) %>% 
  select(station_name, longitude, latitude, year)
View(subset_for_typo_spot)

### 6.231 typo correction.
# Google maps was used to confirm street names too. 

stations_data$station_name[stations_data$station_name == "DuSable Harbor"] <- "Dusable Harbor"
stations_data$station_name[stations_data$station_name == "Loomis St & Taylor St (*)"] <- "Loomis St & Taylor St"
stations_data$station_name[stations_data$station_name == "Orleans St & Elm St (*)"] <- "Orleans St & Elm St"
stations_data$station_name[stations_data$station_name == "Clinton St & Polk St (*)"] <- "Clinton St & Polk St"
stations_data$station_name[stations_data$station_name == "900 W Harrison"] <- "900 W Harrison St"  
stations_data$station_name[stations_data$station_name == "Congress Pkwy & Ogden Ave"] <- "Ogden Ave & Congress Pkwy"
stations_data$station_name[stations_data$station_name == "MLK Jr Dr & Oakwood Blvd"] <- "Martin Luther King Dr & Oakwood Blvd"
stations_data$station_name[stations_data$station_name == "Canal St & Monroe St (*)"] <- "Canal St & Monroe St"
stations_data$station_name[stations_data$station_name == "King Dr & 47th St"] <-"MLK Jr Dr & 47th St"
stations_data$station_name[stations_data$station_name == "St. Clair St & Erie St"] <- "St Clair St & Erie St"
stations_data$station_name[stations_data$station_name == "Damen Ave & Cortland Ave"] <- "Damen Ave & Cortland St"
stations_data$station_name[stations_data$station_name == "Sangamon St & Washington Blvd (*)"] <- "Sangamon St & Washington Blvd"
stations_data$station_name[stations_data$station_name == "MLK Jr Dr & 29th St"] <- "Martin Luther King Dr & 29th St"
stations_data$station_name[stations_data$station_name == "Ravenswood Ave & Montrose Ave (*)"] <- "Ravenswood Ave & Montrose Ave"
stations_data$station_name[stations_data$station_name == "Shore Drive & 55th St"] <- "Shore Dr & 55th St"
stations_data$station_name[stations_data$station_name == "Wallace Ave & 35th St"] <- "Wallace St & 35th St"
stations_data$station_name[stations_data$station_name == "Halsted St & 35th St (*)"] <- "Halsted St & 35th St"
stations_data$station_name[stations_data$station_name == "Halsted St & Blackhawk St (*)"] <- "Halsted St & Blackhawk St"
stations_data$station_name[stations_data$station_name == "Cornell Ave & Hyde Park B lvd"] <- "Cornell Ave & Hyde Park Blvd"
stations_data$station_name[stations_data$station_name == "MLK Jr Dr & 56th St (*)"] <- "MLK Jr Dr & 56th St"
stations_data$station_name[stations_data$station_name == "Washtenaw Ave & 15th St (*)"] <- "Washtenaw Ave & 15th St"
stations_data$station_name[stations_data$station_name == "Albany (Kedzie) Ave & Montrose Ave"] <- "Albany Ave & Montrose Ave"
stations_data$station_name[stations_data$station_name == "Pulaksi Rd & Eddy St"] <- "Pulaski Rd & Eddy St"

### 6.24
# among all these station_ids that are associated with multiple station names, 
# to extract station_id ("sid" in below function) that did not experience a 
# maximum change in GPS coordinates of more than 20 meters throughout years. 
# These stations for sure should have one station_name only. 
# (Side note: stations GPS data has longitude up to 5 decimal places. 
# Every 0.00001 is equivalent to 1m) 

stations_not_moved <- function(sid) {
  single_station <- stations_data %>% 
    filter(station_id == sid ) %>% 
    select(station_name, longitude, latitude, year)
  
  max_long <- max(single_station$longitude) # maximum longitude over the years 
  min_long <- min(single_station$longitude) # minimum longitude over the years
  max_lat <- max(single_station$latitude) 
  min_lat <- min(single_station$latitude)
  diff_long <- abs(max_long - min_long)*100000 # how many meters (in longitude) the station moved
  diff_lat <- abs(max_lat - min_lat)*100000
  
  if(diff_long < 20 & diff_lat < 20) {  # Station didn't move. One station_id for both station_name. 
    print(sid)
  } 
}

# this for loop returns one by one the station_ids that didn't move more than 20m throughout the years. 
# they can be used for verification purposes. 
for (sid in id_with_multi_names) {
  stations_not_moved(sid)
}

### 6.4
# data type update in stations_data.
# use mdy() to change the data type from char to date: affected columns - online_date and capacity_dated
# change from char to numeric: affected column - year. 

stations_data$capacity_dated <- mdy(stations_data$capacity_dated)
stations_data$online_date <- mdy(stations_data$online_date)
stations_data$year <- as.numeric(stations_data$year)

### 7.1 Look out for missing station_id data. 
# assuming that, station_id runs from 1 to 626. 
# to obtain a vector of all unique station_id in the stations_data.
# the vector missing_ids will be used in another script file - stations_data_cleaning_part_2
# for cross check with trips data.
ids_in_stations_data <- sort(unique(stations_data$station_id))
complete_id <- seq(1:626)
missing_ids <-complete_id[!(complete_id %in% ids_in_stations_data)]

# 7.1 follow up ##########################################################
### work with trip data, to verify our station data. 
# to do: look at station_id data in the trips data. Return a list there similarly. 
# only select columns: to_station_id, to_station_name, from_station_id, from_station_name.
# then we can compare if there're missing station data. 
###########################################################


###8.0 study the number of docking stations in each station over the years. 
# subset only station_id and dpcapacity, capacity_dated

capacity_study <- stations_data %>% 
  select(station_id, dpcapacity, capacity_dated) %>% 
  group_by(station_id) %>%  ## new
  drop_na() %>% 
  distinct() %>% 
  arrange(station_id)

View(capacity_study)

# to view the station_ids and their dpcapacity, what distinct values they have taken. 
id_and_unique_capacity <- capacity_study %>% 
  select(-capacity_dated) %>% 
  distinct() %>% 
  arrange(station_id)
View(id_and_unique_capacity)

id_and_unique_cap_ct <- id_and_unique_capacity %>% 
  group_by(station_id) %>% 
  count()

ids_with_capacity_change <- id_and_unique_cap_ct %>% 
  filter(n > 1)

View(ids_with_capacity_change) # df

# subset stations_data for a brief view of stations whose capacity experienced change over the years. 
subset_varying_capacity <- stations_data %>% 
  filter(station_id %in% ids_with_capacity_change$station_id) %>% 
  arrange(station_id)
View(subset_varying_capacity)

### 8.2 which station has the max dpcapacity? 

max(stations_data$dpcapacity) # max capacity is 55. 
stations_data %>% 
  filter(dpcapacity == 55)  #station_id  = 3 and 97. Shedd Aquarium and Museum Campus.

two_museums <- stations_data %>% 
  filter(station_id == 3 | station_id == 97) %>% 
  arrange(station_id, capacity_dated)

# unexpected findings : stations names "Museum Campus" and "Field Museum" (station_id 97) 
# are at similar locations. 
# the station did move at some point in year 2016.
# Longitude difference : 11 meters. Latitude diff: 10 meters. 

### 8.3 any negative numbers in dpcapacity? None.
stations_data %>% 
  filter(dpcapacity < 0) %>% 
  arrange(station_id)

### 8.4 rows of stations_data where dpcapactiy == 0. (potential dirty data)
questionable_capacity <- subset_varying_capacity %>% 
  filter(dpcapacity == 0)
View(questionable_capacity)

# station data of these suspicious stations. 
stations_data %>% 
  filter(stations_data$station_id %in% questionable_capacity$station_id) %>% 
  arrange(station_id)

# 8.4 follow up - in another script file. stations_data_cleaning_part_2 
# to do: look up station_id 581, 582, 622 in trip data. 
# check if there are rides dated on: 2017-12-31 (for station_id = 581, station_id = 582), 
# 2017-06-30 (for station_id = 622) 

# what might needs to be done : changing dpcapacity for those row from 0 to NA. 