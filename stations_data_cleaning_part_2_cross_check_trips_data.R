# This file is for investigations that requires cross referencing trips data for inputs. 

# environment: run these four files first.
# case_study_1_station_01.R , stations_data_cleaning_part_1.R
# trips_01_import.R and trips_02_study_data.R first. 

# 8.4 follow up. 
# to do: look up station_id 581, 582, 622 in trip data. 
# check if there are rides dated on: 
# 2017-12-31 (for station_id = 581, station_id = 582), 
# 2017-06-30 (for station_id = 622)
# because if there exist any, it contradict what is shown in the dpcapacity. 

### 8.4.1 environment 
# run both files of cleaning and merging trip data: 
# trips_01_import.R , trips_02_study_data.R 
# then below. (Takes a while to run)
t_2013_to_2019 <- bind_rows(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019)
rm(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019, t_2020, t_2021, t_2022)

### 8.4.2
# from below codes, found no trips from station 581 and 582 on 2017/12/31. 
# cannot prove that the dpcapacity is wrong for those two stations. 
# found trips from these two stations from June 2018 onwards.
# perhaps those two stations were suspended during the first 5 months of 2018.

t_2017_station_581 <- t_2013_to_2019 %>% 
  filter(starttime >= as.Date("2017-12-30"), 
         starttime <= as.Date("2018-01-01"),
         from_station_id == "581") %>% 
  arrange(from_station_id)                          # station 581 

t_2017_station_582 <- t_2013_to_2019 %>% 
  filter(starttime >= as.Date("2017-12-30"), 
         starttime <= as.Date("2018-01-01"),
         from_station_id == "582") %>%              # station 582
  arrange(from_station_id)

View(t_2017_station_581)  # empty df. there were no trips from station 581. on 2017/12/31. 
View(t_2017_station_582)  # empty df. there were no trips from station 582. on 2017/12/31. 

t_2018_station_581 <- t_2013_to_2019 %>% 
  filter(starttime >= as.Date("2018-01-01"), 
         starttime <= as.Date("2018-12-31"),
         from_station_id == "581") %>% 
  arrange(from_station_id)                          # station 581 

t_2018_station_582 <- t_2013_to_2019 %>% 
  filter(starttime >= as.Date("2018-01-01"), 
         starttime <= as.Date("2018-12-31"),
         from_station_id == "582") %>%              # station 582
  arrange(from_station_id)

View(t_2018_station_581)  # empty df. # No trips from station 581 in year 2018. 
View(t_2018_station_582)  # empty df. # No trips from station 582 in year 2018. 

# below code - found a few trips from station 622 on 2017-06-30 and 2017-07-01 however. 
# conclusion: the dpcapacity can't be 0 on 2017-06-30 

t_2017_station_622 <- t_2013_to_2019 %>% 
  filter(from_station_id == 622, 
         starttime >= as.Date("2017-06-29"), 
         starttime <= as.Date("2017-07-02")) %>% 
  arrange(from_station_id, starttime)

View(t_2017_station_622)

# action ammend the value - in stations_data, the dpcapacity from 0 to NA. 
# for station 622, on 2017-06-30
stations_data$dpcapacity[(stations_data$station_id == 622 & stations_data$dpcapacity == 0)] <- NA

# red flag for stations data. Incomplete data. It was taken in a non-regularly manner. 

# 7.1 follow up. 
### goal: cross check station id/names extracted from trips data, with station data.
# afterall, both datasets have potential dirty data. 
# extract a subset from trip data with columns: 
# to_station_id, to_station_name, from_station_id, from_station_name
# after some combining of rows and columns, duplicates removal, 
# and cleaning typos the same way as the stations_data's station names, 
# then we can compare the two sets of dataset regarding station_id and station_names. 
# Limitations: we have station data from 2013 to 2017 only. 
# Thus we can only use trips data up from 2013 up to 2017-12-31. 
###########################################################

### 7.1.1 cross check station_id. 
# subset only 2013 - 2017 data. 
t_2013_to_2017 <- t_2013_to_2019 %>% 
  filter(starttime >= as.Date("2013-01-01"), starttime <= as.Date("2017-12-31"))

# extract the from_station id and names. For merging into a df soon. 
trips_origin_stations <- t_2013_to_2017 %>% 
  select(from_station_id, from_station_name)

# extract the to_station id and names. For merging into a df soon. 
trips_dest_stations <- t_2013_to_2017 %>% 
  select(to_station_id, to_station_name)

# change the column names to "t_station_id" and "t_station_name". 
# They represent station id and station names extracted from trips data. 
# Afterwards, merge those two dataframes then remove duplicate rows. 

trips_origin_stations <- trips_origin_stations %>% 
  rename("t_station_id" = from_station_id, 
         "t_station_name" = from_station_name, 
         )
trips_dest_stations <- trips_dest_stations %>% 
  rename("t_station_id" = to_station_id, 
         "t_station_name" = to_station_name)
  
# merge the two dfs. 
# this df has the stations id and names extracted from trip datasets. 
trips_stations_data <- bind_rows(trips_origin_stations, trips_dest_stations)

# need to trim white space in the station names, before doing the distinct()!! 
# example : stations_data$station_name <- trimws(stations_data$station_name)
trips_stations_data$t_station_name <- trimws(trips_stations_data$t_station_name)

# Remove duplicates 
trips_stations_data_distinct <- trips_stations_data %>% 
  distinct() # down to 663 rows. 

# change data type of station id from char to numeric so R can sort correctly. 
trips_stations_data_distinct$t_station_id <- as.integer(trips_stations_data_distinct$t_station_id)

# sort by station_id. 
trips_stations_data_distinct <- trips_stations_data_distinct %>% 
  arrange(t_station_id)

# check for missing ids. (this is from previously loaded .R script) 
any(missing_ids %in% trips_stations_data_distinct$t_station_id)

# Conclusion: the trip data does not suggest any missing station_id in stations_data.

###################################### 
### 7.1.2 cross check station names. 

# clean station names in  trips_stations_data_distinct -
# then compare with those station names in stations_data

### copied from previously written code : 
id_name_mapping <- stations_data %>% 
  select(station_id, station_name) %>% 
  distinct() %>% 
  arrange(station_id, station_name)

### 7.1.2.1 in trips_stations_data_distinct, ammend station names similarly to what were done in
# stations_data_cleaning_part_1.R 
# after that we could compare the two sets of station names. 

trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "DuSable Harbor"] <- "Dusable Harbor"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Loomis St & Taylor St (*)"] <- "Loomis St & Taylor St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Orleans St & Elm St (*)"] <- "Orleans St & Elm St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Clinton St & Polk St (*)"] <- "Clinton St & Polk St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "900 W Harrison"] <- "900 W Harrison St"  
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Congress Pkwy & Ogden Ave"] <- "Ogden Ave & Congress Pkwy"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "MLK Jr Dr & Oakwood Blvd"] <- "Martin Luther King Dr & Oakwood Blvd"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Canal St & Monroe St (*)"] <- "Canal St & Monroe St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "King Dr & 47th St"] <-"MLK Jr Dr & 47th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "St. Clair St & Erie St"] <- "St Clair St & Erie St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Damen Ave & Cortland Ave"] <- "Damen Ave & Cortland St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Sangamon St & Washington Blvd (*)"] <- "Sangamon St & Washington Blvd"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "MLK Jr Dr & 29th St"] <- "Martin Luther King Dr & 29th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Ravenswood Ave & Montrose Ave (*)"] <- "Ravenswood Ave & Montrose Ave"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Shore Drive & 55th St"] <- "Shore Dr & 55th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Wallace Ave & 35th St"] <- "Wallace St & 35th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Halsted St & 35th St (*)"] <- "Halsted St & 35th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Halsted St & Blackhawk St (*)"] <- "Halsted St & Blackhawk St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Cornell Ave & Hyde Park B lvd"] <- "Cornell Ave & Hyde Park Blvd"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "MLK Jr Dr & 56th St (*)"] <- "MLK Jr Dr & 56th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Washtenaw Ave & 15th St (*)"] <- "Washtenaw Ave & 15th St"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Albany (Kedzie) Ave & Montrose Ave"] <- "Albany Ave & Montrose Ave"
trips_stations_data_distinct$t_station_name[trips_stations_data_distinct$t_station_name ==  "Pulaksi Rd & Eddy St"] <- "Pulaski Rd & Eddy St"

# compare number of rows between trips_stations_data_distinct with id_name_mapping
nrow(trips_stations_data_distinct)
nrow(id_name_mapping)

# sort ascending by t_station_id. 
trips_stations_data_distinct <- trips_stations_data_distinct %>% 
  arrange(t_station_id)

# a logical vector for referencing - station names that are found in trip data but not stations_data. 
var_1 <- !(trips_stations_data_distinct$t_station_name %in% id_name_mapping$station_name)

# discrepancies df - station names from trip data that are not found in the stations_data. 
discrep <- trips_stations_data_distinct[var_1, ]
View(discrep)

# look up these station ids in stations_data. 
lookup_s <- stations_data %>%
  filter(station_id %in% discrep$t_station_id) %>% 
  select(-capacity_dated) %>% 
  arrange(station_id, station_name)
View(lookup_s)

# look up these station ids in trips data - from_station_id and from_station_name.  
lookup_t <- t_2013_to_2017 %>% 
  filter(from_station_id %in% discrep$t_station_id) %>%      #|(to_station_id %in% discrep$t_station_id))
  arrange(from_station_id)
View(lookup_t)

# according to discrep, there are 6 stations which need investigation. 
# below are manual code - repetitive look up. 
# subset data related to a specific station_id, for spotting typo or extra whitespace, or other reasons
# which might cause the discrepancies in the data. 
# Plug in the station_id into the argument when calling this function. 

study_one_station <- function(s_id) {
  temp_t_stations <- lookup_t %>%  
    filter(from_station_id == s_id) %>%
    select(from_station_id, from_station_name) %>% 
    group_by(from_station_id) %>% 
    distinct()
  View(temp_t_stations)
  
  temp_s <- lookup_s %>%  
    filter(station_id == s_id) %>% 
    arrange(year)
  View(temp_s)  
  
  temp_t_from <- lookup_t %>%  
    filter(from_station_id == s_id) %>% 
    arrange(starttime)
  View(temp_t_from)
}

# example: 
study_one_station(16)

### 7.1.2.2 correct these in trip data - not here but in R script that works specifically with trips data. 
# Will also need to look out for trips data in other years - 2018 to 2022. Did they have some other station names? 
# more investigation is needed. Might need to correct station names in 2018 to 2022 trips data as well. 

# correct these for from_station_name and to_station_name in trips data later. elsewhere, in trip data cleaning scripts. 
# id == 16,  change Paulina  Ave (Wood St) & North Ave to Wood St & North Ave
# id == 72, change Wabash Ave (State St) & 16th St to Wabash Ave & 16th St

# id == 75, the station name did change from Canal to Clinton on 2016-10_01. Then back to Canal on 2017_7_1. 
# stations_data did not reflect this name change. It was Canal in all the rows. Something's missing. 
# The GPS data of id == 75 has been all over the place throughout the years. 
# how important is the location for our question? Not overly important. Not very. Let's just add this to the report. 

# id == 80, stations_data show conflicts with trips data.
# found a lot of trips where the from_station_id is "Aberdeen St & Madison (Monroe) St".. from 2015-07-01 to 2016-01-01.
# after that, it's back to the old name. 
# according to stations_data, from 2017 march till end of july , there should be some station names with the string
# "(Monroe)" in it. 
# but there is none. All are "Aberdeen St & Monroe St". . 

# id == 198, from trips data, 
# on 2015-07-01, changed from Halsted St & Madison St to Green St (Halsted St) & Madison St. 
# on 2016-01-01, changed from "Green St (Halsted St) & Madison St" to "Green St & Madison St"
# this seems to suggest that stations_data might be not comprehensive enough. 

# id == 289, simliar to id 198, the stations_data is incomprehensive, perhaps station data points spaced too far out. 
# stations data of all those years showed, "Wells St & Concord Ln" was the only station name associated with id 289. 
# However, extracted from trips data,  "Wells St & Concord Pl" was its station name from Jul 2015 to Dec 31 2015,
# Suspicion - was "Wells St & Concord Ln" used during that time period in trips data?? checked - nope. 
# They seem to have moved the station, to "Wells St & Concord Pl" for just six months. 
# i looked on google maps. Concord Pl and Concord Ln are in close proximity to each other. 

# optional code: remove the dfs used for investigations. 
rm(capacity_study, discrep, id_and_unique_cap_ct, id_and_unique_capacity, 
   id_name_mapping, ids_with_capacity_change, lookup_s, lookup_t, name_ct, 
   questionable_capacity, single_station_id_data, station_id_and_multi_names, 
   subset_for_typo_spot, subset_varying_capacity, 
  t_2017_station_581, t_2017_station_582, t_2017_station_622, 
   t_2018_station_581, t_2018_station_582, trips_dest_stations, trips_origin_stations, 
  trips_stations_data, trips_stations_data_distinct, two_museums)
#######################################
#### end of stations_data cleaning #### 
#######################################