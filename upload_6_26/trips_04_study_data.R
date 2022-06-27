

###################################################
# below are all exploratory code # 
###################################################

###1.95 check for null values 
any(is.null(t_2020_to_2022)) # FALSE
any(is.null(t_2013_to_2019)) # FALSE

###[ Exploratory code ] 1.2 trip_id values - are they all unique? 
# Ideally, one trip_id for each row.

nrow(t_2013_to_2019)                     #21239924
length(unique(t_2013_to_2019$trip_id))   #21239924 

nrow(t_2020_to_2022)                     #9838603.            
length(unique(t_2020_to_2022$trip_id))   #9838603.

###  [ Exploratory code ] 1.3 identify which columns have NA values. 
#2020 to 2022 data: 
# from_station_name, from_station_id   
# to_station_name, to_station_id
# end_lat,end_lng

cols_NA_20_22 <- c()
for (col in  colnames(t_2020_to_2022)) {
  cols_NA_20_22 [col] <- any(is.na(t_2020_to_2022[col]))
}

cols_NA_20_22

#2013 to 2019 data
# gender, birthyear
cols_NA_13_19 <- c()
for (col in  colnames(t_2013_to_2019)) {
  cols_NA_13_19 [col] <- any(is.na(t_2013_to_2019[col]))
}

cols_NA_13_19

### [ Exploratory code ] 1.5 investigate the rideable type "docked bike". 
# what does docked_bike mean? A bike that is ready for rental? A bike not moving?

temp_11 <- t_2020_to_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  arrange(tripduration)

temp_12 <- temp_11 %>%              # the subset tells me that the "docked bike" do move... 
  filter(from_station_id != to_station_id)

# remove after viewing. 
rm(temp_11, temp_12)

### [ Exploratory code ] 1.7 some trip has the string "HQ QR". How many are there? 

t_2013_to_2019 %>%             # returns empty df. 
  filter(from_station_name == "HQ QR" | to_station_name == "HQ QR")

t_2020_to_2022 %>%             # returns some. 
  filter(from_station_name == "HQ QR" | to_station_name == "HQ QR")

# I'm curious if we can find "HQ" in the station_name columns. 

library(stringr)

HQ_ct_13_19 <- str_count(t_2013_to_2019$from_station_name, "HQ")
HQ_ct_13_19_to <- str_count(t_2013_to_2019$to_station_name, "HQ")

any(HQ_ct_13_19 > 0)             # return FALSE
any(HQ_ct_13_19_to > 0)          # return FALSE

# I'm curious if there are other station names that contain the string "HQ".
# count the number of matches to the string "HQ". 
HQ_ct_20_22 <- str_count(t_2020_to_2022$from_station_name, "HQ")
HQ_ct_20_22_to <- str_count(t_2020_to_2022$to_station_name, "HQ")

any(HQ_ct_20_22 > 0)  #returns TRUE
any(HQ_ct_20_22_to > 0)  #returns TRUE. 

HQ_subset <- data.frame(t_2020_to_2022$from_station_name , HQ_ct_20_22, 
                     t_2020_to_2022$to_station_name, HQ_ct_20_22_to)

HQ_subset <- HQ_subset %>%  # okay. Just the same 4 rows of "HQ QR". 
  filter((HQ_ct_20_22 != 0) | HQ_ct_20_22_to != 0)
