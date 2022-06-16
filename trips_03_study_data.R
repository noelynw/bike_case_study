#Run both files first: trip_01_import.R, trip_02_study_data.R 

# combine the two groups of trip data. Not going further or it runs out of memory.
t_2013_to_2019 <- bind_rows(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019)
t_2020_to_2022 <- bind_rows(t_2020, t_2021, t_2022)
rm(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019, t_2020, t_2021, t_2022)

# rearrange the columns placement
t_2020_to_2022 <- t_2020_to_2022 %>%
  select(1,2,3,4,14,5,6,7,8,9,10,11,12,13)

### 1.0 check for leading/trailing whitespace in columns of type char. 
library(stringr)
any(str_starts(t_2013_to_2019$trip_id, " "))
any(str_starts(t_2013_to_2019$from_station_name, " "))
any(str_starts(t_2013_to_2019$to_station_name, " "))
any(str_starts(t_2013_to_2019$usertype, " "))
any(str_starts(t_2013_to_2019$gender[!is.na(t_2013_to_2019$gender)], " ")) 

any(str_starts(t_2020_to_2022$trip_id, " "))
any(str_starts(t_2020_to_2022$rideable_type, " "))
any(str_starts(t_2020_to_2022$from_station_name[!is.na(t_2020_to_2022$from_station_name)], " "))
any(str_starts(t_2020_to_2022$from_station_id[!is.na(t_2020_to_2022$from_station_id)], " "))
any(str_starts(t_2020_to_2022$to_station_name[!is.na(t_2020_to_2022$to_station_name)], " "))
any(str_starts(t_2020_to_2022$to_station_id[!is.na(t_2020_to_2022$to_station_id)], " "))
any(str_starts(t_2020_to_2022$usertype, " "))

# repeat for end space check. 
any(str_ends(t_2013_to_2019$trip_id, " "))
any(str_ends(t_2013_to_2019$from_station_name, " "))
any(str_ends(t_2013_to_2019$to_station_name, " "))
any(str_ends(t_2013_to_2019$usertype, " "))
any(str_ends(t_2013_to_2019$gender[!is.na(t_2013_to_2019$gender)], " ")) 

any(str_ends(t_2020_to_2022$trip_id, " "))
any(str_ends(t_2020_to_2022$rideable_type, " "))
any(str_ends(t_2020_to_2022$from_station_name[!is.na(t_2020_to_2022$from_station_name)], " "))
any(str_ends(t_2020_to_2022$from_station_id[!is.na(t_2020_to_2022$from_station_id)], " "))
any(str_ends(t_2020_to_2022$to_station_name[!is.na(t_2020_to_2022$to_station_name)], " "))
any(str_ends(t_2020_to_2022$to_station_id[!is.na(t_2020_to_2022$to_station_id)], " "))
any(str_ends(t_2020_to_2022$usertype, " "))

### 1.1 remove duplicate row, keep only one copy of it. 
t_2013_to_2019 <- distinct(t_2013_to_2019, .keep_all = TRUE)
t_2020_to_2022 <- distinct(t_2020_to_2022, .keep_all = TRUE)

### 1.2 trip_id values - are they all unique? 
# Clean data means one trip_id for each row.

nrow(t_2013_to_2019)                    
length(unique(t_2013_to_2019$trip_id))   #OK. same. 

nrow(t_2020_to_2022)                     #10011416            
length(unique(t_2020_to_2022$trip_id))   #10011207. Investigate:

# to count the occurances of each trip_id. To investigate 
# rows that shares one same trip_id
temp_13 <- t_2020_to_2022 %>% 
  select(trip_id, tripduration) %>% 
  group_by(trip_id) %>% 
  count()

# below is a df of duplicated trip_id and the count. 
temp_14 <- temp_13 %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
View(temp_14)

# to investigate the data. Found that, these duplicated trip_ids - 
# in each duplicate pair, one of them has negative values in tripduration. 
# conclusion- the data cleaning of tripduration values will take care of this. 

temp_15 <- t_2020_to_2022[t_2020_to_2022$trip_id %in% temp_14$trip_id, ]

temp_15 <- temp_15 %>% 
  arrange(trip_id)

rm(temp_13, temp_14, temp_15)

################################################################
### 1.3 identify columns which have NA values. 
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

### 1.40 debugging now on 6/2.
# clean the tripduration data. 
# source of cleaning standards to follow: readme of 2017 data
# it doesn't make sense. Everything is NA in 2018 and 2019! I have made a mistakes somewhere!!! 


# Trips less than 1 minute in duration needs to be excluded. Including negatives.
# In other words, only keep rows where tripduration is > 60 secs. 

t_2013_to_2019 <- t_2013_to_2019 %>%
  filter(tripduration >= 60) 

t_2020_to_2022 <- t_2020_to_2022 %>%
  filter(tripduration >= 60) 

# Trips greater than 24 hours in duration are excluded.

t_2013_to_2019 <- t_2013_to_2019 %>%
  filter(tripduration <= 86400) 

t_2020_to_2022 <- t_2020_to_2022 %>%
  filter(tripduration <= 86400) 

### 1.5 investigate the docked bike. 
# what does docked_bike mean? A new type of bikes? 

temp_11 <- t_2020_to_2022 %>% 
  filter(rideable_type == "docked_bike") %>% 
  arrange(tripduration)

temp_12 <- temp_11 %>% 
  filter(from_station_id == to_station_id)

# remove after viewing. 
rm(temp_11, temp_12)

### 1.6 some trips have from_station_id == to_station_id and the trip duration is 60.  
# It could be, it was returned to the same place after 1 minute of use. 
# This is not a problem. 

###1.7 some trip has the string "HQ QR". How many are there? 

t_2013_to_2019 %>%             # returns empty df. 
  filter(from_station_name == "HQ QR" | to_station_name == "HQ QR")

t_2020_to_2022 %>%             # returns some. 
  filter(from_station_name == "HQ QR" | to_station_name == "HQ QR")

# I'm curious if we can find "HQ" in the station_name columns. 

library(stringr)

HQ_ct_13_19 <- str_count(t_2013_to_2019$from_station_name, "HQ")
HQ_ct_13_19_to <- str_count(t_2013_to_2019$to_station_name, "HQ")

any(HQ_ct_13_19 > 0)
any(HQ_ct_13_19_to > 0)


# I'm curious if there are other station names that contain the string "HQ".
# count the number of matches to the string. 
HQ_ct_20_22 <- str_count(t_2020_to_2022$from_station_name, "HQ")
HQ_ct_20_22_to <- str_count(t_2020_to_2022$to_station_name, "HQ")

any(HQ_ct_20_22 > 0)  #returns TRUE
any(HQ_ct_20_22_to > 0)  #returns TRUE. 

new_df <- data.frame(t_2020_to_2022$from_station_name , HQ_ct_20_22, 
                     t_2020_to_2022$to_station_name, HQ_ct_20_22_to)

new_df <- new_df %>%  # okay. Just the same 4 rows of "HQ QR". 
  filter((HQ_ct_20_22 != 0) | HQ_ct_20_22_to != 0)


###1.8 the values in column "usertype" needs to be corrected for consistency.
# usertype: "Customer" is a rider who purchased a 24-Hour Pass;
# "Subscriber" is a rider who purchased an Annual Membership

# What are the unique usertype values
unique(t_2020_to_2022$usertype)
unique(t_2013_to_2019$usertype)

length(t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Dependent"]) #how many? only 190 trips over 8 years. 

# correct the values for consistency. 
t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Customer"] <- "customer"
t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Subscriber"] <- "subscriber"

t_2020_to_2022$usertype[t_2020_to_2022$usertype == "member"] <- "subscriber"
t_2020_to_2022$usertype[t_2020_to_2022$usertype == "casual"] <- "customer"


###1.9 Check for wrong data.
#1.91 verify that tripduration is correct. 

### 2013 - 2019 

duration_13_to_19 <- as.double((t_2013_to_2019$stoptime - t_2013_to_2019$starttime), units = "secs")
all(duration_13_to_19 == t_2013_to_2019$tripduration) # returns FALSE 

difference_13_19 <- abs(t_2013_to_2019$tripduration - duration_13_to_19)

new_df_2 <- data.frame(t_2013_to_2019$starttime, t_2013_to_2019$stoptime,
                       duration_13_to_19, t_2013_to_2019$tripduration, 
                       difference_13_19)

new_df_2 <- new_df_2 %>%  
  filter(difference_13_19 >= 1) %>% 
  arrange(desc(difference_13_19))

View(new_df_2)

#######################################################
#######################################################
#1.93 check for misspellings. Gender, birthyear. (only relevant for 2013-2019 data)
# yes. There're typos in birth year. But I don't have any other sources to verify against. 
# So, erase those rows where the age don't make sense.  
# Add an age column to save the computed age, at starttime. 
# Interestingly, there are users who are born in 2014, 2016, 2017. Rare I suppose. 

unique(t_2013_to_2019$gender)
sort(unique(t_2013_to_2019$birthyear))

#to get the year element from a datetime object. 
d2 <- as.POSIXct(t_2013_to_2019$starttime, format = "%Y-%m-%d %H:%M:%S")
d3 <- format(d2, format = "%Y")
d4 <- as.numeric(d3)  #from char to num

# create new columns : year, age 
t_2013_to_2019$year <- NULL
t_2013_to_2019$year <- d4
t_2013_to_2019$age <- t_2013_to_2019$year - t_2013_to_2019$birthyear

# Ammend values of birthyear from subscribers who are aged more than 5, less than 120. 
# change it to NA. Those rows of data, still contain usable tripduration & gender information for analysis.

temp_22 <- t_2013_to_2019 %>%
  filter(age < 10 | age > 120) %>% 
  birthyear <- 000

# below line doesn't work. Screwed up my data.
# t_2013_to_2019$age[t_2013_to_2019$age < 10 | t_2013_to_2019$age > 120] <- NA

###################################################
###1.94 add a new column - day of the week. Mon - Sun 
# use weekdays() on starttime. 




###1.95 check for null values 
any(is.null(t_2020_to_2022))
any(is.null(t_2013_to_2019))
