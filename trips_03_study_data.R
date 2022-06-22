#Run both files first: trip_01_import.R, trip_02_study_data.R 

# combine the two groups of trip data. Not going further or it runs out of memory.
t_2013_to_2019 <- bind_rows(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019)
t_2020_to_2022 <- bind_rows(t_2020, t_2021, t_2022)
rm(t_2013, t_2014, t_2015, t_2016, t_2017, t_2018, t_2019, t_2020, t_2021, t_2022)

# rearrange the columns placement
t_2020_to_2022 <- t_2020_to_2022 %>%
  select(1,2,3,4,14,5,6,7,8,9,10,11,12,13)

### 1.0 remove leading/trailing whitespace in columns of character type. NEW CODE. 
t_2013_to_2019 <- t_2013_to_2019 %>% 
  mutate(across(where(is.character), str_trim))

t_2020_to_2022 <- t_2020_to_2022 %>% 
  mutate(across(where(is.character), str_trim))

### 1.1 remove duplicate row, keep only one copy of it. 
t_2013_to_2019 <- distinct(t_2013_to_2019, .keep_all = TRUE)
t_2020_to_2022 <- distinct(t_2020_to_2022, .keep_all = TRUE)

### 1.2  tripduration - data cleaning. 
# Trips less than 1 minute in duration needs to be excluded. Including negatives.
# In other words, only keep rows where tripduration is > 60 secs, and less than 24 hrs.  

t_2013_to_2019 <- t_2013_to_2019 %>%
  filter(tripduration >= 60 & tripduration <= 86400) 

t_2020_to_2022 <- t_2020_to_2022 %>%                ###### problem with CST / CDT break here. 
  filter(tripduration >= 60 & tripduration <= 86400) 

# THere could be many reasons that might possibly explain why there are negative tripduration. 
# e.g. the clock on the bike / docking station being inaccurate; formatting errors between system interface. 
# can't just swap the starttime with stoptime. it's horrible to generate unsupported data. 
# Side note : for 2020 to 2022 tripduration, they were all calculated by me. There could be negatives.
# As we have the DST "break" from 1:00 to 1:59 in November. 


###1.8 the values in column "usertype" needs to be corrected for consistency.
# usertype: "Customer" is a rider who purchased a 24-Hour Pass;
# "Subscriber" is a rider who purchased an Annual Membership

# What are the unique usertype values
unique(t_2020_to_2022$usertype)
unique(t_2013_to_2019$usertype)

# there's a mysterious usertype "Dependent". No ideas what it means. How many are there?
length(t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Dependent"]) #only 190 trips over 8 years. 

# correct the values for consistency. 
t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Customer"] <- "customer"
t_2013_to_2019$usertype[t_2013_to_2019$usertype == "Subscriber"] <- "subscriber"

t_2020_to_2022$usertype[t_2020_to_2022$usertype == "member"] <- "subscriber"
t_2020_to_2022$usertype[t_2020_to_2022$usertype == "casual"] <- "customer"


### 1.921 - 2020 to 2022 data. 
# However, for 2020 to 2022 data, I think I will have to put NA, or, in other case,
# minus 3600 from my calculated values for certain time frame. 
# doing my studies in the 06072022_daylight_saving_adjustment.R file.

