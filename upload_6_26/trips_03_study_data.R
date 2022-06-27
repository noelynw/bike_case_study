#Run both files first: trip_01_import.R, trip_02_study_data.R 

# free up memory
rm(case_1_2020, case_1_2021, case_2_2020, case_2_2021, mar2020_DS, mar2021_DS, 
   mar2022_DS, nov2020_DS, nov2021_DS, old_t_2020, old_t_2021, old_t_2022)

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

###1.3 the values in column "usertype" needs to be corrected for consistency.
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


###1.9 Check for wrong data.
#1.91 verify that tripduration is correct. 
# in the read_me.txt CST was supposed to be the timezone they used. 
# but in those tripduration calculation, it seems some of it is CDT, CST... readme lied. g
# so i assumed that, whatever values that were in the starttime stoptime .csv, it was simply
# the clock face time in Chicago. 

###1.92 - 2013 - 2019  data. 
# Here, the calculation does not take care of the daylight saving timezone difference.
calc_duration_13_to_19 <- as.double((t_2013_to_2019$stoptime - t_2013_to_2019$starttime), units = "secs")

# Is my calculated tripduration and our given tripduration data equal?
all(calc_duration_13_to_19 == t_2013_to_2019$tripduration) # returns FALSE 

difference_calc_vs_data <- abs(t_2013_to_2019$tripduration - calc_duration_13_to_19)

# make a few vector for a dateframe. 
start_13_19 <- t_2013_to_2019$starttime
stop_13_19 <- t_2013_to_2019$stoptime
dura_13_19 <- t_2013_to_2019$tripduration
new_df_11 <- data.frame(start_13_19, stop_13_19, calc_duration_13_to_19, dura_13_19, 
                        difference_calc_vs_data)

# below code found that, all of these big difference happen on days where the clock changes. 
new_df_2 <- new_df_11 %>% 
  filter(difference_calc_vs_data >= 3500) %>% 
  arrange(start_13_19)

## below code found that, the rest of the difference is at most 60 seconds only. 
# I think it's acceptable. Coz our given starttime are not accurate down to the second! 
new_df_3 <- new_df_11 %>%  
  filter(difference_calc_vs_data < 3500 & difference_calc_vs_data > 0) %>% 
  arrange(start_13_19)

max(new_df_3$difference_calc_vs_data) #60. 

# 6,537,256 rows where, there's no difference between my calculated tripduration and the given ones. 
# 14,702,436 rows where, there's a difference of > 0 and < 60 secs. 
# 232 rows where, there's a >=3500 secs difference. Some of those are related to daylight savings. 

# conclusion: the tripduration column in the dataset is clean enough. 

#######################################################
#1.93 check for misspellings. Gender, birthyear. (only relevant for 2013-2019 data)
# yes. There're typos in birth year. But I don't have any other sources to verify against. 
# So, change those age to NA where the age don't make sense.  
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

# clean the birthyear and age columns. Change the value to NA, as there might be a typo in the birthyear.
temp_22 <- t_2013_to_2019 %>%
  filter(age < 10 | age > 115) 

t_2013_to_2019[(t_2013_to_2019$trip_id %in% temp_22$trip_id), c("birthyear", "age") ] <- NA

###################################################
###1.94 add a new column - day of the week. Mon - Sun 
# use weekdays() on starttime. or, use wday() from lubridate. 
t_2013_to_2019$day_of_week <- wday(t_2013_to_2019$starttime, label = TRUE)
t_2020_to_2022$day_of_week <- wday(t_2020_to_2022$starttime, label = TRUE)

