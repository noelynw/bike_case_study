### 1.0 Run trips_01_import.R to install &  packages, then import csv.
#environment 
install.packages(hablar)
library(hablar)

### 1.1 Study the colnames. 
# for consistency, follow the colnames in readme of 2013.
# Compare year to year. Check for discrepancies in colnames. 
# Examine manually for 2018, 2019, 2020 data as their colnames are vastly different. 
all.equal(colnames(t_2013), colnames(t_2014)) #[1] "1 string mismatch"
all.equal(colnames(t_2014), colnames(t_2015)) #TRUE. 
all.equal(colnames(t_2015), colnames(t_2016)) #TRUE. 
all.equal(colnames(t_2016), colnames(t_2017)) #[1] "2 string mismatches"
all.equal(colnames(t_2017), colnames(t_2021)) #[1] "Lengths (12, 13) differ (string compare on first 12)"
                                                          #[2] "12 string mismatches"  
all.equal(colnames(t_2021), colnames(t_2022)) #TRUE 

### 1.2 To ensure column name consistency 

### 1.20 correction of colname in t_2013. 
t_2013 <- t_2013 %>% 
  rename(birthyear = birthday)

###1.21 correct the column names in t_2018_Q1 , t_2019_Q2, to match with the data from 2013 - 2017.

t_2018_Q1_n1 <- t_2018_Q1 %>% 
  rename(trip_id = "01 - Rental Details Rental ID", 
         starttime = "01 - Rental Details Local Start Time", 
         stoptime = "01 - Rental Details Local End Time",              
         bikeid = "01 - Rental Details Bike ID",
         tripduration = "01 - Rental Details Duration In Seconds Uncapped",
         from_station_name = "03 - Rental Start Station Name",                  
         to_station_name = "02 - Rental End Station Name",                    
         from_station_id = "03 - Rental Start Station ID",                    
         to_station_id = "02 - Rental End Station ID",                      
         usertype = "User Type",                                       
         gender = "Member Gender",
         birthyear = "05 - Member Details Member Birthday Year")

t_2019_Q2_n1 <- t_2019_Q2 %>% 
  rename(trip_id = "01 - Rental Details Rental ID", 
         starttime = "01 - Rental Details Local Start Time", 
         stoptime = "01 - Rental Details Local End Time",              
         bikeid = "01 - Rental Details Bike ID",
         tripduration = "01 - Rental Details Duration In Seconds Uncapped",
         from_station_name = "03 - Rental Start Station Name",                  
         to_station_name = "02 - Rental End Station Name",                    
         from_station_id = "03 - Rental Start Station ID",                    
         to_station_id = "02 - Rental End Station ID",                      
         usertype = "User Type",                                       
         gender = "Member Gender",
         birthyear = "05 - Member Details Member Birthday Year")

### 1.22 check for discrepancies in colnames year by year. 
all.equal(colnames(t_2018_Q1_n1), colnames(t_2018_Q2))  #[1] "2 string mismatches"
all.equal(colnames(t_2018_Q2), colnames(t_2018_Q3)) #TRUE
all.equal(colnames(t_2018_Q3), colnames(t_2018_Q4)) #TRUE

all.equal(colnames(t_2019_Q1), colnames(t_2019_Q2_n1))  #[1] "2 string mismatches"
all.equal(colnames(t_2019_Q2_n1), colnames(t_2019_Q3))  #[1] "2 string mismatches"
all.equal(colnames(t_2019_Q3), colnames(t_2019_Q4))  #TRUE

#investigate each using colnames(). Code has been omitted here for brevity. 

### 1.23 change colnames : start_time, end_time to starttime, stoptime. 
# 2017, 2018, 2019 data. 

t_2017 <- t_2017 %>%                        #merged 2017 data. 
  rename(starttime = start_time, stoptime = end_time)

t_2018_Q2 <- t_2018_Q2 %>% 
  rename(starttime = start_time, stoptime = end_time)

t_2018_Q3 <- t_2018_Q3 %>% 
  rename(starttime = start_time, stoptime = end_time)

t_2018_Q4 <- t_2018_Q4 %>% 
  rename(starttime = start_time, stoptime = end_time)

t_2019_Q1 <- t_2019_Q1 %>% 
  rename(starttime = start_time, stoptime = end_time)

t_2019_Q3 <- t_2019_Q3 %>% 
  rename(starttime = start_time, stoptime = end_time)

t_2019_Q4 <- t_2019_Q4 %>% 
  rename(starttime = start_time, stoptime = end_time)

###1.4  combine 2018, 2019 data. 
t_2018 <- bind_rows(t_2018_Q1_n1, t_2018_Q2, t_2018_Q3, t_2018_Q4) #merged 2018 data. 

t_2019 <- bind_rows(t_2019_Q1, t_2019_Q2_n1, t_2019_Q3, t_2019_Q4 ) #merged 2019 data. 

###1.51 change the data type from num to chr, 
#affected columns: trip_id, from_station_id and to_station_id 
#affected data: 2013 to 2019

t_2013 <- t_2013 %>%  
  mutate(trip_id = as.character(trip_id), 
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2014 <- t_2014 %>%  
  mutate(trip_id = as.character(trip_id), 
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2015 <- t_2015 %>%  
  mutate(trip_id = as.character(trip_id),
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2016 <- t_2016 %>%  
  mutate(trip_id = as.character(trip_id),
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2017 <- t_2017 %>%  
  mutate(trip_id = as.character(trip_id),
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2018 <- t_2018 %>%  
  mutate(trip_id = as.character(trip_id),
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

t_2019 <- t_2019 %>%  
  mutate(trip_id = as.character(trip_id),
         from_station_id = as.character(from_station_id),
         to_station_id = as.character(to_station_id)
         )

### 1.6 investigate the colnames of 2020, 2021, 2022 data. 
# used colnames()

###1.61 investigation
# tried to combine all the monthly 2020 dfs before cleaning colnames. 
# can't. the start_station_id , end_station_id data type is inconsistent.
# most is dbl, december's df has it in chr. 

head(t_2020_12) #char. And in 2021 2022 data, start_station_d and end _station_id is char.

###1.62 combine January to November 2020 trips data
t_2020_jan_to_nov <- bind_rows(t_2020_Q1, t_2020_04, t_2020_05, t_2020_06,  
                       t_2020_07, t_2020_08, t_2020_09, t_2020_10, t_2020_11)

###1.63 coerce station ids from dbl to char, for t_2020_jan_to_nov
# affected columns : start_station_id , end_station_id 
# need the convert() from package hablar.

t_2020_jan_to_nov <- t_2020_jan_to_nov %>% 
  convert(chr(start_station_id, end_station_id))

###1.65 here, combine all 2020 dataframes. 

t_2020 <- bind_rows(t_2020_jan_to_nov, t_2020_12)   # merged 2020 trip data. 

###1.7 now the data are ready for the column names correction. 
# affected data : 2020, 2021, 2022

t_2020 <- t_2020 %>%  
  rename(trip_id = ride_id,
         starttime = started_at, 
         stoptime = ended_at, 
         from_station_name = start_station_name,
         from_station_id = start_station_id, 
         to_station_name = end_station_name, 
         to_station_id = end_station_id, 
         usertype = member_casual)

t_2021 <- t_2021 %>%  
  rename(trip_id = ride_id,
         starttime = started_at, 
         stoptime = ended_at, 
         from_station_name = start_station_name,
         from_station_id = start_station_id, 
         to_station_name = end_station_name, 
         to_station_id = end_station_id, 
         usertype = member_casual)

t_2022 <- t_2022 %>%  
  rename(trip_id = ride_id,
         starttime = started_at, 
         stoptime = ended_at, 
         from_station_name = start_station_name,
         from_station_id = start_station_id, 
         to_station_name = end_station_name, 
         to_station_id = end_station_id, 
         usertype = member_casual)

### 1.8 change the datatype of startime and stoptime, from chr to dttm. 
# for years: 2014 to 2017. 
t_2014 <- t_2014 %>%
  mutate(starttime = strptime(starttime, "%m/%d/%Y %H:%M", tz ="UTC"), 
         stoptime = strptime(stoptime, "%m/%d/%Y %H:%M", tz ="UTC"))

t_2015 <- t_2015 %>%
  mutate(starttime = strptime(starttime, "%m/%d/%Y %H:%M", tz ="UTC"), 
         stoptime = strptime(stoptime, "%m/%d/%Y %H:%M", tz ="UTC"))

t_2016 <- t_2016 %>%
  mutate(starttime = strptime(starttime, "%m/%d/%Y %H:%M", tz ="UTC"), 
         stoptime = strptime(stoptime, "%m/%d/%Y %H:%M", tz ="UTC"))

t_2017 <- t_2017 %>%
  mutate(starttime = strptime(starttime, "%m/%d/%Y %H:%M", tz ="UTC"), 
         stoptime = strptime(stoptime, "%m/%d/%Y %H:%M", tz ="UTC"))

# 2013, and 2018 - 2022 onwards the starttime and stoptime are in dttm to begin with, not in char. 

### 1.9  calculate tripduration for 2020 to 2022 trips data, 
# now that the timezone of the starttime and stoptime has been set,
# which takes care of daylight savings troubles. 

#create new column, tripduration. 

t_2020 <- t_2020 %>% 
  mutate(tripduration = difftime(stoptime, starttime), .before = 5) %>% 
  convert(num(tripduration))

t_2021 <- t_2021 %>% 
  mutate(tripduration = difftime(stoptime, starttime), .before = 5) %>% 
  convert(num(tripduration))

t_2022 <- t_2022 %>% 
  mutate(tripduration = difftime(stoptime, starttime), .before = 5) %>% 
  convert(num(tripduration))

#free up memory space. Now that all the dfs have been merged. 
rm(t_2018_Q1, t_2018_Q1_n1, t_2018_Q2, t_2018_Q3, t_2018_Q4, 
   t_2019_Q1, t_2019_Q2, t_2019_Q2_n1, t_2019_Q3, t_2019_Q4,
   t_2020_Q1, t_2020_04, t_2020_05, t_2020_06, t_2020_07, t_2020_08, 
   t_2020_09, t_2020_10, t_2020_11, t_2020_12, t_2020_jan_to_nov)

### 1.91 adjust the calculated tripduration for daylight saving time. 2020 to 2022. 

# make a back up, for verification comparison later on. 
old_t_2020 <- t_2020
old_t_2021 <- t_2021
old_t_2022 <- t_2022

########### for 2020 March daylight saving adjustment 

# step1)  important window for year 2020's daylight saving day. 
dt_01_2020 <- as.POSIXct('2020-03-07 02:00:00', tz = "UTC")
dt_02_2020 <- as.POSIXct('2020-03-08 02:00:00', tz = "UTC")
dt_03_2020 <- as.POSIXct('2020-03-08 03:00:00', tz = "UTC")
dt_04_2020 <- as.POSIXct('2020-03-09 03:00:00', tz = "UTC")

# step2) subset data that are dated around the daylight saving in march 2020.
mar2020_DS <- t_2020 %>% 
  filter((stoptime < dt_04_2020 & stoptime > dt_03_2020) & 
           (starttime < dt_02_2020 & starttime > dt_01_2020)) %>% 
  arrange(stoptime)

# step3) reference vector containing the trip_id of rows whose tripduration needs to minus 3600. 
vect_mar_2020 <- mar2020_DS$trip_id   

# step4)  correct the tripduration data. Minus 3600 secs. 
t_2020[t_2020$trip_id %in% vect_mar_2020, "tripduration"] <- t_2020[(t_2020$trip_id %in% vect_mar_2020), "tripduration"] - 3600

########## for 2020 November daylight saving adjustment. 

# step1) important windows in time. 
dt_11_2020 <- as.POSIXct('2020-11-01 01:00:00', tz = "UTC")
dt_12_2020 <- as.POSIXct('2020-11-01 02:00:00', tz = "UTC")

# step2) subset data that are dated around the daylight saving day. 
nov2020_DS <- t_2020 %>% 
  filter((starttime < dt_12_2020 & starttime > dt_11_2020) & (stoptime < dt_12_2020 & stoptime > dt_11_2020)) %>% 
  arrange(starttime)

# step3) subset by two cases. create reference vector for each case. 
case_1_2020 <- nov2020_DS %>% 
  filter(tripduration < 0)

case_2_2020 <- nov2020_DS %>% 
  filter(tripduration >= 0)

vect_nov_2020_case_1 <- case_1_2020$trip_id 
vect_nov_2020_case_2 <- case_2_2020$trip_id

# step4) correct the tripduration data. 
# For case_1, plus 3600 secs. For case_2, put NA in the tripduration coz 
# we don't know if it is 1:01 a.m. CST or CDT... 

# case 1
t_2020[t_2020$trip_id %in% vect_nov_2020_case_1, "tripduration"] <- t_2020[(t_2020$trip_id %in% vect_nov_2020_case_1), "tripduration"] + 3600

# case 2 
t_2020[t_2020$trip_id %in% vect_nov_2020_case_2, "tripduration"] <- NA

########### for 2021 March, daylight saving adjustment 

# step1)  important window for year 2021's daylight saving day. 
dt_01_2021 <- as.POSIXct('2021-03-13 02:00:00', tz = "UTC")
dt_02_2021 <- as.POSIXct('2021-03-14 02:00:00', tz = "UTC")
dt_03_2021 <- as.POSIXct('2021-03-14 03:00:00', tz = "UTC")
dt_04_2021 <- as.POSIXct('2021-03-15 03:00:00', tz = "UTC")

# step2) subset data that are dated around the daylight saving in march 2021.
mar2021_DS <- t_2021 %>% 
  filter((stoptime < dt_04_2021 & stoptime > dt_03_2021) & 
           (starttime < dt_02_2021 & starttime > dt_01_2021)) %>% 
  arrange(stoptime)

# step3) reference vector containing the trip_id of rows whose tripduration needs to minus 3600. 
vect_mar_2021 <- mar2021_DS$trip_id   

# step4)  correct the tripduration data. Minus 3600 secs. 
t_2021[t_2021$trip_id %in% vect_mar_2021, "tripduration"] <- t_2021[(t_2021$trip_id %in% vect_mar_2021), "tripduration"] - 3600

########## for 2021 November daylight saving adjustment. 

# step1) important windows in time. 
dt_11_2021 <- as.POSIXct('2021-11-07 01:00:00', tz = "UTC")
dt_12_2021 <- as.POSIXct('2021-11-07 02:00:00', tz = "UTC")

# step2) subset data that are dated around the daylight saving day. 
nov2021_DS <- t_2021 %>% 
  filter((starttime < dt_12_2021 & starttime > dt_11_2021) & (stoptime < dt_12_2021 & stoptime > dt_11_2021)) %>% 
  arrange(starttime)

# step3) subset by two cases. create reference vector for each case. 
case_1_2021 <- nov2021_DS %>% 
  filter(tripduration < 0)

case_2_2021 <- nov2021_DS %>% 
  filter(tripduration >= 0)

vect_nov_2021_case_1 <- case_1_2021$trip_id 
vect_nov_2021_case_2 <- case_2_2021$trip_id

# step4) correct the tripduration data. 

t_2021[(t_2021$trip_id %in% vect_nov_2021_case_1), "tripduration"] <- t_2021[t_2021$trip_id %in% vect_nov_2021_case_1, "tripduration"] + 3600
t_2021[(t_2021$trip_id %in% vect_nov_2021_case_2), "tripduration"] <- NA

########### for 2022 March, daylight saving adjustment 

# step1)  important window for year 2022's daylight saving day. 
dt_01_2022 <- as.POSIXct('2022-03-12 02:00:00', tz = "UTC")
dt_02_2022 <- as.POSIXct('2022-03-13 02:00:00', tz = "UTC")
dt_03_2022 <- as.POSIXct('2022-03-13 03:00:00', tz = "UTC")
dt_04_2022 <- as.POSIXct('2022-03-14 03:00:00', tz = "UTC")

# step2) subset data that are dated around the daylight saving in march 2022.
mar2022_DS <- t_2022 %>% 
  filter((stoptime < dt_04_2022 & stoptime > dt_03_2022) & 
           (starttime < dt_02_2022 & starttime > dt_01_2022)) %>% 
  arrange(stoptime)

# step3) reference vector containing the trip_id of rows whose tripduration needs to minus 3600. 
vect_mar_2022 <- mar2022_DS$trip_id   

# step4)  correct the tripduration data. Minus 3600 secs. 
t_2022[t_2022$trip_id %in% vect_mar_2022, "tripduration"] <- t_2022[(t_2022$trip_id %in% vect_mar_2022), "tripduration"] - 3600

##############################################################
### Verify. The time 2:00 to 2:59 does not exist on the daylight change day in March. 
# Check to make sure there doesn't exist starttime/stoptime during that.
# Else, we will need to change the value to NA. 

dt_x1_2020 <- as.POSIXct('2020-03-08 02:00:00', tz = "UTC")
dt_x2_2020 <- as.POSIXct('2020-03-08 02:59:00', tz = "UTC")

t_2020 %>% 
  filter((stoptime < dt_x2_2020 & stoptime > dt_x1_2020) & 
           (starttime < dt_x2_2020 & starttime > dt_x1_2020)) %>% 
  arrange(stoptime)

dt_x1_2021 <- as.POSIXct('2021-03-14 02:00:00', tz = "UTC")
dt_x2_2021 <- as.POSIXct('2021-03-14 02:59:00', tz = "UTC")

t_2021 %>% 
  filter((stoptime < dt_x2_2021 & stoptime > dt_x1_2021) & 
           (starttime < dt_x2_2021 & starttime > dt_x1_2021)) %>% 
  arrange(stoptime)

dt_x1_2022 <- as.POSIXct('2022-03-13 02:00:00', tz = "UTC")
dt_x2_2022 <- as.POSIXct('2022-03-13 02:59:00', tz = "UTC")

t_2022 %>% 
  filter((stoptime < dt_x2_2022 & stoptime > dt_x1_2022) & 
           (starttime < dt_x2_2022 & starttime > dt_x1_2022)) %>% 
  arrange(stoptime)

##############################################################
### verify each pair

View(old_t_2020[t_2020$trip_id %in% vect_mar_2020, "tripduration"])
View(t_2020[t_2020$trip_id %in% vect_mar_2020, "tripduration"])

View(t_2020[t_2020$trip_id %in% vect_nov_2020_case_1, "tripduration"])
View(old_t_2020[t_2020$trip_id %in% vect_nov_2020_case_1, "tripduration"])

View(t_2020[t_2020$trip_id %in% vect_nov_2020_case_2, "tripduration"])
View(old_t_2020[t_2020$trip_id %in% vect_nov_2020_case_2, "tripduration"])

View(old_t_2021[t_2021$trip_id %in% vect_mar_2021, "tripduration"])
View(t_2021[t_2021$trip_id %in% vect_mar_2021, "tripduration"])

View(t_2021[t_2021$trip_id %in% vect_nov_2021_case_1, "tripduration"])
View(old_t_2021[t_2021$trip_id %in% vect_nov_2021_case_1, "tripduration"])

View(t_2021[t_2021$trip_id %in% vect_nov_2021_case_2, "tripduration"])
View(old_t_2021[t_2021$trip_id %in% vect_nov_2021_case_2, "tripduration"])

View(old_t_2022[t_2022$trip_id %in% vect_mar_2022, "tripduration"])
View(t_2022[t_2022$trip_id %in% vect_mar_2022, "tripduration"])