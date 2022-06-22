########### for 2020 March, daylight saving adjustment for tripduration data #############
# make a back up, for verification comparison later on. 
old_t_2020 <- t_2020

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

########## Now, work on November 2020 data. 

# step1) important windows in time. 
dt_11_2020 <- as.POSIXct('2020-11-01 01:00:00', tz = "UTC")
dt_12_2020 <- as.POSIXct('2020-11-01 02:00:00', tz = "UTC")


# step2) subset data that are dated around the daylight saving day. 
nov2020_DS <- t_2020 %>% 
  filter((starttime < dt_12_2020 & starttime > dt_11_2020) & (stoptime < dt_12_2020 & stoptime > dt_11_2020)) %>% 
  arrange(starttime)

# step3) subset by two cases. create reference vector for each case. 
#vect_nov_2020_case_1  <- nov2020_DS[nov2020_DS$tripduration < 0, "trip_id"]
#vect_nov_2020_case_2  <- nov2020_DS[nov2020_DS$tripduration >= 0, "trip_id"]

# correction: 
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

############## verify

View(old_t_2020[t_2020$trip_id %in% vect_mar_2020, "tripduration"])
View(t_2020[t_2020$trip_id %in% vect_mar_2020, "tripduration"])

View(t_2020[t_2020$trip_id %in% vect_nov_2020_case_1, "tripduration"])
View(old_t_2020[t_2020$trip_id %in% vect_nov_2020_case_1, "tripduration"])

View(t_2020[t_2020$trip_id %in% vect_nov_2020_case_2, "tripduration"])
View(old_t_2020[t_2020$trip_id %in% vect_nov_2020_case_2, "tripduration"])

### Verify. The time 2:00 to 2:59 does not exist on the daylight change day in March. 
# Check to make sure there doesn't exist starttime/stoptime during that.
# Else, we will need to change the value to NA. 
dt_x1_2020 <- as.POSIXct('2020-03-08 02:00:00', tz = "UTC")
dt_x2_2020 <- as.POSIXct('2020-03-08 02:59:00', tz = "UTC")

t_2020 %>% 
  filter((stoptime < dt_x2_2020 & stoptime > dt_x1_2020) & 
           (starttime < dt_x2_2020 & starttime > dt_x1_2020)) %>% 
  arrange(stoptime)