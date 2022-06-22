########### for 2022 March, daylight saving adjustment for tripduration data #############
# make a back up, for verification comparison later on. 
old_t_2022 <- t_2022

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

### Verify. The time 2:00 to 2:59 does not exist on the daylight change day in March. 
# Check to make sure there doesn't exist starttime/stoptime during that.
# Else, we will need to change the value to NA. 
dt_x1_2022 <- as.POSIXct('2022-03-13 02:00:00', tz = "UTC")
dt_x2_2022 <- as.POSIXct('2022-03-13 02:59:00', tz = "UTC")

t_2022 %>% 
  filter((stoptime < dt_x2_2022 & stoptime > dt_x1_2022) & 
           (starttime < dt_x2_2022 & starttime > dt_x1_2022)) %>% 
  arrange(stoptime)

## Verify.

View(old_t_2022[t_2022$trip_id %in% vect_mar_2022, "tripduration"])
View(t_2022[t_2022$trip_id %in% vect_mar_2022, "tripduration"])
