#this file does : 
#import station data, basic cleaning for combining rows of multiple files.

##set up the environment. Load packages. 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")  #for date and time
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# to set the file directory. 
setwd("~/R/My R codes/Case_study_1_data_read_only/unzipped/all_data/stations_and_trip_data")
getwd() 

#to import the csv to data frames. 
stations_2013 <- read_csv("Divvy_Stations_2013.csv")
stations_2014_Q1Q2 <- read_csv("Divvy_Stations_2014-Q1Q2_CSV.csv")
stations_2014_Q3Q4 <- read_csv("Divvy_Stations_2014-Q3Q4.csv")
stations_2015 <- read_csv("Divvy_Stations_2015.csv")
stations_2016_Q1Q2 <- read_csv("Divvy_Stations_2016_Q1Q2.csv")
stations_2016_Q3 <- read_csv("Divvy_Stations_2016_Q3.csv")
stations_2016_Q4 <- read_csv("Divvy_Stations_2016_Q4.csv")
stations_2017_Q1Q2 <- read_csv("Divvy_Stations_2017_Q1Q2.csv")
stations_2017_Q3Q4 <- read_csv("Divvy_Stations_2017_Q3Q4.csv")

######### basic column cleaning needed for combining dfs ######### 

### 1.1 removal of unnecessary columns
#using the function subset(df, select = -x)

#landmark 
stations_2013 <- subset(stations_2013, select = -landmark)
stations_2015 <- subset(stations_2015, select = -landmark)

#Out of curiosity
#Is there any cities other than chicago in this "city" column?
# 1 Chicago 2 Evanston 3 Oak Park. Those are suburbs of Chicago. 
stations_2017_Q1Q2 %>% 
  distinct(city)
stations_2017_Q3Q4 %>% 
  distinct(city)

#city removal 
stations_2017_Q1Q2 <- subset(stations_2017_Q1Q2, select = -city)
stations_2017_Q3Q4 <- subset(stations_2017_Q3Q4, select = -city)

#..8 removal
stations_2017_Q3Q4 <- subset(stations_2017_Q3Q4, select = -...8)

### 1.2 column name corrections

stations_2014_Q1Q2 <- stations_2014_Q1Q2 %>% 
  rename(online_date = "online date")

stations_2013 <- stations_2013 %>% 
  rename(online_date = "online date")

stations_2014_Q3Q4 <- stations_2014_Q3Q4 %>% 
  rename(online_date = dateCreated)

###1.31 some online_date is in the format of datetime. 
# to be corrected for consistency, change from datetime to date.
# First, hunt down the data file that has date time formats. 
# Below code found that, some has datetime format despite character data type
# stations_2014_Q3Q4, stations_2017_Q1Q2, stations_2017_Q3Q4 are identified. 


# the df with datetime in the online_date column's values, have strings longer than 10 char. 
# if the nchar() return  more than 10, it indicates that it's in datetime format. 
all(nchar(stations_2013$online_date) < 11)
all(nchar(stations_2014_Q1Q2$online_date) < 11)
all(nchar(stations_2014_Q3Q4$online_date) < 11)    ## has datetime
all(nchar(stations_2016_Q1Q2$online_date) < 11)
all(nchar(stations_2016_Q3$online_date) < 11)
all(nchar(stations_2016_Q4$online_date) < 11)
all(nchar(stations_2017_Q1Q2$online_date) < 11)  ## has datetime
all(nchar(stations_2017_Q3Q4$online_date) < 11)  ## has datetime 

### 1.32 correct the values in online_date column. Remove the "time" component. 
# dataf stations_2014_Q3Q4, stations_2017_Q1Q2, stations_2017_Q3Q4 are involved. 
# using the seperate() to breakdown the date and time into 2 columns. 
# and, remove the "time" column using the select()

stations_2014_Q3Q4 <- stations_2014_Q3Q4 %>% 
  separate(online_date, into = c("online_date", "time") , sep = " ") %>% 
  select(!(time))

stations_2017_Q1Q2 <- stations_2017_Q1Q2 %>% 
  separate(online_date, into = c("online_date", "time"), sep = " ") %>% 
  select(!(time))

stations_2017_Q3Q4 <- stations_2017_Q3Q4 %>% 
  separate(online_date, into = c("online_date", "time"), sep = " ") %>% 
  select(!(time))

###1.4 Add a column called "year", and "capacity_dated"

# input document : read_me files. 
# e.g. 2013 - dpcapacity: number of total docks at each station as of 2/7/2014
# the column dpcapacity includes a hidden column - its first date of validity. 
# the number of docks could change over time. 
# Thus this date is important station data that should be in our dataset.

stations_2013 <- stations_2013 %>% 
  mutate(capacity_dated = "2/7/2014", .after = 5, year="2013" )

stations_2014_Q1Q2<- stations_2014_Q1Q2 %>% 
  mutate(capacity_dated = "8/20/2014", .after = 5, year="2014")

stations_2014_Q3Q4 <- stations_2014_Q3Q4 %>% 
  mutate(capacity_dated = "12/31/2014", .after = 5, year="2014")

stations_2015 <- stations_2015 %>%   # there's no read_me file for 2015. 
  mutate(year="2015")

stations_2016_Q1Q2 <- stations_2016_Q1Q2 %>% 
  mutate(capacity_dated = "6/30/2016", .after = 5, year="2016")

stations_2016_Q3 <- stations_2016_Q3 %>% 
  mutate(capacity_dated = "9/30/2016", .after = 5, year="2016")

stations_2016_Q4 <- stations_2016_Q4 %>% 
  mutate(capacity_dated = "12/31/2016", .after = 5, year="2016")

stations_2017_Q1Q2 <- stations_2017_Q1Q2 %>% 
  mutate(capacity_dated = "6/30/2017", .after = 5, year="2017")

stations_2017_Q3Q4 <- stations_2017_Q3Q4 %>% 
  mutate(capacity_dated = "12/31/2017", .after = 5, year="2017")

######### column processing ends  ######### 

### 5.1 to combine all the years of station rows data into one data frame. 
stations_data <- bind_rows(stations_2013,
                           stations_2014_Q1Q2,
                           stations_2014_Q3Q4,
                           stations_2015,
                           stations_2016_Q1Q2,
                           stations_2016_Q3,
                           stations_2016_Q4,
                           stations_2017_Q1Q2,
                           stations_2017_Q3Q4)
  
View(stations_data)

#now that all data have been combined into a single DF, erase the original dfs. 
rm(stations_2013, stations_2014_Q1Q2, stations_2014_Q3Q4, 
   stations_2015, stations_2016_Q1Q2, stations_2016_Q3, stations_2016_Q4, 
   stations_2017_Q1Q2, stations_2017_Q3Q4)

#5.2 missing online_date column in 2015 stations data.
# thinking if it's necessary to fill in the missing values. 
# decided it is not needed as there is multiple "online_date" for each station_id. 

#5.3 change the column "id" to "station_id" to avoid confusion with trips id data
stations_data <- rename(stations_data, "station_id" = "id")
stations_data <- rename(stations_data, "station_name" = "name")

#5.4 to check if any NA is in the new column - year
any(is.na(stations_data$year))

# check for duplicate rows. None. 
any(duplicated(stations_data))
