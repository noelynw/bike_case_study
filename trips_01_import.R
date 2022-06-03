#This file does:
#import the csv trip data to data frames. Then combine trips data by the year. 2013 - 2017, 2021 to 2022. 
#Some years can't be combined yet - needs cleaning. years 2018 to 2020. 
#Cleaning begins from trips_02_study_data.R file. 

### 1.1. set up the environment. Load packages. 
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

### 1.2 to set the file directory. 
setwd("~/R/My R codes/Case_study_1_data_read_only/unzipped/all_data/stations_and_trip_data")
getwd()

### 1.3 import csv files. 
t_2013 <- read_csv("2013.csv")

t_2014_Q1Q2 <- read_csv("2014_Q1Q2.csv")
t_2014_07 <- read_csv("2014_Q3_07.csv")
t_2014_0809 <- read_csv("2014_Q3_0809.csv")
t_2014_Q4 <- read_csv("2014_Q4.csv")
                    
t_2015_Q1 <- read_csv("2015_Q1.csv")
t_2015_Q2 <- read_csv("2015_Q2.csv")
t_2015_07 <- read_csv("2015_07.csv")
t_2015_08 <- read_csv("2015_08.csv")
t_2015_09 <- read_csv("2015_09.csv")
t_2015_Q4 <- read_csv("2015_Q4.csv")
                    
t_2016_Q1 <- read_csv("2016_Q1.csv")
t_2016_04 <- read_csv("2016_04.csv")
t_2016_05 <- read_csv("2016_05.csv")
t_2016_06 <- read_csv("2016_06.csv")
t_2016_Q3 <- read_csv("2016_Q3.csv")
t_2016_Q4 <- read_csv("2016_Q4.csv")
                    
t_2017_Q1 <- read_csv("2017_Q1.csv")
t_2017_Q2 <- read_csv("2017_Q2.csv")
t_2017_Q3 <- read_csv("2017_Q3.csv")
t_2017_Q4 <- read_csv("2017_Q4.csv")

t_2018_Q1 <- read_csv("2018_Q1.csv")
t_2018_Q2 <- read_csv("2018_Q2.csv")
t_2018_Q3 <- read_csv("2018_Q3.csv")
t_2018_Q4 <- read_csv("2018_Q4.csv")

t_2019_Q1 <- read_csv("2019_Q1.csv")
t_2019_Q2 <- read_csv("2019_Q2.csv")
t_2019_Q3 <- read_csv("2019_Q3.csv")
t_2019_Q4 <- read_csv("2019_Q4.csv")

t_2020_Q1 <- read_csv("2020_Q1.csv")
t_2020_04 <- read_csv("2020_04.csv")
t_2020_05 <- read_csv("2020_05.csv")
t_2020_06 <- read_csv("2020_06.csv")
t_2020_07 <- read_csv("2020_07.csv")
t_2020_08 <- read_csv("2020_08.csv")
t_2020_09 <- read_csv("2020_09.csv")
t_2020_10 <- read_csv("2020_10.csv")
t_2020_11 <- read_csv("2020_11.csv")
t_2020_12 <- read_csv("2020_12.csv")

t_2021_01 <- read_csv("2021_01.csv")
t_2021_02 <- read_csv("2021_02.csv")
t_2021_03 <- read_csv("2021_03.csv")
t_2021_04 <- read_csv("2021_04.csv")
t_2021_05 <- read_csv("2021_05.csv")
t_2021_06 <- read_csv("2021_06.csv")
t_2021_07 <- read_csv("2021_07.csv")
t_2021_08 <- read_csv("2021_08.csv")
t_2021_09 <- read_csv("2021_09.csv")
t_2021_10 <- read_csv("2021_10.csv")
t_2021_11 <- read_csv("2021_11.csv")
t_2021_12 <- read_csv("2021_12.csv")

t_2022_01 <- read_csv("2022_01.csv")
t_2022_02 <- read_csv("2022_02.csv")
t_2022_03 <- read_csv("2022_03.csv") 
t_2022_04 <- read_csv("2022_04.csv") 

### 1.4 combine df for 2013 to 2017, 2021 to 2022 datasets. Except 2018, 2019, 2020.
# Because from manual comparison using colnames(), it was found that the column names of those 
# are different. Needs basic cleaning first. 

#t_2013 need no combining. There's only one df. 

t_2014 <- bind_rows(t_2014_Q1Q2, 
                    t_2014_07, t_2014_0809,
                    t_2014_Q4)
t_2015 <- bind_rows(t_2015_Q1,
                    t_2015_Q2,
                    t_2015_07, t_2015_08, t_2015_09,
                    t_2015_Q4)
t_2016 <- bind_rows(t_2016_Q1,
                    t_2016_04, t_2016_05, t_2016_06,
                    t_2016_Q3,
                    t_2016_Q4)
t_2017 <- bind_rows(t_2017_Q1,
                    t_2017_Q2,
                    t_2017_Q3,
                    t_2017_Q4)
t_2021 <- bind_rows(t_2021_01, t_2021_02, t_2021_03,
                    t_2021_04, t_2021_05, t_2021_06,
                    t_2021_07, t_2021_08, t_2021_09,
                    t_2021_10, t_2021_11, t_2021_12)
t_2022 <- bind_rows(t_2022_01, t_2022_02, t_2022_03, t_2022_04)


### 1.5 remove the combined imported objects (except t_2013, and 2018, 2019, 2020 data) 
# To free up memory. 
rm(t_2014_Q1Q2, t_2014_07, t_2014_0809, t_2014_Q4)
rm(t_2015_Q1, t_2015_Q2, t_2015_07, t_2015_08, t_2015_09, t_2015_Q4)
rm(t_2016_Q1, t_2016_04, t_2016_05, t_2016_06, t_2016_Q3, t_2016_Q4)

rm(t_2017_Q1, t_2017_Q2, t_2017_Q3, t_2017_Q4)

rm(t_2021_01, t_2021_02, t_2021_03, t_2021_04,
   t_2021_05, t_2021_06, t_2021_07, t_2021_08,
   t_2021_09, t_2021_10, t_2021_11, t_2021_12) 

rm(t_2022_01, t_2022_02, t_2022_03, t_2022_04) 

