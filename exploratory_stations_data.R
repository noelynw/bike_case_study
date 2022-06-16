### Step 1 - prepare the environment, install and load the packages, 
# according to the Case_study_01.R 
### Step 2 - set the file directory. Then import the csv to data frames. 


# to check dimension of the data frames. 
dim(stations_2014_Q1Q2)

# to view a list of column names in all these files. 
c_2013 <- c(names(stations_2013))
c_2014_1 <- c(names(stations_2014_Q1Q2))
c_2014_2 <- c(names(stations_2014_Q3Q4))
c_2015 <- c(names(stations_2015))
c_2016_1 <- c(names(stations_2016_Q1Q2))
c_2016_2 <- c(names(stations_2016_Q3))
c_2016_3 <- c(names(stations_2016_Q4))
c_2017_1 <- c(names(stations_2017_Q1Q2))
c_2017_2 <- c(names(stations_2017_Q3Q4))

c_2013 
c_2014_1
c_2014_2
c_2015
c_2016_1
c_2016_2
c_2016_3
c_2017_1
c_2017_2

# to have an overview of the data
glimpse(stations_2013)
head(stations_2013)
str(stations_2013)

# to check data type consistency of the columns in different files. 
# e.g. dbl, chr. 
head(stations_2013)
head(stations_2014_Q1Q2)
head(stations_2014_Q3Q4)
head(stations_2015)
head(stations_2016_Q1Q2)
head(stations_2016_Q3)
head(stations_2016_Q4)
head(stations_2017_Q1Q2)
head(stations_2017_Q3Q4)

# to check for na values. 
any(is.na(stations_2013))
any(is.na(stations_2014_Q1Q2))
any(is.na(stations_2014_Q3Q4))
any(is.na(stations_2015))
any(is.na(stations_2016_Q1Q2))
any(is.na(stations_2016_Q3))
any(is.na(stations_2016_Q4))
any(is.na(stations_2017_Q1Q2))
any(is.na(stations_2017_Q3Q4))

