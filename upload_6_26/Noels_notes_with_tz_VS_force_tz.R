############################################################################## 
# How is my t_2013 different from the raw, dirty 2013 csv? Are the times the same? 
# shit. R did change the clock time in those columns. Not just the tz. 

raw_t_2013 <- read_csv("2013.csv")

raw_t_2013 <- raw_t_2013 %>% 
  arrange(starttime)
view(raw_t_2013)
View(t_2013)

# learned something about the, with_tz() and force_tz(). 
# they both change the time "numbers" into the specified timezone. 
# i.e. the time in the column changes. 

# test the with_tz()
after_tz_starttime_2013 <- with_tz(raw_t_2013$starttime, tzone = "America/Chicago")
all(after_tz_starttime_2013 == raw_t_2013$starttime)

test_111 <- data.frame(after_tz_starttime_2013, raw_t_2013$starttime)

# test the force_tz()
after_forcing_starttime_2013 <- force_tz(raw_t_2013$starttime, tzone = "America/Chicago")
all(after_forcing_starttime_2013 == raw_t_2013$starttime)
test_222 <- data.frame(after_tz_starttime_2013, raw_t_2013$starttime)

####
# test the read_csv arguments. try out the locale()
raw_with_locale_2013 <- read_csv("2013.csv", 
                            col_names = TRUE, 
                            locale = locale(tz = "America/Chicago"))
raw_with_locale_2013 <- raw_with_locale_2013 %>% 
  arrange(starttime)
View(raw_with_locale_2013)

# how about t_2015? Nope. locale() didn't work because the data type of starttime is in char. 
raw_with_locale_2015 <- read_csv("2015_Q1.csv", 
                                 col_names = TRUE, 
                                 locale = locale(tz = "America/Chicago"))
raw_with_locale_2015 <- raw_with_locale_2015 %>% 
  arrange(starttime)
View(raw_with_locale_2015)
