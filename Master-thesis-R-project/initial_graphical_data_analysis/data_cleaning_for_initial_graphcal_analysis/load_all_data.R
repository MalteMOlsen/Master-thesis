#///////////////////////////////////////////////////////////////////////////////
#Load all data 
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create a tsibble for each data aggregation 
#one minut, five minutes, 15 minutes, half hourly, and hourly data 
#read from the different csv files

data_one_min <- read_csv("data/data_one_min.csv") %>% 
  #convert the date time to a right time format
  mutate(time_one_min=ymd_hms(time_one_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

data_five_min <- read_csv("data/data_five_min.csv") %>% 
  #convert the date time to a right time format
  mutate(time_five_min=ymd_hms(time_five_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

data_fifteen_min <- read_csv("data/data_fifteen_min.csv") %>% 
  #convert the date time to a right time format
  mutate(time_fifteen_min=ymd_hms(time_fifteen_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

data_thirty_min <- read_csv("data/data_thirty_min.csv") %>% 
  #convert the date time to a right time format
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

data_hour <- read_csv("data/data_hour.csv") %>% 
  #convert the date time to a right time format
  mutate(time_hour=ymd_hms(time_hour)) %>% 
  #convert the data frame to a tsibble
  as_tsibble() 


