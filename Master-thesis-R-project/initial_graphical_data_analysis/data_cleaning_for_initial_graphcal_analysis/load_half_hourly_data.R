#///////////////////////////////////////////////////////////////////////////////
#Load the half hourly data 
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create a tsibble with the half hourly data 
#read from a cleaned up csv file

#read the data from a csv file 
data_thirty_min <- read_csv("data/data_thirty_min.csv") %>% 
  #convert the date time to a right time format
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()