#///////////////////////////////////////////////////////////////////////////////
#Load model data 
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create a tsibble with the half hourly data 
#read from a cleaned up csv file

#set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data")

#read the data from a csv file 
model_data <- read_csv("model_data.csv") %>% 
  #convert the date time to a right time format
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()
