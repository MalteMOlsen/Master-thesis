
#Data cleaning

#load data and packages
source("load_half_hourly_data_and_packages.R")

#Check the name and data structure
str(data_thirty_min)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Investigating the data columns that are of interest
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

##Flow to AN tank
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  filter(is.na(flow_AN_m3_h))

#plot the time series
data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  filter(flow_AN_m3_h==0)

# data_thirty_min %>% 
#   select(flow_AN_m3_h) %>% 
#   filter_index("2019-11-05"~"2019-11-06") %>% 
#   autoplot()



##Ammonium to AN tank
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(is.na(ammonium_to_AN_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L==0)



## Temperature in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(is.na(T_PT4_C))

#plot the time series
data_thirty_min %>% 
  select(T_PT4_C) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C==0)



## Temperature in process tank 3
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(T_PT3_C) %>% 
  filter(is.na(T_PT3_C))

#plot the time series
data_thirty_min %>% 
  select(T_PT3_C) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(T_PT3_C) %>% 
  filter(T_PT3_C==0)



## Temperature in process tank 2
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(T_PT2_C) %>% 
  filter(is.na(T_PT2_C))

#plot the time series
data_thirty_min %>% 
  select(T_PT2_C) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(T_PT2_C) %>% 
  filter(T_PT2_C==0)



## Temperature in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(T_PT1_C) %>% 
  filter(is.na(T_PT1_C))

#plot the time series
data_thirty_min %>% 
  select(T_PT1_C) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C==0)

