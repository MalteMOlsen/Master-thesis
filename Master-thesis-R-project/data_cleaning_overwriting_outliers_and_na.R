
#Data cleaning - overwriting outliers, missing values and measurement errors


#load data and packages
source("load_half_hourly_data_and_packages.R")
source("functions.R")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Flow to the AN tank
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              flow_AN_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       flow_AN_m3_h)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()

#17-07-2018
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-07-17",
                              flow_AN_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       flow_AN_m3_h)

data_thirty_min %>% 
  filter_index("2018-07-17") %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()


#Long time intervals (imported from DIMS)
temp <- counting_values_are_NA_in_a_day(data_thirty_min,
                                flow_AN_m3_h)
#---------
#Outliers
#---------

#none

#---------
#Non meaningful zeroes
#---------
#Replacing the zero values with NA
data_thirty_min$flow_AN_m3_h[data_thirty_min$flow_AN_m3_h==0] <- NA


data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium to the AN tank 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2018-07-17
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-07-17",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

data_thirty_min %>% 
  filter_index("2018-07-17") %>% 
  select(ammonium_to_AN_mg_L)%>% 
  autoplot()

#2018-12-11
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-12-11",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

data_thirty_min %>% 
  filter_index("2018-12-11") %>% 
  select(ammonium_to_AN_mg_L)%>% 
  autoplot()



#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(ammonium_to_AN_mg_L)%>% 
  autoplot()


#Long time intervals
temp <- counting_values_are_NA_in_a_day(data_thirty_min,
                                        ammonium_to_AN_mg_L)

#---------
#Outliers
#---------



#---------
#Non meaningful zeroes
#---------
  
#none



