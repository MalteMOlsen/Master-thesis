#///////////////////////////////////////////////////////////////////////////////
#Data cleaning - overwriting  missing values and grapical investigation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find the missing values which can be replaced
#make a grapical investigation of the area, and then replace the missing 
#values.
#This script is cleaned up to become "data_cleaning_remove_NA"

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphcal_analysis")

#load data and packages
source("setup.R")

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


#2020-10-20
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-10-20",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

data_thirty_min %>% 
  filter_index("2020-10-20") %>% 
  select(ammonium_to_AN_mg_L)%>% 
  autoplot()


data_thirty_min %>% 
  filter_index("2020-10-19"~"2020-10-21") %>% 
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

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain data
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#None

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_PT1_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT1_mg_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(ammonium_PT1_mg_L)%>% 
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                        ammonium_PT1_mg_L)

#---------
#Outliers
#---------
data_thirty_min %>% 
  select(ammonium_PT1_mg_L) %>% 
  filter_index("2021-04"~"2021-06") %>%
  autoplot()


#---------
#Non meaningful zeroes
#---------
 #Zero values should be possible


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium in process tank 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_PT2_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT2_mg_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(ammonium_PT2_mg_L)%>% 
  autoplot()


#2018-07-17
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-07-17",
                              ammonium_PT2_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT2_mg_L)

data_thirty_min %>% 
  filter_index("2018-07-17") %>% 
  select(ammonium_PT2_mg_L)%>% 
  autoplot()






#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                ammonium_PT2_mg_L)

#---------
#Outliers
#---------
data_thirty_min %>% 
  select(ammonium_PT2_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium in process tank 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_PT3_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT3_mg_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(ammonium_PT3_mg_L)%>% 
  autoplot()



#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                ammonium_PT3_mg_L)
                                                

#---------
#Outliers
#---------
data_thirty_min %>% 
  select(ammonium_PT3_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_PT4_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT4_mg_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(ammonium_PT4_mg_L)%>% 
  autoplot()



#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                ammonium_PT4_mg_L)


#---------
#Outliers
#---------
data_thirty_min %>% 
  select(ammonium_PT4_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#SS in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              SS_PT1_g_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       SS_PT1_g_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(SS_PT1_g_L)%>% 
  autoplot()




#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                SS_PT1_g_L)

#---------
#Outliers
#---------
data_thirty_min %>% 
  select(SS_PT1_g_L) %>%
  autoplot()
#The time series is a bit unstable were the signal go from max to min in a short time



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#SS in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              SS_PT4_g_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       SS_PT4_g_L)

data_thirty_min %>% 
  filter_index("2020-01-10") %>% 
  select(SS_PT4_g_L)%>% 
  autoplot()



#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                SS_PT4_g_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(SS_PT4_g_L) %>%
  autoplot()
#The time series is a bit unstable were the signal go from max to min in a short time

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aeration in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              airflow_PT1_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       airflow_PT1_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(airflow_PT1_m3_h)%>%
  autoplot()



#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                airflow_PT1_m3_h)


#---------
#Outliers
#---------
data_thirty_min %>% 
  select(airflow_PT1_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aeration in process tank 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              airflow_PT2_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       airflow_PT2_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(airflow_PT2_m3_h)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                airflow_PT2_m3_h)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(airflow_PT2_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aeration in process tank 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              airflow_PT3_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       airflow_PT3_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(airflow_PT3_m3_h)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                airflow_PT3_m3_h)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(airflow_PT3_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#Zero values should be possible



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aeration in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              airflow_PT4_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       airflow_PT4_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(airflow_PT4_m3_h)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                airflow_PT4_m3_h)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(airflow_PT4_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#????




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Flow to the hydrolysis tank
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              flow_HT_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       flow_HT_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(flow_HT_m3_h)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                flow_HT_m3_h)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(flow_HT_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#!!!




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Flow in the effluent
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              flow_effluent_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       flow_effluent_m3_h)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(flow_effluent_m3_h)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                flow_effluent_m3_h)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(flow_effluent_m3_h) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#!!!




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Nitrate in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              nitrate_PT1_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       nitrate_PT1_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(nitrate_PT1_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                nitrate_PT1_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(nitrate_PT1_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Nitrate in process tank 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              nitrate_PT2_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       nitrate_PT2_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(nitrate_PT2_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                nitrate_PT2_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(nitrate_PT2_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Nitrate in process tank 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              nitrate_PT3_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       nitrate_PT3_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(nitrate_PT3_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                nitrate_PT3_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(nitrate_PT3_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Nitrate in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              nitrate_PT4_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       nitrate_PT4_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(nitrate_PT4_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                nitrate_PT4_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(nitrate_PT4_mg_L) %>%
  autoplot()
#None clear outliers, maybe one or two

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Ammonium in the effluent
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_effluent_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_effluent_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(ammonium_effluent_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                ammonium_effluent_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Nitrate in the effluent
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              nitrate_effluent_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       nitrate_effluent_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(nitrate_effluent_mg_L)%>%
  autoplot()

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                nitrate_effluent_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(nitrate_effluent_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#DO in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              DO_PT1_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       DO_PT1_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(DO_PT1_mg_L)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                DO_PT1_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(DO_PT1_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#DO in process tank 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              DO_PT2_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       DO_PT2_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(DO_PT2_mg_L)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                DO_PT2_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(DO_PT2_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#DO in process tank 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              DO_PT3_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       DO_PT3_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(DO_PT3_mg_L)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                DO_PT3_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(DO_PT3_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#DO in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              DO_PT4_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       DO_PT4_mg_L)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(DO_PT4_mg_L)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                DO_PT4_mg_L)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(DO_PT4_mg_L) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#zero is an allowed value




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Temperature in process tank 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              T_PT4_C)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       T_PT4_C)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(T_PT4_C)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                T_PT4_C)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(T_PT4_C) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#This time series needs a bit of clean up




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Temperature in process tank 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              T_PT3_C)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       T_PT3_C)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(T_PT3_C)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                T_PT3_C)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(T_PT3_C) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#This time series needs a bit of clean up




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Temperature in process tank 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              T_PT2_C)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       T_PT2_C)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(T_PT2_C)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                T_PT2_C)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(T_PT2_C) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#This time series needs a bit of clean up




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Temperature in process tank 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#---------
#NA values
#---------
#Small time intervals
#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              T_PT1_C)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       T_PT1_C)

data_thirty_min %>%
  filter_index("2020-01-10") %>%
  select(T_PT1_C)%>%
  autoplot()


#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                T_PT1_C)
#---------
#Outliers
#---------
data_thirty_min %>% 
  select(T_PT1_C) %>%
  autoplot()
#None clear outliers

#---------
#Non meaningful zeroes
#---------
#This time series needs a bit of clean up
