
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

data_thirty_min %>%
  select(flow_AN_m3_h) %>%
  filter_index("2019-11-05"~"2019-11-06") %>%
  autoplot()
data_thirty_min %>%
  select(flow_AN_m3_h) %>%
  filter_index("2018-12-11") %>%
  autoplot()
data_thirty_min %>%
  select(flow_AN_m3_h) %>%
  filter_index("2020-10-20") %>%
  autoplot()



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

data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2022") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L==0)
MIN_temp_non_meaning_zero <-  data_one_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L==0)



##Rain data
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(rainfall_mm) %>% 
  filter(is.na(rainfall_mm))

#plot the time series
data_thirty_min %>% 
  select(rainfall_mm) %>% 
  autoplot()



##Ammonium in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_PT1_mg_L) %>% 
  filter(is.na(ammonium_PT1_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_PT1_mg_L) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_PT1_mg_L) %>% 
  filter(ammonium_PT1_mg_L==0)
MIN_temp_non_meaning_zero <-  data_one_min %>% 
  select(ammonium_PT1_mg_L) %>% 
  filter(ammonium_PT1_mg_L==0)



##Ammonium in process tank 2
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_PT2_mg_L) %>% 
  filter(is.na(ammonium_PT2_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_PT2_mg_L) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_PT2_mg_L) %>% 
  filter(ammonium_PT2_mg_L==0)
MIN_temp_non_meaning_zero <-  data_one_min %>% 
  select(ammonium_PT2_mg_L) %>% 
  filter(ammonium_PT2_mg_L==0)



##Ammonium in process tank 3
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_PT3_mg_L) %>% 
  filter(is.na(ammonium_PT3_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_PT3_mg_L) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_PT3_mg_L) %>% 
  filter(ammonium_PT3_mg_L==0)
MIN_temp_non_meaning_zero <-  data_one_min %>% 
  select(ammonium_PT3_mg_L) %>% 
  filter(ammonium_PT3_mg_L==0)



##Ammonium in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_PT4_mg_L) %>% 
  filter(is.na(ammonium_PT4_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_PT4_mg_L) %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_PT4_mg_L) %>% 
  filter(ammonium_PT4_mg_L==0)



##SS in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(SS_PT1_g_L) %>% 
  filter(is.na(SS_PT1_g_L))

#plot the time series
data_thirty_min %>% 
  select(SS_PT1_g_L) %>% 
  filter_index("2018-01-05"~"2018-01-07") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(SS_PT1_g_L) %>% 
  filter(SS_PT1_g_L==0)



##SS in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(SS_PT4_g_L) %>% 
  filter(is.na(SS_PT4_g_L))

#plot the time series
data_thirty_min %>% 
  select(SS_PT4_g_L) %>% 
  #filter_index("2018-01-05"~"2018-01-07") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(SS_PT4_g_L) %>% 
  filter(SS_PT4_g_L==0)



##aeration in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(airflow_PT1_m3_h) %>% 
  filter(is.na(airflow_PT1_m3_h))

#plot the time series
data_thirty_min %>% 
  select(airflow_PT1_m3_h) %>% 
  #filter_index("2018-01-05"~"2018-01-07") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(airflow_PT1_m3_h) %>% 
  filter(airflow_PT1_m3_h==0)



##aeration in process tank 2
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(airflow_PT2_m3_h) %>% 
  filter(is.na(airflow_PT2_m3_h))

#plot the time series
data_thirty_min %>% 
  select(airflow_PT2_m3_h) %>% 
  filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(airflow_PT2_m3_h) %>% 
  filter(airflow_PT2_m3_h==0)



##aeration in process tank 3
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(airflow_PT3_m3_h) %>% 
  filter(is.na(airflow_PT3_m3_h))

#plot the time series
data_thirty_min %>% 
  select(airflow_PT3_m3_h) %>% 
  filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(airflow_PT3_m3_h) %>% 
  filter(airflow_PT3_m3_h==0)



##aeration in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(airflow_PT4_m3_h) %>% 
  filter(is.na(airflow_PT4_m3_h))

#plot the time series
data_thirty_min %>% 
  select(airflow_PT4_m3_h) %>% 
  filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(airflow_PT4_m3_h) %>% 
  filter(airflow_PT4_m3_h==0)



##flow to the hydrolysis tank
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(flow_HT_m3_h) %>% 
  filter(is.na(flow_HT_m3_h))

#plot the time series
data_thirty_min %>% 
  select(flow_HT_m3_h) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(flow_HT_m3_h) %>% 
  filter(flow_HT_m3_h==0)


##flow in effluent
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  filter(is.na(flow_effluent_m3_h))

#plot the time series
data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  filter(flow_effluent_m3_h==0)



##Nitrate in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(nitrate_PT1_mg_L) %>% 
  filter(is.na(nitrate_PT1_mg_L))

#plot the time series
data_thirty_min %>% 
  select(nitrate_PT1_mg_L) %>% 
  filter_index("2018-06-05"~"2018-07-25") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(nitrate_PT1_mg_L) %>% 
  filter(nitrate_PT1_mg_L==0)




##Nitrate in process tank 2
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(nitrate_PT2_mg_L) %>% 
  filter(is.na(nitrate_PT2_mg_L))

#plot the time series
data_thirty_min %>% 
  select(nitrate_PT2_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(nitrate_PT2_mg_L) %>% 
  filter(nitrate_PT2_mg_L==0)



##Nitrate in process tank 3
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(nitrate_PT3_mg_L) %>% 
  filter(is.na(nitrate_PT3_mg_L))

#plot the time series
data_thirty_min %>% 
  select(nitrate_PT3_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(nitrate_PT3_mg_L) %>% 
  filter(nitrate_PT3_mg_L==0)




##Nitrate in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(nitrate_PT4_mg_L) %>% 
  filter(is.na(nitrate_PT4_mg_L))

#plot the time series
data_thirty_min %>% 
  select(nitrate_PT4_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(nitrate_PT4_mg_L) %>% 
  filter(nitrate_PT4_mg_L==0)



##Ammonium in the effluent
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  filter(is.na(ammonium_effluent_mg_L))

#plot the time series
data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  filter(ammonium_effluent_mg_L==0)



##Nitrate in the effluent
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(nitrate_effluent_mg_L) %>% 
  filter(is.na(nitrate_effluent_mg_L))

#plot the time series
data_thirty_min %>% 
  select(nitrate_effluent_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(nitrate_effluent_mg_L) %>% 
  filter(nitrate_effluent_mg_L==0)



#DO in process tank 1
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(DO_PT1_mg_L) %>% 
  filter(is.na(DO_PT1_mg_L))

#plot the time series
data_thirty_min %>% 
  select(DO_PT1_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(DO_PT1_mg_L) %>% 
  filter(DO_PT1_mg_L==0)



#DO in process tank 2
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(DO_PT2_mg_L) %>% 
  filter(is.na(DO_PT2_mg_L))

#plot the time series
data_thirty_min %>% 
  select(DO_PT2_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(DO_PT2_mg_L) %>% 
  filter(DO_PT2_mg_L==0)




#DO in process tank 3
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(DO_PT3_mg_L) %>% 
  filter(is.na(DO_PT3_mg_L))

#plot the time series
data_thirty_min %>% 
  select(DO_PT3_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(DO_PT3_mg_L) %>% 
  filter(DO_PT3_mg_L==0)


#DO in process tank 4
#----------
#Finding number of NA
temp_na <- data_thirty_min %>% 
  select(DO_PT4_mg_L) %>% 
  filter(is.na(DO_PT4_mg_L))

#plot the time series
data_thirty_min %>% 
  select(DO_PT4_mg_L) %>% 
  #filter_index("2018-03-12"~"2018-03-12") %>% 
  autoplot()

#finding non meaningful zero values
temp_non_meaning_zero <-  data_thirty_min %>% 
  select(DO_PT4_mg_L) %>% 
  filter(DO_PT4_mg_L==0)



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


