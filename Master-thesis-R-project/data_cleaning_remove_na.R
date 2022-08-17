
#Data cleaning - overwriting  missing values


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

#17-07-2018
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-07-17",
                              flow_AN_m3_h)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       flow_AN_m3_h)

#Replacing the zero values with NA
data_thirty_min$flow_AN_m3_h[data_thirty_min$flow_AN_m3_h==0] <- NA


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

#2018-12-11
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-12-11",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

#2020-01-10
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-01-10",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

#2020-10-20
temp <- NA_linear_approximate(data_thirty_min,
                              "2020-10-20",
                              ammonium_to_AN_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_to_AN_mg_L)

-----------------------------------------------------------------------
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

#Long time intervals
temp_counter <- counting_values_are_NA_in_a_day(data_thirty_min,
                                                ammonium_PT1_mg_L)

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

#2018-07-17
temp <- NA_linear_approximate(data_thirty_min,
                              "2018-07-17",
                              ammonium_PT2_mg_L)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       ammonium_PT2_mg_L)



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
                              T_PT1_C)


data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp,
                                       T_PT1_C)



#Overwriting the extra data from DIMS




