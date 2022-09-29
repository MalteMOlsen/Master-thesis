#///////////////////////////////////////////////////////////////////////////////
#Removing outliers from the one minute data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to remove the outliers which has been determined 
#in the graphical investigation of the datframe


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Run the Setup file
source("setup_for_model_data_creation.R")


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Adding the difference columns
source("adding_difference_columns.R")

count_na <- function(df,column_name){
  p <-  df %>% 
    filter(is.na({{column_name}})) 
  p%>% 
    nrow()
}



#----
#Ammonium concentration
#------------------------------------
#Max ammonium concentration is 85 mg/L, plus minus 10 in diff and the lag diff column

#Ammonium in the AN tank
data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

data_one_min$ammonium_to_AN_mg_L[data_one_min$ammonium_to_AN_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_lag1_ammonium_to_AN_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_lag1_ammonium_to_AN_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_to_AN_mg_L)

#Ammonium in process tank 1
data_one_min %>% 
  count_na(ammonium_PT1_mg_L)

data_one_min$ammonium_PT1_mg_L[data_one_min$ammonium_PT1_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_PT1_mg_L)

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT1_mg_L)

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT1_mg_L)

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_lag1_ammonium_PT1_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT1_mg_L)

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_lag1_ammonium_PT1_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT1_mg_L)


#Ammonium in process tank 2
data_one_min %>% 
  count_na(ammonium_PT2_mg_L)

data_one_min$ammonium_PT2_mg_L[data_one_min$ammonium_PT2_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_PT2_mg_L)

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT2_mg_L)

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT2_mg_L)

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_lag1_ammonium_PT2_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT2_mg_L)

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_lag1_ammonium_PT2_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT2_mg_L)


#Ammonium in process tank 3
data_one_min %>% 
  count_na(ammonium_PT3_mg_L)

data_one_min$ammonium_PT3_mg_L[data_one_min$ammonium_PT3_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_PT3_mg_L)

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT3_mg_L)

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT3_mg_L)

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_lag1_ammonium_PT3_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT3_mg_L)

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_lag1_ammonium_PT3_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT3_mg_L)


#Ammonium in process tank 4
data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

data_one_min$ammonium_PT4_mg_L[data_one_min$ammonium_PT4_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_lag1_ammonium_PT4_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_lag1_ammonium_PT4_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_PT4_mg_L)

#Ammonium in the effluent
data_one_min %>% 
  count_na(ammonium_effluent_mg_L)

data_one_min$ammonium_effluent_mg_L[data_one_min$ammonium_effluent_mg_L>85] <- NA

data_one_min %>% 
  count_na(ammonium_effluent_mg_L)

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_ammonium_effluent_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_effluent_mg_L)

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_ammonium_effluent_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_effluent_mg_L)

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_lag1_ammonium_effluent_mg_L>10] <- NA

data_one_min %>% 
  count_na(ammonium_effluent_mg_L)

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_lag1_ammonium_effluent_mg_L< -10] <- NA

data_one_min %>% 
  count_na(ammonium_effluent_mg_L)


#----
#Flow
#------------------------------------
#None








#----
#Temperature
#------------------------------------
#The values above 25 and under 2 degrees are removed, and a difference of +-1

#Temperature in process tank 1
data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$T_PT1_C>25] <- NA

data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$T_PT1_C< 2] <- NA

data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C>1] <- NA

data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$diff_lag1_T_PT1_C>1] <- NA

data_one_min %>% 
  count_na(T_PT1_C)

data_one_min$T_PT1_C[data_one_min$diff_lag1_T_PT1_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT1_C)


#Temperature in process tank 2
data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$T_PT2_C>25] <- NA

data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$T_PT2_C< 2] <- NA

data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C>1] <- NA

data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$diff_lag1_T_PT2_C>1] <- NA

data_one_min %>% 
  count_na(T_PT2_C)

data_one_min$T_PT2_C[data_one_min$diff_lag1_T_PT2_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT2_C)


#Temperature in process tank 3
data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$T_PT3_C>25] <- NA

data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$T_PT3_C< 2] <- NA

data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C>1] <- NA

data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$diff_lag1_T_PT3_C>1] <- NA

data_one_min %>% 
  count_na(T_PT3_C)

data_one_min$T_PT3_C[data_one_min$diff_lag1_T_PT3_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT3_C)


#Temperature in process tank 4
data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$T_PT4_C>25] <- NA

data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$T_PT4_C< 2] <- NA

data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C>1] <- NA

data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$diff_lag1_T_PT4_C>1] <- NA

data_one_min %>% 
  count_na(T_PT4_C)

data_one_min$T_PT4_C[data_one_min$diff_lag1_T_PT4_C< -1] <- NA

data_one_min %>% 
  count_na(T_PT4_C)



#----
#Nitrate
#------------------------------------
#Max nitrate concentration is 85 mg/L, plus minus 10 in diff and the lag diff column

#process tank 1
data_one_min %>% 
  count_na(nitrate_PT1_mg_L)

data_one_min$nitrate_PT1_mg_L[data_one_min$nitrate_PT1_mg_L>85] <- NA

data_one_min %>% 
  count_na(nitrate_PT1_mg_L)

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_nitrate_PT1_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT1_mg_L)

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_nitrate_PT1_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT1_mg_L)

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_lag1_nitrate_PT1_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT1_mg_L)

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_lag1_nitrate_PT1_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT1_mg_L)


#process tank 2
data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

data_one_min$nitrate_PT2_mg_L[data_one_min$nitrate_PT2_mg_L>85] <- NA

data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_nitrate_PT2_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_nitrate_PT2_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_lag1_nitrate_PT2_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_lag1_nitrate_PT2_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT2_mg_L)

#process tank 3
data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

data_one_min$nitrate_PT3_mg_L[data_one_min$nitrate_PT3_mg_L>85] <- NA

data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_nitrate_PT3_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_nitrate_PT3_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_lag1_nitrate_PT3_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_lag1_nitrate_PT3_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT3_mg_L)

#process tank 4
data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

data_one_min$nitrate_PT4_mg_L[data_one_min$nitrate_PT4_mg_L>85] <- NA

data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_nitrate_PT4_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_nitrate_PT4_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_lag1_nitrate_PT4_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_lag1_nitrate_PT4_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_PT4_mg_L)

#effluent
data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

data_one_min$nitrate_effluent_mg_L[data_one_min$nitrate_effluent_mg_L>85] <- NA

data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_nitrate_effluent_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_nitrate_effluent_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_lag1_nitrate_effluent_mg_L>10] <- NA

data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_lag1_nitrate_effluent_mg_L< -10] <- NA

data_one_min %>% 
  count_na(nitrate_effluent_mg_L)

#----
#Aeration
#------------------------------------
#The max broader is set to be 8000 m3/h no difference cleaning


#Process tank 1
data_one_min %>% 
  count_na(airflow_PT1_m3_h)

data_one_min$airflow_PT1_m3_h[data_one_min$airflow_PT1_m3_h>8000] <- NA

data_one_min %>% 
  count_na(airflow_PT1_m3_h)

#Process tank 2
data_one_min %>% 
  count_na(airflow_PT2_m3_h)

data_one_min$airflow_PT2_m3_h[data_one_min$airflow_PT2_m3_h>8000] <- NA

data_one_min %>% 
  count_na(airflow_PT2_m3_h)

#Process tank 3
data_one_min %>% 
  count_na(airflow_PT3_m3_h)

data_one_min$airflow_PT3_m3_h[data_one_min$airflow_PT3_m3_h>8000] <- NA


data_one_min %>% 
  count_na(airflow_PT3_m3_h)

#Process tank 4
data_one_min %>% 
  count_na(airflow_PT4_m3_h)

data_one_min$airflow_PT4_m3_h[data_one_min$airflow_PT4_m3_h>8000] <- NA

data_one_min %>% 
  count_na(airflow_PT4_m3_h)

#----
#Dissolved oxygen
#------------------------------------
#The engeering tool box is stating that the fresh water solubility is 13.9 mg/L
#at 2 degrees and salinity reduces the solubility, and the difference broader is
#set to be 5 mg/L based on how the data looks

#DO in process tank 1
data_one_min %>% 
  count_na(DO_PT1_mg_L)

data_one_min$DO_PT1_mg_L[data_one_min$DO_PT1_mg_L>13.9] <- NA

data_one_min %>% 
  count_na(DO_PT1_mg_L)

data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT1_mg_L)

data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT1_mg_L)

data_one_min$DO_PT1_mg_L[data_one_min$diff_lag1_DO_PT1_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT1_mg_L)

data_one_min$DO_PT1_mg_L[data_one_min$diff_lag1_DO_PT1_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT1_mg_L)

#DO in process tank 2
data_one_min %>% 
  count_na(DO_PT2_mg_L)

data_one_min$DO_PT2_mg_L[data_one_min$DO_PT2_mg_L>13.9] <- NA

data_one_min %>% 
  count_na(DO_PT2_mg_L)

data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT2_mg_L)

data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT2_mg_L)

data_one_min$DO_PT2_mg_L[data_one_min$diff_lag1_DO_PT2_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT2_mg_L)

data_one_min$DO_PT2_mg_L[data_one_min$diff_lag1_DO_PT2_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT2_mg_L)

#DO in process tank 3
data_one_min %>% 
  count_na(DO_PT3_mg_L)

data_one_min$DO_PT3_mg_L[data_one_min$DO_PT3_mg_L>13.9] <- NA

data_one_min %>% 
  count_na(DO_PT3_mg_L)

data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT3_mg_L)

data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT3_mg_L)

data_one_min$DO_PT3_mg_L[data_one_min$diff_lag1_DO_PT3_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT3_mg_L)

data_one_min$DO_PT3_mg_L[data_one_min$diff_lag1_DO_PT3_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT3_mg_L)


#DO in process tank 4
data_one_min %>% 
  count_na(DO_PT4_mg_L)

data_one_min$DO_PT4_mg_L[data_one_min$DO_PT4_mg_L>13.9] <- NA

data_one_min %>% 
  count_na(DO_PT4_mg_L)

data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT4_mg_L)

data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT4_mg_L)

data_one_min$DO_PT4_mg_L[data_one_min$diff_lag1_DO_PT4_mg_L>5] <- NA

data_one_min %>% 
  count_na(DO_PT4_mg_L)

data_one_min$DO_PT4_mg_L[data_one_min$diff_lag1_DO_PT4_mg_L< -5] <- NA

data_one_min %>% 
  count_na(DO_PT4_mg_L)


#----
#SS concentration
#------------------------------------
#The max concentration is chosen to be 10 mg/L and minimum od 0 and the 
#max diff is set to be 2 mg/L both are only based on the graphs, and a
#physical insistent

#SS in process tank 1 

data_one_min %>% 
  count_na(SS_PT1_g_L)

data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L>10] <- NA

data_one_min %>% 
  count_na(SS_PT1_g_L)

data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L<=0] <- NA

data_one_min %>% 
  count_na(SS_PT1_g_L)

data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L>2] <- NA

data_one_min %>% 
  count_na(SS_PT1_g_L)

data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L< -2] <- NA

data_one_min %>% 
  count_na(SS_PT1_g_L)


#SS in process tank 4 
data_one_min %>% 
  count_na(SS_PT4_g_L)

data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L>10] <- NA

data_one_min %>% 
  count_na(SS_PT4_g_L)

data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L<=0] <- NA

data_one_min %>% 
  count_na(SS_PT4_g_L)

data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L>2] <- NA

data_one_min %>% 
  count_na(SS_PT4_g_L)

data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L< -2] <- NA

data_one_min %>% 
  count_na(SS_PT4_g_L)

#SS in the effluent 
data_one_min %>% 
  count_na(SS_to_AN_g_L)

data_one_min$SS_to_AN_g_L[data_one_min$SS_to_AN_g_L>10] <- NA

data_one_min %>% 
  count_na(SS_to_AN_g_L)

data_one_min$SS_to_AN_g_L[data_one_min$SS_to_AN_g_L<=0] <- NA

data_one_min %>% 
  count_na(SS_to_AN_g_L)

data_one_min$SS_to_AN_g_L[data_one_min$diff_SS_to_AN_g_L>2] <- NA

data_one_min %>% 
  count_na(SS_to_AN_g_L)

data_one_min$SS_to_AN_g_L[data_one_min$diff_SS_to_AN_g_L< -2] <- NA

data_one_min %>% 
  count_na(SS_to_AN_g_L)
