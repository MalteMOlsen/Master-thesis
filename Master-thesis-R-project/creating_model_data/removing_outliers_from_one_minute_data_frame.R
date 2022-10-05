#///////////////////////////////////////////////////////////////////////////////
#Removing outliers from the one minute data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to remove the outliers which has been determined 
#in the graphical investigation of the one minute data frame

#----
#Ammonium concentration
#------------------------------------
#Max ammonium concentration is 85 mg/L, plus minus 10 in diff

#Ammonium in the AN tank
data_one_min$ammonium_to_AN_mg_L[data_one_min$ammonium_to_AN_mg_L>85] <- NA

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L>10] <- NA

data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L< -10] <- NA


#Ammonium in process tank 1
data_one_min$ammonium_PT1_mg_L[data_one_min$ammonium_PT1_mg_L>85] <- NA

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L>10] <- NA

data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L< -10] <- NA


#Ammonium in process tank 2
data_one_min$ammonium_PT2_mg_L[data_one_min$ammonium_PT2_mg_L>85] <- NA

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L>10] <- NA

data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L< -10] <- NA


#Ammonium in process tank 3
data_one_min$ammonium_PT3_mg_L[data_one_min$ammonium_PT3_mg_L>85] <- NA

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L>10] <- NA

data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L< -10] <- NA


#Ammonium in process tank 4
data_one_min$ammonium_PT4_mg_L[data_one_min$ammonium_PT4_mg_L>85] <- NA

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L>10] <- NA

data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L< -10] <- NA


#Ammonium in the effluent
data_one_min$ammonium_effluent_mg_L[data_one_min$ammonium_effluent_mg_L>85] <- NA

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_ammonium_effluent_mg_L>10] <- NA

data_one_min$ammonium_effluent_mg_L[data_one_min$diff_ammonium_effluent_mg_L< -10] <- NA

#----
#Temperature
#------------------------------------
#The values above 25 and under 2 degrees are removed, and a difference of +-1

#Temperature in process tank 1
data_one_min$T_PT1_C[data_one_min$T_PT1_C>25] <- NA

data_one_min$T_PT1_C[data_one_min$T_PT1_C< 2] <- NA

data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C>1] <- NA

data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C< -1] <- NA


#Temperature in process tank 2
data_one_min$T_PT2_C[data_one_min$T_PT2_C>25] <- NA

data_one_min$T_PT2_C[data_one_min$T_PT2_C< 2] <- NA

data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C>1] <- NA

data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C< -1] <- NA


#Temperature in process tank 3
data_one_min$T_PT3_C[data_one_min$T_PT3_C>25] <- NA

data_one_min$T_PT3_C[data_one_min$T_PT3_C< 2] <- NA

data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C>1] <- NA

data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C< -1] <- NA


#Temperature in process tank 4
data_one_min$T_PT4_C[data_one_min$T_PT4_C>25] <- NA

data_one_min$T_PT4_C[data_one_min$T_PT4_C< 2] <- NA

data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C>1] <- NA

data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C< -1] <- NA


#----
#Nitrate
#------------------------------------
#Max nitrate concentration is 85 mg/L, plus minus 10 in diff

#process tank 1
data_one_min$nitrate_PT1_mg_L[data_one_min$nitrate_PT1_mg_L>85] <- NA

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_nitrate_PT1_mg_L>10] <- NA

data_one_min$nitrate_PT1_mg_L[data_one_min$diff_nitrate_PT1_mg_L< -10] <- NA


#process tank 2
data_one_min$nitrate_PT2_mg_L[data_one_min$nitrate_PT2_mg_L>85] <- NA

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_nitrate_PT2_mg_L>10] <- NA

data_one_min$nitrate_PT2_mg_L[data_one_min$diff_nitrate_PT2_mg_L< -10] <- NA


#process tank 3
data_one_min$nitrate_PT3_mg_L[data_one_min$nitrate_PT3_mg_L>85] <- NA

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_nitrate_PT3_mg_L>10] <- NA

data_one_min$nitrate_PT3_mg_L[data_one_min$diff_nitrate_PT3_mg_L< -10] <- NA


#process tank 4
data_one_min$nitrate_PT4_mg_L[data_one_min$nitrate_PT4_mg_L>85] <- NA

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_nitrate_PT4_mg_L>10] <- NA

data_one_min$nitrate_PT4_mg_L[data_one_min$diff_nitrate_PT4_mg_L< -10] <- NA


#effluent
data_one_min$nitrate_effluent_mg_L[data_one_min$nitrate_effluent_mg_L>85] <- NA

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_nitrate_effluent_mg_L>10] <- NA

data_one_min$nitrate_effluent_mg_L[data_one_min$diff_nitrate_effluent_mg_L< -10] <- NA


#----
#Aeration
#------------------------------------
#The max broader is set to be 8000 m3/h no difference cleaning

#Process tank 1
data_one_min$airflow_PT1_m3_h[data_one_min$airflow_PT1_m3_h>8000] <- NA


#Process tank 2
data_one_min$airflow_PT2_m3_h[data_one_min$airflow_PT2_m3_h>8000] <- NA


#Process tank 3
data_one_min$airflow_PT3_m3_h[data_one_min$airflow_PT3_m3_h>8000] <- NA


#Process tank 4
data_one_min$airflow_PT4_m3_h[data_one_min$airflow_PT4_m3_h>8000] <- NA


#----
#Dissolved oxygen
#------------------------------------
#The engeering tool box is stating that the fresh water solubility is 13.9 mg/L
#at 2 degrees and salinity reduces the solubility, and the difference broader is
#set to be 5 mg/L based on how the data looks

#DO in process tank 1
data_one_min$DO_PT1_mg_L[data_one_min$DO_PT1_mg_L>13.9] <- NA

data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L>5] <- NA

data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L< -5] <- NA


#DO in process tank 2
data_one_min$DO_PT2_mg_L[data_one_min$DO_PT2_mg_L>13.9] <- NA

data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L>5] <- NA

data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L< -5] <- NA


#DO in process tank 3
data_one_min$DO_PT3_mg_L[data_one_min$DO_PT3_mg_L>13.9] <- NA

data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L>5] <- NA

data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L< -5] <- NA


#DO in process tank 4
data_one_min$DO_PT4_mg_L[data_one_min$DO_PT4_mg_L>13.9] <- NA

data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L>5] <- NA

data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L< -5] <- NA


#----
#SS concentration
#------------------------------------
#The max concentration is chosen to be 10 mg/L and minimum od 0 and the 
#max diff is set to be 2 mg/L both are only based on the graphs, and a
#physical insistent

#SS in process tank 1 
data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L>10] <- NA

data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L<=0] <- NA

data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L>2] <- NA

data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L< -2] <- NA


#SS in process tank 4
data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L>10] <- NA

data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L<=0] <- NA

data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L>2] <- NA

data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L< -2] <- NA


#SS in the effluent 
data_one_min$SS_to_AN_g_L[data_one_min$SS_to_AN_g_L>10] <- NA

data_one_min$SS_to_AN_g_L[data_one_min$SS_to_AN_g_L<=0] <- NA

data_one_min$SS_to_AN_g_L[data_one_min$diff_SS_to_AN_g_L>2] <- NA

data_one_min$SS_to_AN_g_L[data_one_min$diff_SS_to_AN_g_L< -2] <- NA

