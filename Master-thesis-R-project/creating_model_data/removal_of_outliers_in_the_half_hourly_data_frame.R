#///////////////////////////////////////////////////////////////////////////////
#Removing outliers from the half hourly data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to remove the outliers which has been determined 
#in the graphical investigation of the half hourly data frame


#----
#Ammonium concentration
#------------------------------------
#Max ammonium concentration plus minus 1 in diff

#Ammonium in the AN tank
model_data$ammonium_to_AN_mg_L[model_data$diff_ammonium_to_AN_mg_L>1] <- NA

model_data$ammonium_to_AN_mg_L[model_data$diff_ammonium_to_AN_mg_L< -1] <- NA


#Ammonium in process tank 1
model_data$ammonium_PT1_mg_L[model_data$diff_ammonium_PT1_mg_L>1] <- NA

model_data$ammonium_PT1_mg_L[model_data$diff_ammonium_PT1_mg_L< -1] <- NA


#Ammonium in process tank 2
model_data$ammonium_PT2_mg_L[model_data$diff_ammonium_PT2_mg_L>1] <- NA

model_data$ammonium_PT2_mg_L[model_data$diff_ammonium_PT2_mg_L< -1] <- NA


#Ammonium in process tank 3
model_data$ammonium_PT3_mg_L[model_data$diff_ammonium_PT3_mg_L>1] <- NA

model_data$ammonium_PT3_mg_L[model_data$diff_ammonium_PT3_mg_L< -1] <- NA


#Ammonium in process tank 4
model_data$ammonium_PT4_mg_L[model_data$diff_ammonium_PT4_mg_L>1] <- NA

model_data$ammonium_PT4_mg_L[model_data$diff_ammonium_PT4_mg_L< -1] <- NA


#Ammonium in the effluent
model_data$ammonium_effluent_mg_L[model_data$diff_ammonium_effluent_mg_L>1] <- NA

model_data$ammonium_effluent_mg_L[model_data$diff_ammonium_effluent_mg_L< -1] <- NA


#----
#Nitrate
#------------------------------------
#Max nitrate concentration plus minus 1 in diff

#process tank 1
model_data$nitrate_PT1_mg_L[model_data$diff_nitrate_PT1_mg_L>1] <- NA

model_data$nitrate_PT1_mg_L[model_data$diff_nitrate_PT1_mg_L< -1] <- NA


#process tank 2
model_data$nitrate_PT2_mg_L[model_data$diff_nitrate_PT2_mg_L>1] <- NA

model_data$nitrate_PT2_mg_L[model_data$diff_nitrate_PT2_mg_L< -1] <- NA


#process tank 3
model_data$nitrate_PT3_mg_L[model_data$diff_nitrate_PT3_mg_L>1] <- NA

model_data$nitrate_PT3_mg_L[model_data$diff_nitrate_PT3_mg_L< -1] <- NA


#process tank 4
model_data$nitrate_PT4_mg_L[model_data$diff_nitrate_PT4_mg_L>1] <- NA

model_data$nitrate_PT4_mg_L[model_data$diff_nitrate_PT4_mg_L< -1] <- NA


#effluent
model_data$nitrate_effluent_mg_L[model_data$diff_nitrate_effluent_mg_L>1] <- NA

model_data$nitrate_effluent_mg_L[model_data$diff_nitrate_effluent_mg_L< -1] <- NA



#----
#Temperature
#------------------------------------
#The values with a difference of +-0.1

#Temperature in process tank 1
model_data$T_PT1_C[model_data$diff_T_PT1_C>0.1] <- NA

model_data$T_PT1_C[model_data$diff_T_PT1_C< -0.1] <- NA


#Temperature in process tank 2
model_data$T_PT2_C[model_data$diff_T_PT2_C>0.1] <- NA

model_data$T_PT2_C[model_data$diff_T_PT2_C< -0.1] <- NA


#Temperature in process tank 3
model_data$T_PT3_C[model_data$diff_T_PT3_C>0.1] <- NA

model_data$T_PT3_C[model_data$diff_T_PT3_C< -0.1] <- NA


#Temperature in process tank 4
model_data$T_PT4_C[model_data$diff_T_PT4_C>0.1] <- NA

model_data$T_PT4_C[model_data$diff_T_PT4_C< -0.1] <- NA


#----
#Dissolved oxygen
#------------------------------------
#The difference broader is set to be 0.2 based on how the data looks

#DO in process tank 1
model_data$DO_PT1_mg_L[model_data$diff_DO_PT1_mg_L>0.2] <- NA

model_data$DO_PT1_mg_L[model_data$diff_DO_PT1_mg_L< -0.2] <- NA


#DO in process tank 2
model_data$DO_PT2_mg_L[model_data$diff_DO_PT2_mg_L>0.2] <- NA

model_data$DO_PT2_mg_L[model_data$diff_DO_PT2_mg_L< -0.2] <- NA


#DO in process tank 3
model_data$DO_PT3_mg_L[model_data$diff_DO_PT3_mg_L>0.2] <- NA

model_data$DO_PT3_mg_L[model_data$diff_DO_PT3_mg_L< -0.2] <- NA


#DO in process tank 4
model_data$DO_PT4_mg_L[model_data$diff_DO_PT4_mg_L>0.2] <- NA

model_data$DO_PT4_mg_L[model_data$diff_DO_PT4_mg_L< -0.2] <- NA

