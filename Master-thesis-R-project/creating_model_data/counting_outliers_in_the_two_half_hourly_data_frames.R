#///////////////////////////////////////////////////////////////////////////////
#Finding NA values in the model_data data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to investigate how many NA values are created 
#with the data cleaning of the one minute data frame

#Setup
#-------------------------------------------------------------------------------
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project")

#read the data from a csv file 
model_data <- read_csv("data/model_data.csv") %>% 
  #convert the date time to a right time format
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

#Function
#-------------------------------------------------------------------------------
#Defining the function that will be used 

data_loss <- function(column_name){
  MV_before <- data_thirty_min %>% 
    select({{column_name}}) %>% 
    filter(is.na({{column_name}})) %>% 
    nrow()
  
  AV_before <- data_thirty_min %>% 
    select({{column_name}}) %>%
    nrow()
  
  
  MV_after <- model_data %>% 
    select({{column_name}}) %>% 
    filter(is.na({{column_name}})) %>% 
    nrow()
  
  AV_after <- model_data %>% 
    select({{column_name}}) %>%
    nrow()
  
  percentage_before <- (MV_before*100)/AV_before
  percentage_after <- (MV_after*100)/AV_after  
  print("before:")
  print(percentage_before)
  print("after:")
  print(percentage_after)
  
}



#Find the numbers of NA in the columns in the uncleaned data
data_loss(ammonium_to_AN_mg_L)

data_loss(ammonium_PT1_mg_L)

data_loss(ammonium_PT2_mg_L)

data_loss(ammonium_PT3_mg_L)

data_loss(ammonium_PT4_mg_L)

data_loss(ammonium_effluent_mg_L)



data_loss(nitrate_PT1_mg_L)

data_loss(nitrate_PT2_mg_L)

data_loss(nitrate_PT3_mg_L)

data_loss(nitrate_PT4_mg_L)

data_loss(nitrate_effluent_mg_L)


data_loss(T_PT1_C)

data_loss(T_PT2_C)

data_loss(T_PT3_C)

data_loss(T_PT4_C)


data_loss(DO_PT1_mg_L)

data_loss(DO_PT2_mg_L)

data_loss(DO_PT3_mg_L)

data_loss(DO_PT4_mg_L)


data_loss(SS_PT1_g_L)

data_loss(SS_PT4_g_L)

data_loss(SS_to_AN_g_L)



data_loss(airflow_PT1_m3_h)

data_loss(airflow_PT2_m3_h)

data_loss(airflow_PT3_m3_h)

data_loss(airflow_PT4_m3_h)




