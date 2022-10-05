#///////////////////////////////////////////////////////////////////////////////
#Finding outliers in the half hourly data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find if there outliers left in the half hourly
#data frame, after the cleaning of the model data

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

#-------------------------------------------------------------------------------
#Function

outliers_graphs <- function(column_name){
  p1<- model_data %>%
    filter_index("2018-01-01"~"2019-06-01") %>% 
    select({{column_name}}) %>% 
    autoplot()+
    theme_malte()+
    geom_hline(yintercept = 0.1,color="Red")+
    geom_hline(yintercept = -0.1,color="Red")
  
  p2<- model_data %>%
    filter_index("2019-06-01"~"2021-01-01") %>% 
    select({{column_name}}) %>% 
    autoplot()+
    theme_malte()+
    geom_hline(yintercept = 0.1,color="Red")+
    geom_hline(yintercept = -0.1,color="Red")
  
  p3<- model_data %>%
    filter_index("2021-01-01"~.) %>% 
    select({{column_name}}) %>% 
    autoplot()+
    theme_malte()+
    geom_hline(yintercept = 0.1,color="Red")+
    geom_hline(yintercept = -0.1,color="Red")
  
  gridExtra::grid.arrange(p1,p2,p3)
    
}

#Ammonium and nitrate concentration - difference
outliers_graphs(diff_ammonium_to_AN_mg_L)
outliers_graphs(diff_ammonium_PT1_mg_L)
outliers_graphs(diff_ammonium_PT2_mg_L)
outliers_graphs(diff_ammonium_PT3_mg_L)
outliers_graphs(diff_ammonium_PT4_mg_L)
outliers_graphs(diff_ammonium_effluent_mg_L)
outliers_graphs(diff_nitrate_effluent_mg_L)
outliers_graphs(diff_nitrate_PT1_mg_L)
outliers_graphs(diff_nitrate_PT2_mg_L)
outliers_graphs(diff_nitrate_PT3_mg_L)
outliers_graphs(diff_nitrate_PT4_mg_L)

#Temperature - difference
outliers_graphs(diff_T_PT1_C)
outliers_graphs(diff_T_PT2_C)
outliers_graphs(diff_T_PT3_C)
outliers_graphs(diff_T_PT4_C)

#Airflow - difference (no need for removal)
outliers_graphs(diff_airflow_PT1_m3_h)
outliers_graphs(diff_airflow_PT2_m3_h)
outliers_graphs(diff_airflow_PT3_m3_h)
outliers_graphs(diff_airflow_PT4_m3_h)

#DO concentration - difference
outliers_graphs(diff_DO_PT1_mg_L)
outliers_graphs(diff_DO_PT2_mg_L)
outliers_graphs(diff_DO_PT3_mg_L)
outliers_graphs(diff_DO_PT4_mg_L)

#SS concentration - difference
outliers_graphs(diff_SS_to_AN_g_L)
outliers_graphs(diff_SS_PT1_g_L)
outliers_graphs(diff_SS_PT4_g_L)

