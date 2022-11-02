#///////////////////////////////////////////////////////////////////////////////
#Making data frames that should go into Python
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is 

#For ammonium load forecasting

#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

model_data <- model_data %>% 
  mutate(rain_elleven_hour_accumulated = rollapplyr(rainfall_mm, 
                                                    width=22, 
                                                    FUN=sum, 
                                                    partial=T)) %>%
  mutate(DIxR1H=rain_one_hour_accumulated*drought) %>% 
  mutate(DIxR1hH=rain_one_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR2H=rain_two_hour_accumulated*drought) %>%
  mutate(DIxR2hH=rain_two_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR3H=rain_three_hour_accumulated*drought) %>%
  mutate(DIxR3hH=rain_three_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR4H=rain_four_day_accumulated*drought) %>%
  mutate(DIxR4hH=rain_four_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR5H=rain_five_day_accumulated*drought) %>%
  mutate(DIxR5hH=rain_five_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR6H=rain_six_hour_accumulated*drought) %>%
  mutate(DIxR7H=rain_seven_hour_accumulated*drought) %>%
  mutate(DIxR8H=rain_eight_hour_accumulated*drought) %>%
  mutate(DIxR9H=rain_nine_hour_accumulated*drought) %>%
  mutate(DIxR10H=rain_ten_hour_accumulated*drought) %>%
  mutate(DIxR11H=rain_elleven_hour_accumulated*drought) %>%
  mutate(DIxR12H=rain_twelve_hour_accumulated*drought) %>%
  mutate(DIxR13H=rain_thriteen_hour_accumulated*drought) %>%
  mutate(DIxR14H=rain_fourteen_hour_accumulated*drought) %>%
  mutate(DIxR15H=rain_fifteen_hour_accumulated*drought) %>%
  mutate(DIxR16H=rain_sixteen_hour_accumulated*drought) %>%
  mutate(DIxR17H=rain_seventeen_hour_accumulated*drought) %>%
  mutate(DIxR18H=rain_eighteen_hour_accumulated*drought) %>%
  mutate(DIxR19H=rain_nineteen_hour_accumulated*drought) %>%
  mutate(DIxR20H=rain_twenty_hour_accumulated*drought) %>%
  mutate(DIxR21H=rain_twentyone_hour_accumulated*drought) %>%
  mutate(DIxR22H=rain_tweentytwo_hour_accumulated*drought) %>%
  mutate(DIxR23H=rain_twentythree_hour_accumulated*drought) %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought)

model_data <- model_data %>% 
  select(-flow_effluent_m3_h,
         -flow_HT_m3_h,
         -flow_influent_m3_h,
         -T_PT1_C,
         -T_PT2_C,
         -T_PT3_C,
         -T_PT4_C,
         -ammonium_PT1_mg_L,
         -ammonium_PT2_mg_L,
         -ammonium_PT3_mg_L,
         -ammonium_PT4_mg_L,
         -ammonium_effluent_mg_L,
         -ammonium_to_AN_mg_L,
         -SS_PT1_g_L,
         -SS_PT4_g_L,
         -SS_to_AN_g_L,
         -airflow_PT1_m3_h,
         -airflow_PT2_m3_h,
         -airflow_PT3_m3_h,
         -airflow_PT4_m3_h,
         -nitrate_PT1_mg_L,
         -nitrate_PT2_mg_L,
         -nitrate_PT3_mg_L,
         -nitrate_PT4_mg_L,
         -nitrate_effluent_mg_L,
         -DO_PT1_mg_L,
         -DO_PT2_mg_L,
         -DO_PT3_mg_L,
         -DO_PT4_mg_L,
         -EU_PT_kWh_h,
         -SC_operatingNumber,
         -lag1_flow_influent_m3_h,
         -lag1_flow_effluent_m3_h,                     
         -lag1_flow_HT_m3_h,                           
         -lag1_ammonium_to_AN_mg_L,                    
         -lag1_T_PT1_C,                                
         -lag1_T_PT2_C,                                
         -lag1_T_PT3_C,                                
         -lag1_T_PT4_C,                                
         -lag1_ammonium_PT1_mg_L,                      
         -lag1_ammonium_PT2_mg_L,                      
         -lag1_ammonium_PT3_mg_L,                      
         -lag1_ammonium_PT4_mg_L,                      
         -lag1_SS_PT1_g_L,                             
         -lag1_SS_PT4_g_L,                             
         -lag1_SS_to_AN_g_L,                           
         -lag1_airflow_PT1_m3_h,                       
         -lag1_airflow_PT2_m3_h,                       
         -lag1_airflow_PT3_m3_h,                       
         -lag1_airflow_PT4_m3_h,                       
         -lag1_nitrate_PT1_mg_L,                       
         -lag1_nitrate_PT2_mg_L,                       
         -lag1_nitrate_PT3_mg_L,                       
         -lag1_nitrate_PT4_mg_L,                       
         -lag1_DO_PT1_mg_L,                            
         -lag1_DO_PT2_mg_L,                            
         -lag1_DO_PT3_mg_L,                            
         -lag1_DO_PT4_mg_L,                            
         -lag1_ammonium_effluent_mg_L,                 
         -lag1_nitrate_effluent_mg_L,                           
         -diff_flow_influent_m3_h,                     
         -diff_flow_effluent_m3_h,                     
         -diff_flow_HT_m3_h,                    
         -diff_T_PT1_C,                                
         -diff_T_PT2_C,                                
         -diff_T_PT3_C,                                
         -diff_T_PT4_C,                                
         -diff_ammonium_PT1_mg_L,                      
         -diff_ammonium_PT2_mg_L,                      
         -diff_ammonium_PT3_mg_L,                      
         -diff_ammonium_PT4_mg_L,                      
         -diff_SS_PT1_g_L,                             
         -diff_SS_PT4_g_L,                             
         -diff_SS_to_AN_g_L,                        
         -diff_airflow_PT1_m3_h,                       
         -diff_airflow_PT2_m3_h,                       
         -diff_airflow_PT3_m3_h,                       
         -diff_airflow_PT4_m3_h,                       
         -diff_nitrate_PT1_mg_L,                       
         -diff_nitrate_PT2_mg_L,                       
         -diff_nitrate_PT3_mg_L,                       
         -diff_nitrate_PT4_mg_L,                       
         -diff_DO_PT1_mg_L,                            
         -diff_DO_PT2_mg_L,                            
         -diff_DO_PT3_mg_L,                            
         -diff_DO_PT4_mg_L,                            
         -diff_ammonium_effluent_mg_L,                 
         -diff_nitrate_effluent_mg_L,                           
         -diff_EU_PT_kWh_h,                            
         -diff_SC_operatingNumber, 
         -total_N_effluent_mg_L,                       
         -ammonium_load_PT4_kg_h,                      
         -ammonium_load_PT3_kg_h,                      
         -ammonium_load_PT2_kg_h,                      
         -ammonium_load_PT1_kg_h,                      
         -nitrate_load_PT4_kg_h,                       
         -nitrate_load_PT3_kg_h,                       
         -nitrate_load_PT2_kg_h,                       
         -nitrate_load_PT1_kg_h,                       
         -total_N_load_PT4_kg_h,                       
         -total_N_load_PT3_kg_h,                       
         -total_N_load_PT2_kg_h,                       
         -total_N_load_PT1_kg_h,                       
         -ammonium_load_effluent_kg_h,                 
         -nitrate_load_effluent_kg_h,                  
         -total_N_load_effluent_kg_h,  
         -PT4_ammonium_removal_kg_h,                   
         -PT3_ammonium_removal_kg_h,                   
         -PT2_ammonium_removal_kg_h,                   
         -PT1_ammonium_removal_kg_h,                   
         -sand_filter_ammonium_removal_kg_h,           
         -percentage_PT4_ammonium_removal_kg_h,
         -percentage_PT3_ammonium_removal_kg_h,
         -percentage_PT2_ammonium_removal_kg_h,
         -percentage_PT1_ammonium_removal_kg_h,
         -percentage_sand_filter_ammonium_removal_kg_h)


model_data <- model_data %>%
  mutate(timestamp = as.POSIXct(time_thirty_min)) %>% 
  # complete sequence to full sequence from min to max by second
  complete(timestamp = seq.POSIXt(min(timestamp), max(timestamp), by = '30 min')) 


temp <- model_data %>% filter(is.na(time_thirty_min))



model_data <- model_data %>% 
  mutate(load_ammonium_AN_lag1=lag(ammonium_load_AN_kg_h,1))%>% 
  mutate(load_ammonium_AN_lag2=lag(ammonium_load_AN_kg_h,2))%>% 
  mutate(load_ammonium_AN_lag3=lag(ammonium_load_AN_kg_h,3))%>% 
  mutate(load_ammonium_AN_lag4=lag(ammonium_load_AN_kg_h,4))%>% 
  mutate(load_ammonium_AN_lag5=lag(ammonium_load_AN_kg_h,5))%>% 
  mutate(load_ammonium_AN_lag6=lag(ammonium_load_AN_kg_h,6))%>% 
  mutate(load_ammonium_AN_lag7=lag(ammonium_load_AN_kg_h,7))%>% 
  mutate(load_ammonium_AN_lag8=lag(ammonium_load_AN_kg_h,8))%>% 
  mutate(load_ammonium_AN_lag9=lag(ammonium_load_AN_kg_h,9))%>% 
  mutate(load_ammonium_AN_lag10=lag(ammonium_load_AN_kg_h,10))%>% 
  mutate(load_ammonium_AN_lag11=lag(ammonium_load_AN_kg_h,11))%>% 
  mutate(load_ammonium_AN_lag12=lag(ammonium_load_AN_kg_h,12))%>% 
  mutate(load_ammonium_AN_lag13=lag(ammonium_load_AN_kg_h,13))%>% 
  mutate(load_ammonium_AN_lag14=lag(ammonium_load_AN_kg_h,14))%>% 
  mutate(load_ammonium_AN_lag15=lag(ammonium_load_AN_kg_h,15))%>% 
  mutate(load_ammonium_AN_lag16=lag(ammonium_load_AN_kg_h,16))%>% 
  mutate(load_ammonium_AN_lag17=lag(ammonium_load_AN_kg_h,17))%>% 
  mutate(load_ammonium_AN_lag18=lag(ammonium_load_AN_kg_h,18))%>% 
  mutate(load_ammonium_AN_lag19=lag(ammonium_load_AN_kg_h,19))%>% 
  mutate(load_ammonium_AN_lag20=lag(ammonium_load_AN_kg_h,20))%>% 
  mutate(load_ammonium_AN_lag21=lag(ammonium_load_AN_kg_h,21))%>% 
  mutate(load_ammonium_AN_lag22=lag(ammonium_load_AN_kg_h,22))%>% 
  mutate(load_ammonium_AN_lag23=lag(ammonium_load_AN_kg_h,23))%>% 
  mutate(load_ammonium_AN_lag24=lag(ammonium_load_AN_kg_h,24))%>%
  mutate(load_ammonium_AN_lag25=lag(ammonium_load_AN_kg_h,25))%>% 
  mutate(load_ammonium_AN_lag26=lag(ammonium_load_AN_kg_h,26))%>% 
  mutate(load_ammonium_AN_lag27=lag(ammonium_load_AN_kg_h,27))%>% 
  mutate(load_ammonium_AN_lag28=lag(ammonium_load_AN_kg_h,28))%>% 
  mutate(load_ammonium_AN_lag29=lag(ammonium_load_AN_kg_h,29))%>% 
  mutate(load_ammonium_AN_lag30=lag(ammonium_load_AN_kg_h,30))%>% 
  mutate(load_ammonium_AN_lag31=lag(ammonium_load_AN_kg_h,31))%>% 
  mutate(load_ammonium_AN_lag32=lag(ammonium_load_AN_kg_h,32))%>% 
  mutate(load_ammonium_AN_lag33=lag(ammonium_load_AN_kg_h,33))%>% 
  mutate(load_ammonium_AN_lag34=lag(ammonium_load_AN_kg_h,34))%>% 
  mutate(load_ammonium_AN_lag35=lag(ammonium_load_AN_kg_h,35))%>% 
  mutate(load_ammonium_AN_lag36=lag(ammonium_load_AN_kg_h,36))%>% 
  mutate(load_ammonium_AN_lag37=lag(ammonium_load_AN_kg_h,37))%>% 
  mutate(load_ammonium_AN_lag38=lag(ammonium_load_AN_kg_h,38))%>% 
  mutate(load_ammonium_AN_lag39=lag(ammonium_load_AN_kg_h,39))%>% 
  mutate(load_ammonium_AN_lag40=lag(ammonium_load_AN_kg_h,40))%>% 
  mutate(load_ammonium_AN_lag41=lag(ammonium_load_AN_kg_h,41))%>% 
  mutate(load_ammonium_AN_lag42=lag(ammonium_load_AN_kg_h,42))%>% 
  mutate(load_ammonium_AN_lag43=lag(ammonium_load_AN_kg_h,43))%>% 
  mutate(load_ammonium_AN_lag44=lag(ammonium_load_AN_kg_h,44))%>% 
  mutate(load_ammonium_AN_lag45=lag(ammonium_load_AN_kg_h,45))%>% 
  mutate(load_ammonium_AN_lag46=lag(ammonium_load_AN_kg_h,46))%>% 
  mutate(load_ammonium_AN_lag47=lag(ammonium_load_AN_kg_h,47))%>% 
  mutate(load_ammonium_AN_lag48=lag(ammonium_load_AN_kg_h,48))%>%
  mutate(flow_AN_lag1=lag(flow_AN_m3_h,1))%>% 
  mutate(flow_AN_lag2=lag(flow_AN_m3_h,2))%>% 
  mutate(flow_AN_lag3=lag(flow_AN_m3_h,3))%>% 
  mutate(flow_AN_lag4=lag(flow_AN_m3_h,4))%>% 
  mutate(flow_AN_lag5=lag(flow_AN_m3_h,5))%>% 
  mutate(flow_AN_lag6=lag(flow_AN_m3_h,6))%>% 
  mutate(flow_AN_lag7=lag(flow_AN_m3_h,7))%>% 
  mutate(flow_AN_lag8=lag(flow_AN_m3_h,8))%>% 
  mutate(flow_AN_lag9=lag(flow_AN_m3_h,9))%>% 
  mutate(flow_AN_lag10=lag(flow_AN_m3_h,10))%>% 
  mutate(flow_AN_lag11=lag(flow_AN_m3_h,11))%>% 
  mutate(flow_AN_lag12=lag(flow_AN_m3_h,12))%>% 
  mutate(flow_AN_lag13=lag(flow_AN_m3_h,13))%>% 
  mutate(flow_AN_lag14=lag(flow_AN_m3_h,14))%>% 
  mutate(flow_AN_lag15=lag(flow_AN_m3_h,15))%>% 
  mutate(flow_AN_lag16=lag(flow_AN_m3_h,16))%>% 
  mutate(flow_AN_lag17=lag(flow_AN_m3_h,17))%>% 
  mutate(flow_AN_lag18=lag(flow_AN_m3_h,18))%>% 
  mutate(flow_AN_lag19=lag(flow_AN_m3_h,19))%>% 
  mutate(flow_AN_lag20=lag(flow_AN_m3_h,20))%>% 
  mutate(flow_AN_lag21=lag(flow_AN_m3_h,21))%>% 
  mutate(flow_AN_lag22=lag(flow_AN_m3_h,22))%>% 
  mutate(flow_AN_lag23=lag(flow_AN_m3_h,23))%>% 
  mutate(flow_AN_lag24=lag(flow_AN_m3_h,24))%>%
  mutate(flow_AN_lag25=lag(flow_AN_m3_h,25))%>% 
  mutate(flow_AN_lag26=lag(flow_AN_m3_h,26))%>% 
  mutate(flow_AN_lag27=lag(flow_AN_m3_h,27))%>% 
  mutate(flow_AN_lag28=lag(flow_AN_m3_h,28))%>% 
  mutate(flow_AN_lag29=lag(flow_AN_m3_h,29))%>% 
  mutate(flow_AN_lag30=lag(flow_AN_m3_h,30))%>% 
  mutate(flow_AN_lag31=lag(flow_AN_m3_h,31))%>% 
  mutate(flow_AN_lag32=lag(flow_AN_m3_h,32))%>% 
  mutate(flow_AN_lag33=lag(flow_AN_m3_h,33))%>% 
  mutate(flow_AN_lag34=lag(flow_AN_m3_h,34))%>% 
  mutate(flow_AN_lag35=lag(flow_AN_m3_h,35))%>% 
  mutate(flow_AN_lag36=lag(flow_AN_m3_h,36))%>% 
  mutate(flow_AN_lag37=lag(flow_AN_m3_h,37))%>% 
  mutate(flow_AN_lag38=lag(flow_AN_m3_h,38))%>% 
  mutate(flow_AN_lag39=lag(flow_AN_m3_h,39))%>% 
  mutate(flow_AN_lag40=lag(flow_AN_m3_h,40))%>% 
  mutate(flow_AN_lag41=lag(flow_AN_m3_h,41))%>% 
  mutate(flow_AN_lag42=lag(flow_AN_m3_h,42))%>% 
  mutate(flow_AN_lag43=lag(flow_AN_m3_h,43))%>% 
  mutate(flow_AN_lag44=lag(flow_AN_m3_h,44))%>% 
  mutate(flow_AN_lag45=lag(flow_AN_m3_h,45))%>% 
  mutate(flow_AN_lag46=lag(flow_AN_m3_h,46))%>% 
  mutate(flow_AN_lag47=lag(flow_AN_m3_h,47))%>% 
  mutate(flow_AN_lag48=lag(flow_AN_m3_h,48))%>%
  #
  #The rain column
  #
  mutate(rain_lag1=lag(rainfall_mm,1))%>% 
  mutate(rain_lag2=lag(rainfall_mm,2))%>% 
  mutate(rain_lag3=lag(rainfall_mm,3))%>% 
  mutate(rain_lag4=lag(rainfall_mm,4))%>% 
  mutate(rain_lag5=lag(rainfall_mm,5))%>% 
  mutate(rain_lag6=lag(rainfall_mm,6))%>% 
  mutate(rain_lag7=lag(rainfall_mm,7))%>% 
  mutate(rain_lag8=lag(rainfall_mm,8))%>% 
  mutate(rain_lag9=lag(rainfall_mm,9))%>% 
  mutate(rain_lag10=lag(rainfall_mm,10))%>% 
  mutate(rain_lag11=lag(rainfall_mm,11))%>% 
  mutate(rain_lag12=lag(rainfall_mm,12))%>% 
  mutate(rain_lag13=lag(rainfall_mm,13))%>% 
  mutate(rain_lag14=lag(rainfall_mm,14))%>% 
  mutate(rain_lag15=lag(rainfall_mm,15))%>% 
  mutate(rain_lag16=lag(rainfall_mm,16))%>% 
  mutate(rain_lag17=lag(rainfall_mm,17))%>% 
  mutate(rain_lag18=lag(rainfall_mm,18))%>% 
  mutate(rain_lag19=lag(rainfall_mm,19))%>% 
  mutate(rain_lag20=lag(rainfall_mm,20))%>% 
  mutate(rain_lag21=lag(rainfall_mm,21))%>% 
  mutate(rain_lag22=lag(rainfall_mm,22))%>% 
  mutate(rain_lag23=lag(rainfall_mm,23))%>% 
  mutate(rain_lag24=lag(rainfall_mm,24))%>%
  mutate(rain_lag25=lag(rainfall_mm,25))%>% 
  mutate(rain_lag26=lag(rainfall_mm,26))%>% 
  mutate(rain_lag27=lag(rainfall_mm,27))%>% 
  mutate(rain_lag28=lag(rainfall_mm,28))%>% 
  mutate(rain_lag29=lag(rainfall_mm,29))%>% 
  mutate(rain_lag30=lag(rainfall_mm,30))%>% 
  mutate(rain_lag31=lag(rainfall_mm,31))%>% 
  mutate(rain_lag32=lag(rainfall_mm,32))%>% 
  mutate(rain_lag33=lag(rainfall_mm,33))%>% 
  mutate(rain_lag34=lag(rainfall_mm,34))%>% 
  mutate(rain_lag35=lag(rainfall_mm,35))%>% 
  mutate(rain_lag36=lag(rainfall_mm,36))%>% 
  mutate(rain_lag37=lag(rainfall_mm,37))%>% 
  mutate(rain_lag38=lag(rainfall_mm,38))%>% 
  mutate(rain_lag39=lag(rainfall_mm,39))%>% 
  mutate(rain_lag40=lag(rainfall_mm,40))%>% 
  mutate(rain_lag41=lag(rainfall_mm,41))%>% 
  mutate(rain_lag42=lag(rainfall_mm,42))%>% 
  mutate(rain_lag43=lag(rainfall_mm,43))%>% 
  mutate(rain_lag44=lag(rainfall_mm,44))%>% 
  mutate(rain_lag45=lag(rainfall_mm,45))%>% 
  mutate(rain_lag46=lag(rainfall_mm,46))%>% 
  mutate(rain_lag47=lag(rainfall_mm,47))%>% 
  mutate(rain_lag48=lag(rainfall_mm,48))




setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models")


full_model <- readRDS("TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors.rds")

fitted_data <- augment(full_model) %>% 
  select(.fitted)


outliers <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter(is.na(ammonium_load_AN_kg_h))

outliers_2 <- fitted_data %>% 
  filter(is.na(.fitted))



model_data <- model_data %>% 
    #joining the data with a left_join, the functions finds a best guess of a key
    #which in time column, and then add a column with linear interpolation values
    left_join(fitted_data) %>%  
    #Replace values in df_with_NA if the value are NA
    mutate(ammonium_load_AN_kg_h=if_else(is.na(ammonium_load_AN_kg_h), 
                                         .fitted, 
                                        ammonium_load_AN_kg_h))

outliers <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter(is.na(ammonium_load_AN_kg_h))

outliers_2 <- fitted_data %>% 
  filter(is.na(.fitted))




model_data %>% 
  filter(is.na(rainfall_mm))
model_data %>% 
  filter(is.na(rain_one_day_accumulated))
model_data %>% 
  filter(is.na(rain_difference))
model_data %>% 
  filter(is.na(rain_four_hour_accumulated))
model_data %>% 
  filter(is.na(lag1_rainfall_mm))


flow_plot <- function(time_interval){
  model_data %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_thirty_min))+
    geom_line(aes(y=flow_influent_m3_h))+
    geom_line(aes(y=flow_effluent_m3_h, color="RED"))
}

flow_plot("2022-01-01")


flow_plot("2020-01-01")

#

model_data <- model_data %>% 
  mutate(weekday=weekdays(time_thirty_min)) %>% 
  mutate(mondays=if_else(weekday=="mandag",
                         1,
                         0))%>% 
  mutate(tuesday=if_else(weekday=="tirsdag",
                         1,
                         0))%>% 
  mutate(wednesday=if_else(weekday=="onsdag",
                         1,
                         0))%>% 
  mutate(thursday=if_else(weekday=="torsdag",
                         1,
                         0))%>% 
  mutate(friday=if_else(weekday=="fredag",
                         1,
                         0))%>% 
  mutate(saturday=if_else(weekday=="lørdag",
                         1,
                         0))%>% 
  mutate(sunday=if_else(weekday=="søndag",
                         1,
                         0))



#Loading the csv file in to the function defined data frame
#using the read_delim to specify that the file is separated by ;
#converting all the data columns to charters, to prevent R form 
#automatically converting the columns to numbers, which gives problems 
#with commas and dots

#set working directory
wd <-"C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data/extra_data_ammonium_conc_AN_tank.csv"

df <- read_delim(paste(wd,sep = ""), 
                 delim=";",
                 col_types = cols(.default = "c"))

# Change "," to "." and converting all columns to numeric, expect in the time column
df[,-1] <- lapply(df[,-1], 
                  function(x) 
                    as.numeric(gsub(",", ".",  as.character(x))))

# Change the time column to the year-month-day hour-minute-second format
df <- df %>% 
  mutate(DATETIME=ymd_hms(DATETIME))

df <- df %>% 
  mutate(ammonium_to_AN_mg_L = ((`EGA.FT-BL600-Z_QBIO`*`EGA.AM-BL600-Z_NH4N_QBIO`)+(`EGA.AM-BL714-Z_NH4N_KALIB_EXPFILT`*`EGA.FT-BL610-Z_QR`))/(`EGA.FT-BL600-Z_QBIO`+`EGA.FT-BL610-Z_QR`))



#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors
#----------------------------------
full_model <- model_data %>% 
  fill_gaps() %>% 
  model(
    ARIMA(ammonium_load_AN_kg_h~1+ PDQ(0,0,0)+pdq(5,0,0)+
           rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 24) +
           fourier(period = "week", K = 15) +
           fourier(period = "year", K = 9)+
           drought+
           DIxR1H+
           DIxR1hH+
           DIxR2H+
           DIxR2hH+
           DIxR3H+
           DIxR3hH+
           DIxR4H+
           DIxR4hH+
           DIxR5H+
           DIxR5hH+
           DIxR6H+
           DIxR7H+
           DIxR8H+
           DIxR9H+
           DIxR10H+
           DIxR11H+
           DIxR12H+
           DIxR13H+
           DIxR14H+
           DIxR15H+
           DIxR16H+
           DIxR17H+
           DIxR18H+
           DIxR19H+
           DIxR20H+
           DIxR21H+
           DIxR22H+
           DIxR23H+
           DIxR1)
  )
















overwrite_NA_values <- function(df_with_NA,
                                df_with_values_in_the_gap,
                                column_name){
  df <- df_with_NA %>% 
    #joining the data with a left_join, the functions finds a best guess of a key
    #which in time column, and then add a column with linear interpolation values
    left_join(df_with_values_in_the_gap) %>%  
    #Replace values in df_with_NA if the value are NA
    mutate(temp=if_else(is.na({{column_name}}), 
                        temp_column, 
                        {{column_name}})) %>% 
    #Remove the column with the interpolated data
    select(-temp_column) %>% 
    #Remove the column which still include the NA values
    select(-{{column_name}}) %>% 
    #Rename the column where the NA has been replaced to the name of the column
    rename({{column_name}}:=temp)
}



model_data <- model_data %>%
  # Only show outlying periods
  right_join(outliers %>% select(-Trips))
