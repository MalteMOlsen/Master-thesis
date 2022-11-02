#///////////////////////////////////////////////////////////////////////////////
#Plotting models and the data of the validation set
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to plot models and the data of the validation set

#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis")

source("adding_lag_columns.R")

#Defining the training and validation data
#-------
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
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  select(-flow_effluent_m3_h)

validation_data <- model_data %>% 
  mutate(diff_NH_load=difference(ammonium_load_AN_kg_h)) %>% 
  mutate(lag1_diff_NH_load_AN=lag(diff_NH_load,1))%>% 
  mutate(lag2_diff_NH_load_AN=lag(diff_NH_load,2))%>% 
  mutate(lag3_diff_NH_load_AN=lag(diff_NH_load,3))%>% 
  mutate(lag4_diff_NH_load_AN=lag(diff_NH_load,4))%>% 
  mutate(lag5_diff_NH_load_AN=lag(diff_NH_load,5))%>%  
  filter_index("2022-01-01"~"2022-03-31") %>% 
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


#specify the rain in the future
future_values <- validation_data %>% 
  fill_gaps()




setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models")


full_model <- readRDS("TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors.rds")



fc <- forecast(full_model, 
               new_data = future_values)

# Plot forecasts against actual values
p1_data <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter_index("2022-01-01"~"2022-01-10")
fc %>%
  filter_index("2022-01-01"~"2022-01-10") %>% 
  autoplot(color="red",p1_data,level=NULL)+
  theme_malte()





plot_model_fit <- function(time_interval){
  
fc <- forecast(full_model, 
               new_data = future_values)

# Plot forecasts against actual values
p1_data <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter_index(time_interval)
fc %>%
  filter_index(time_interval) %>% 
  autoplot(color="red",p1_data,level=NULL)+
  theme_malte()
}
plot_model_fit("2022-01-01"~"2022-01-10")

model_data %>% 
  select(rainfall_mm) %>% 
  filter_index("2022-01") %>% 
  autoplot()+
  theme_malte()

plot_model_fit("2022-01-11"~"2022-01-20")
plot_model_fit("2022-01-21"~"2022-01-31")

plot_model_fit("2022-02-01"~"2022-02-10")
plot_model_fit("2022-02-11"~"2022-02-20")
plot_model_fit("2022-02-21"~"2022-02-28")


plot_model_fit("2022-03-01"~"2022-03-10")
plot_model_fit("2022-03-11"~"2022-03-20")
plot_model_fit("2022-03-21"~"2022-03-31")




setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/three_months_of_training_data")


full_model_3_months <- readRDS("TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errorsthree_months.rds")


fc <- forecast(full_model_3_months, 
               new_data = future_values)

# Plot forecasts against actual values
p1_data <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter_index("2022-01-01"~"2022-01-10")
fc %>%
  filter_index("2022-01-01"~"2022-01-10") %>% 
  autoplot(color="red",p1_data,level=NULL)+
  theme_malte()
