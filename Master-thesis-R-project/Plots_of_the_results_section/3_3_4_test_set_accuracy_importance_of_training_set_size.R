#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "test_set_accuracy/influent_without_weather_forecast"

#Making a list of all the csv file names that should be used
list_of_filenames <- list.files(paste(parrent_folder,
                                      folder_name,
                                      sep = "/"))

#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
#Check length need to be 288
#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!


wd <- paste(parrent_folder,folder_name,sep="/")

drops1 <- c("...1")
accuracy_table <- data.frame()


#set working directory
for (file in list_of_filenames){
  df <- read_delim(paste(wd,file,sep = "/"))
  
  df <- df[ , !(names(df) %in% drops1)]

  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set.csv"){
    file='XGBoost - 4.25 years of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set_half_years_of_training_data.csv"){
    file='XGBoost - 9 months of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set_one_years_of_training_data.csv"){
    file='XGBoost - 1.25 year of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set_three_months_of_training_data.csv"){
    file='XGBoost - 6 months of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set_two_years_of_training_data.csv"){
    file='XGBoost - 2.25 years of training data'
  }
  
  
  df <- df %>% 
    mutate(modelling_method=file)
  
  accuracy_table <- bind_rows(df,
                              accuracy_table)
}


#-------------------------------------------------------------------------------
#Functions for the plots


#Defining the theme
theme_malte <- function(){
  theme_light(base_size = 12)+
    theme(
      axis.line = element_line(),
      panel.border = element_blank(),
      axis.title=element_text(size=12),
      axis.text=element_text(size=11),
      plot.tag = element_text(size = 14),
      plot.tag.position = c(0.75, 1),
      plot.subtitle=element_text(size=12))}


accuracy_over_forecasting_horizon <- function(df,
                                              eval_parameter){
  df %>% 
    ggplot(                    
      aes(x = forecasting_horizon,
          y = {{eval_parameter}},
          group = modelling_method,
          col = modelling_method)) +
    geom_line()+
    theme_malte()
  
}



accuracy_table %>% 
  ggplot() +
  geom_line(                   
    aes(x = forecasting_horizon,
        y = mae_validation,
        group = modelling_method,
        col = modelling_method))+
  geom_line(                   
    aes(x = forecasting_horizon,
        y = rmse_validation,
        group = modelling_method,
        col = modelling_method))+
  theme_malte()+
  labs(x="Forecast horizon",
       y='MAE/RMSE of the validation set')









#accuracy_table %>% accuracy_over_forecasting_horizon(r2_training)
accuracy_table %>% accuracy_over_forecasting_horizon(r2_validation)+
  labs(x="Forecast horizon",
       y=''~R^2~'of the test set')
#accuracy_table %>% accuracy_over_forecasting_horizon(mae_training)
p_accuracy_without_weather_forecast <- accuracy_table %>% accuracy_over_forecasting_horizon(mae_validation)+
  labs(x=element_blank(),
       y='MAE of the test set',
       title="XGBoost without weather forecast")
#accuracy_table %>% accuracy_over_forecasting_horizon(rmse_training)
accuracy_table %>% accuracy_over_forecasting_horizon(rmse_validation)+
  labs(x="Forecast horizon",
       y='RMSE of the test set')
#accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_validation)
#accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_validation)


#///////////////////////////////////////////////////////////////////////////////

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "test_set_accuracy/influent_with_weather_forecast"

#Making a list of all the csv file names that should be used
list_of_filenames <- list.files(paste(parrent_folder,
                                      folder_name,
                                      sep = "/"))

#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
#Check length need to be 288
#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!


wd <- paste(parrent_folder,folder_name,sep="/")

drops1 <- c("...1")
accuracy_table2 <- data.frame()


#set working directory
for (file in list_of_filenames){
  df <- read_delim(paste(wd,file,sep = "/"))
  
  df <- df[ , !(names(df) %in% drops1)]
  
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set.csv"){
    file='XGBoost - 4.25 years of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set_half_years_of_training_data.csv"){
    file='XGBoost - 0.75 years of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set_one_years_of_training_data.csv"){
    file='XGBoost - 1.25 year of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set_three_months_of_training_data.csv"){
    file='XGBoost - 0.5 years of training data'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set_two_years_of_training_data.csv"){
    file='XGBoost - 2.25 years of training data'
  }
  
  
  df <- df %>% 
    mutate(modelling_method=file)
  
  accuracy_table2 <- bind_rows(df,
                              accuracy_table2)
}


#accuracy_table %>% accuracy_over_forecasting_horizon(r2_training)
accuracy_table2 %>% accuracy_over_forecasting_horizon(r2_validation)+
  labs(x="Forecast horizon",
       y=''~R^2~'of the test set')
#accuracy_table %>% accuracy_over_forecasting_horizon(mae_training)
p_accuracy_with_weather_forecast <- accuracy_table2 %>% accuracy_over_forecasting_horizon(mae_validation)+
  labs(x="Forecast horizon",
       y='MAE of the test set',
       colour = "Training set size")+
  scale_color_manual(values = c(
                                "darkorange",
                                "darkblue", 
                                "cyan1",
                                "magenta1",
                                "Black"))
       #title="XGBoost with weather forecast")
#accuracy_table %>% accuracy_over_forecasting_horizon(rmse_training)
accuracy_table2 %>% accuracy_over_forecasting_horizon(rmse_validation)+
  labs(x="Forecast horizon",
       y='RMSE of the test set')
#accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_training)
accuracy_table2 %>% accuracy_over_forecasting_horizon(median_ae_validation)
#accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_training)
accuracy_table2 %>% accuracy_over_forecasting_horizon(max_ae_validation)


gridExtra::grid.arrange(p_accuracy_without_weather_forecast,
                        p_accuracy_with_weather_forecast)

