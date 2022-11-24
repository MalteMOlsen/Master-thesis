#Plotting the accuracy

#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "accuracy_linear_models_accumulated_rain"

#Making a list of all the csv file names that should be used
list_of_filenames <- list.files(paste(parrent_folder,
                                      folder_name,
                                      sep = "/"))
 

#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
#Check length need to be 288
#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!

new_col_names <- c("feature_delay",
                   "r2_training",
                   "rmse_training",
                   "mae_training",
                   "mape_training",
                   "median_ae_training",
                   "max_ae_training",
                   "r2_validation",
                   "rmse_validation",
                   "mae_validation",
                   "mape_validation",
                   "median_ae_validation",
                   "max_ae_validation")

wd <- paste(parrent_folder,folder_name,sep="/")

drops1 <- c("...1")
drops2 <- c("mape_validation",
            "mape_training")

accuracy_table <- data.frame()


#set working directory
for (file in list_of_filenames){
  df <- read_delim(paste(wd,file,sep = "/"))
  
  df <- df[ , !(names(df) %in% drops1)]
  
  colnames(df) <- new_col_names
  
  df <- df[ , !(names(df) %in% drops2)]
  
  forecasting_counter <- gsub("_step_forecast.*",
                              "",
                              file) %>% 
    as.numeric()
  
  
  iteration_counter <- gsub(".*iteration_step_",
                            "",
                            file)
  
  iteration_counter <- gsub(".csv",
                            "",
                            iteration_counter) %>% 
    as.numeric()
  
  df <- df %>% 
    mutate(forecasting_horizon=forecasting_counter) %>% 
    mutate(time_step=iteration_counter)
  
  accuracy_table <- bind_rows(df,
                              accuracy_table)
}



#-------------------------------------------------------------------------------
#Functions for the plots


#Defining the theme
theme_malte <- function(){
  theme_light()+
    theme(
      axis.line = element_line(),
      panel.border = element_blank())
}


all_fh_within_a_ts <- function(df,
                               timestep,
                               eval_parameter){
  df %>%  
    filter(time_step==timestep) %>%
    ggplot(                    
      aes(x = feature_delay,
          y = {{eval_parameter}},
          group = forecasting_horizon,
          col = forecasting_horizon)) +
    geom_line()+
    
    theme_malte()
  
}


all_ts_within_a_fh <- function(df,
                               forecastinghorizon,
                               eval_parameter){
  df %>%  
    filter(forecasting_horizon==forecastinghorizon) %>%
    ggplot(                    
      aes(x = feature_delay,
          y = {{eval_parameter}},
          group = time_step,
          col = time_step,
          guide_colourbar(title.theme="red"))) +
    geom_line()+
    
    theme_malte()
  
}





accuracy_table %>% 
  all_fh_within_a_ts(1,r2_training)

accuracy_table %>% 
  all_fh_within_a_ts(1,mae_training)

accuracy_table %>% 
  all_fh_within_a_ts(3,mae_validation)



accuracy_table %>% 
  all_ts_within_a_fh(1,r2_training)














accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_r2_training_set))+
  geom_point(aes(y=step_2_r2_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_r2_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_r2_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_r2_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_r2_training_set), color="Purple", na.rm = T)+
  theme_malte()










  






accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_mae_training_set))+
  geom_point(aes(y=step_2_mae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_mae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_mae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_mae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_mae_training_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_r2_valid_set))+
  geom_point(aes(y=step_2_r2_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_r2_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_r2_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_r2_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_r2_valid_set), color="Purple", na.rm = T)+
  theme_malte()



accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_mae_valid_set))+
  geom_point(aes(y=step_2_mae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_mae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_mae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_mae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_mae_valid_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_rmse_training_set))+
  geom_point(aes(y=step_2_rmse_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_rmse_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_rmse_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_rmse_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_rmse_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_rmse_valid_set))+
  geom_point(aes(y=step_2_rmse_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_rmse_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_rmse_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_rmse_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_rmse_valid_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_median_ae_training_set))+
  geom_point(aes(y=step_2_median_ae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_median_ae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_median_ae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_median_ae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_median_ae_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_median_ae_valid_set))+
  geom_point(aes(y=step_2_median_ae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_median_ae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_median_ae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_median_ae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_median_ae_valid_set), color="Purple", na.rm = T)+
  theme_malte()



accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_max_ae_training_set))+
  geom_point(aes(y=step_2_max_ae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_max_ae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_max_ae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_max_ae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_max_ae_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_max_ae_valid_set))+
  geom_point(aes(y=step_2_max_ae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_max_ae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_max_ae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_max_ae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_max_ae_valid_set), color="Purple", na.rm = T)+
  theme_malte()




























# one_step_accuracy_step_1 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_1.csv")
# 
# #choosing the the working directory to be here
# setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code")
# one_step_accuracy_step_1 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_1.csv")
# one_step_accuracy_step_2 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_2.csv")
# one_step_accuracy_step_3 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_3.csv")
# one_step_accuracy_step_4 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_4.csv")
# one_step_accuracy_step_5 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_5.csv")
# one_step_accuracy_step_10 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_10.csv")
# 
# 
# 
# fourty_egith_step_accuracy_step_2 <- read_csv("48_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_2.csv")
