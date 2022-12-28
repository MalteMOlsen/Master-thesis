#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3) 
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "SFS_model_vs_full_linear_model_vs_seasonal_model"

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
  
  if (file=="AR_375_ammonium_and_AR_350_rain_and_seasonality_terms_linear_model_accuracy_all_forecasting_horizon.csv"){
    file='Seasonal linear model'
  }
  if (file=="SFS_optimal_model_performance.csv"){
    file='SFS linear model'
  }  
  if (file=="AR_375_ammonium_and_AR_350_rain_linear_model_accuracy_all_forecasting_horizon.csv"){
    file='Linear model'
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
      plot.tag.position = c(1, 1),
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
    theme_malte()+
    scale_color_manual(values = c("Black",
                                  "Red",
                                  "Blue"))
  
}


#accuracy_table %>% accuracy_over_forecasting_horizon(r2_training)
accuracy_table %>% accuracy_over_forecasting_horizon(r2_validation)+
  labs(x="Forecast horizon",
       y=''~R^2~'of the validation set')
#accuracy_table %>% accuracy_over_forecasting_horizon(mae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(mae_validation)+
  labs(x="Forecast horizon",
       y='MAE of the validation set')
#accuracy_table %>% accuracy_over_forecasting_horizon(rmse_training)
accuracy_table %>% accuracy_over_forecasting_horizon(rmse_validation)+
  labs(x="Forecast horizon",
       y='RMSE of the validation set')
#accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_validation)+
  labs(x="Forecast horizon",
       y='Median AE of the validation set')
#accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_validation)+
  labs(x="Forecast horizon",
       y='Max AE of the validation set')

