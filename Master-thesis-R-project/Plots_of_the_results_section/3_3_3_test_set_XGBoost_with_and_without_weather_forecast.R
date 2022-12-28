#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "test_set_accuracy/influent"

#Making a list of all the csv file names that should be used
list_of_filenames <- list("XGB_with_AN_tank_hyperparameters_optimasation_on_test_set.csv" ,
                                "XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set.csv")


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
  
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set.csv"){
    file='XGBoost with weather forecast'
  }
  if (file=="XGB_with_AN_tank_hyperparameters_optimasation_on_test_set.csv"){
    file='XGBoost without weather forecast'
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
    theme_malte()+
    scale_color_manual(values = c("Black",
                                  "Red"))
  
}






#accuracy_table %>% accuracy_over_forecasting_horizon(r2_training)
accuracy_table %>% accuracy_over_forecasting_horizon(r2_validation)+
  labs(x="Forecast horizon",
       y=''~R^2~'of the test set')
#accuracy_table %>% accuracy_over_forecasting_horizon(mae_training)
p1 <- accuracy_table %>% accuracy_over_forecasting_horizon(mae_validation)+
  labs(x=element_blank(),
       y='MAE of the test set',
       tag="A")
#accuracy_table %>% accuracy_over_forecasting_horizon(rmse_training)
p2 <- accuracy_table %>% accuracy_over_forecasting_horizon(rmse_validation)+
  labs(x="Forecast horizon",
       y='RMSE of the test set',
       tag="B")
#accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(median_ae_validation)
#accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_training)
accuracy_table %>% accuracy_over_forecasting_horizon(max_ae_validation)+
  labs(x="Forecast horizon",
       y='Max AE of the test set')




gridExtra::grid.arrange(p1,p2)

plot_grid(p1,
          p2,
          align = "v", 
          nrow = 2,
          ncol=1,
          rel_widths=c(1,1))
