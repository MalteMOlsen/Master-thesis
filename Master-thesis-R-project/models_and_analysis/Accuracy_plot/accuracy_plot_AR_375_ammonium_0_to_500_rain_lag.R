#Plotting the accuracy

#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "AR_375_ammonium_with_0_to500_lagged_rain"

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

  
  forecasting_counter <- gsub("_step_forecast.*",
                              "",
                              file) %>% 
    as.numeric()
  
  
  df <- df %>% 
    mutate(forecasting_horizon=forecasting_counter) 
  
  accuracy_table <- bind_rows(df,
                              accuracy_table)
}



#-------------------------------------------------------------------------------
#Functions for the plots


#FOR APPENDIX
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

accuracy_table %>% 
  all_fh_within_a_ts(1,
                     r2_validation)



accuracy_table %>% 
  all_fh_within_a_ts(1,
                     rmse_validation)


accuracy_table %>% 
  all_fh_within_a_ts(1,
                     mae_validation)







all_ts_within_a_fh <- function(df,
                               forecastinghorizon,
                               eval_parameter){
  df %>%  
    filter(forecasting_horizon==forecastinghorizon) %>%
    ggplot(                    
      aes(x = feature_delay,
          y = {{eval_parameter}}))+
    geom_line()+
    
    theme_malte()
  
}





p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=''~r^2~'of the validation set',
       subtitle  = "Forecastin horizon of 1")

p2 <- accuracy_table %>% 
  all_ts_within_a_fh(7,r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=element_blank(),
       subtitle  = "Forecastin horizon of 7")

p3 <- accuracy_table %>% 
  all_ts_within_a_fh(21,r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delayss",
       y=''~r^2~'of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delayss",
       y=element_blank(),
       subtitle  = "Forecastin horizon of 42")

gridExtra::grid.arrange(p1,p2,p3,p4)








#MAE

p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     mae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y='MAE of the validation set',
       subtitle  = "Forecastin horizon of 1")

p2 <- accuracy_table %>% 
  all_ts_within_a_fh(7,mae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=element_blank(),
       subtitle  = "Forecastin horizon of 7")

p3 <- accuracy_table %>% 
  all_ts_within_a_fh(21,mae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delayss",
       y='MAE of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,mae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delayss",
       y=element_blank(),
       subtitle  = "Forecastin horizon of 42")

gridExtra::grid.arrange(p1,p2,p3,p4)




#RMSE

p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     rmse_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y='RMSE of the validation set',
       subtitle  = "Forecastin horizon of 1")

p2 <- accuracy_table %>% 
  all_ts_within_a_fh(7,rmse_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=element_blank(),
       subtitle  = "Forecastin horizon of 7")

p3 <- accuracy_table %>% 
  all_ts_within_a_fh(21,rmse_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delayss",
       y='RMSE of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,rmse_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
       y=element_blank(),
       subtitle  = "Forecastin horizon of 42")

gridExtra::grid.arrange(p1,p2,p3,p4)


