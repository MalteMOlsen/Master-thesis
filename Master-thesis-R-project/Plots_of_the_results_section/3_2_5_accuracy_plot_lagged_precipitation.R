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
drops2 <- c("mape_validation",
            "mape_training")

accuracy_table <- data.frame()


#set working directory
for (file in list_of_filenames){
  df <- read_delim(paste(wd,file,sep = "/"))
  
  
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
  theme_light(base_size = 12)+
    theme(
      axis.line = element_line(),
      panel.border = element_blank(),
      axis.title=element_text(size=12),
      axis.text=element_text(size=11),
      plot.tag = element_text(size = 14),
      plot.tag.position = c(1, 1),
      plot.subtitle=element_text(size=12))}

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
          y = {{eval_parameter}},
          group = time_step,
          col = time_step))+
    scale_colour_gradient(name="Timestep",
                          low = "#2a9d8f", 
                          high = "#e76f51")+
    geom_line()+
    
    theme_malte()
  
}





p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=''~R^2~'of the validation set',
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
  labs(x="# of delays",
       y=''~R^2~'of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,r2_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
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
  labs(x="# of delays",
       y='MAE of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,mae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
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
  labs(x="# of delays",
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





#Median AE-validation set

p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     median_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y='Median AE-validation set',
       subtitle  = "Forecastin horizon of 1")

p2 <- accuracy_table %>% 
  all_ts_within_a_fh(7,median_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=element_blank(),
       subtitle  = "Forecastin horizon of 7")

p3 <- accuracy_table %>% 
  all_ts_within_a_fh(21,median_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
       y='Median AE-validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,median_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
       y=element_blank(),
       subtitle  = "Forecastin horizon of 42")

gridExtra::grid.arrange(p1,p2,p3,p4)


p1 <- accuracy_table %>% 
  all_ts_within_a_fh(1,
                     max_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y='Max AE of the validation set',
       subtitle  = "Forecastin horizon of 1")

p2 <- accuracy_table %>% 
  all_ts_within_a_fh(7,max_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x=element_blank(),
       y=element_blank(),
       subtitle  = "Forecastin horizon of 7")

p3 <- accuracy_table %>% 
  all_ts_within_a_fh(21,max_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
       y='Max AE of the validation set',
       subtitle  = "Forecastin horizon of 21")

p4 <- accuracy_table %>% 
  all_ts_within_a_fh(42,max_ae_validation)+
  geom_vline(xintercept = 350, 
             color="Black")+
  labs(x="# of delays",
       y=element_blank(),
       subtitle  = "Forecastin horizon of 42")

gridExtra::grid.arrange(p1,p2,p3,p4)

