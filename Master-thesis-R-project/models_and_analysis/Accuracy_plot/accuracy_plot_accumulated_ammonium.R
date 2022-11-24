#Plotting the accuracy

#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code"
folder_name = "accuracy_linear_models_accumulated_ammonium_load"

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


grid_r2_part1 <- function(forecastinghorizon){
  p1 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,r2_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p2 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,r2_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p3 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,r2_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  p4 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,r2_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}


grid_rmse_part1 <- function(forecastinghorizon){
  p1 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,rmse_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p2 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,rmse_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p3 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,rmse_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  p4 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,rmse_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}

grid_mae_part1 <- function(forecastinghorizon){
  p1 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,mae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p2 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,mae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p3 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,mae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  p4 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,mae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}


grid_median_ae_part1 <- function(forecastinghorizon){
  p1 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,median_ae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p2 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,median_ae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p3 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,median_ae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  p4 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,median_ae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}


grid_max_ae_part1 <- function(forecastinghorizon){
  p1 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,max_ae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p2 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon,max_ae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon)
  
  p3 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,max_ae_training)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  p4 <- accuracy_table %>% 
    all_ts_within_a_fh(forecastinghorizon+1,max_ae_validation)+
    geom_vline(xintercept = 300, 
               color="red")+
    geom_vline(xintercept = 350, 
               color="red")+
    geom_vline(xintercept = 400, 
               color="red")+
    labs(subtitle  = forecastinghorizon+1)
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}

#-------------------------------------------------------------------------------
#The plots where forecasting horizon is constants and the time steps are tested
#towards on another (Make 48 plot x12)


grid_r2_part1(1)
grid_r2_part1(3)
grid_r2_part1(5)
grid_r2_part1(7)
grid_r2_part1(9)
grid_r2_part1(11)
grid_r2_part1(13)
grid_r2_part1(15)
grid_r2_part1(17)
grid_r2_part1(19)
grid_r2_part1(21)
grid_r2_part1(23)
grid_r2_part1(25)
grid_r2_part1(27)
grid_r2_part1(29)
grid_r2_part1(31)
grid_r2_part1(33)
grid_r2_part1(35)
grid_r2_part1(37)
grid_r2_part1(39)
grid_r2_part1(41)
grid_r2_part1(43)
grid_r2_part1(45)
grid_r2_part1(47)






grid_rmse_part1(1)
grid_rmse_part1(3)
grid_rmse_part1(5)
grid_rmse_part1(7)
grid_rmse_part1(9)
grid_rmse_part1(11)
grid_rmse_part1(13)
grid_rmse_part1(15)
grid_rmse_part1(17)
grid_rmse_part1(19)
grid_rmse_part1(21)
grid_rmse_part1(23)
grid_rmse_part1(25)
grid_rmse_part1(27)
grid_rmse_part1(29)
grid_rmse_part1(31)
grid_rmse_part1(33)
grid_rmse_part1(35)
grid_rmse_part1(37)
grid_rmse_part1(39)
grid_rmse_part1(41)
grid_rmse_part1(43)
grid_rmse_part1(45)
grid_rmse_part1(47)







grid_mae_part1(1)
grid_mae_part1(3)
grid_mae_part1(5)
grid_mae_part1(7)
grid_mae_part1(9)
grid_mae_part1(11)
grid_mae_part1(13)
grid_mae_part1(15)
grid_mae_part1(17)
grid_mae_part1(19)
grid_mae_part1(21)
grid_mae_part1(23)
grid_mae_part1(25)
grid_mae_part1(27)
grid_mae_part1(29)
grid_mae_part1(31)
grid_mae_part1(33)
grid_mae_part1(35)
grid_mae_part1(37)
grid_mae_part1(39)
grid_mae_part1(41)
grid_mae_part1(43)
grid_mae_part1(45)
grid_mae_part1(47)








grid_median_ae_part1(1)
grid_median_ae_part1(3)
grid_median_ae_part1(5)
grid_median_ae_part1(7)
grid_median_ae_part1(9)
grid_median_ae_part1(11)
grid_median_ae_part1(13)
grid_median_ae_part1(15)
grid_median_ae_part1(17)
grid_median_ae_part1(19)
grid_median_ae_part1(21)
grid_median_ae_part1(23)
grid_median_ae_part1(25)
grid_median_ae_part1(27)
grid_median_ae_part1(29)
grid_median_ae_part1(31)
grid_median_ae_part1(33)
grid_median_ae_part1(35)
grid_median_ae_part1(37)
grid_median_ae_part1(39)
grid_median_ae_part1(41)
grid_median_ae_part1(43)
grid_median_ae_part1(45)
grid_median_ae_part1(47)






grid_max_ae_part1(1)
grid_max_ae_part1(3)
grid_max_ae_part1(5)
grid_max_ae_part1(7)
grid_max_ae_part1(9)
grid_max_ae_part1(11)
grid_max_ae_part1(13)
grid_max_ae_part1(15)
grid_max_ae_part1(17)
grid_max_ae_part1(19)
grid_max_ae_part1(21)
grid_max_ae_part1(23)
grid_max_ae_part1(25)
grid_max_ae_part1(27)
grid_max_ae_part1(29)
grid_max_ae_part1(31)
grid_max_ae_part1(33)
grid_max_ae_part1(35)
grid_max_ae_part1(37)
grid_max_ae_part1(39)
grid_max_ae_part1(41)
grid_max_ae_part1(43)
grid_max_ae_part1(45)
grid_max_ae_part1(47)




