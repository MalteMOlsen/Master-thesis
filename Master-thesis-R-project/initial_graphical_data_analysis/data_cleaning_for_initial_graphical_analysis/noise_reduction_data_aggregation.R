#///////////////////////////////////////////////////////////////////////////////
#Finding the times where the process tanks were insufficient
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to make some graphs to display the noise reduction

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

#Setup
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")


#-------------------------------------------------------------------------------
#Making the plotting function
noise_plot <- function(df, time_interval){
  p1 <- df %>% 
    filter_index(time_interval) %>% 
    select((flow_AN_m3_h)) %>% 
    autoplot()+
    theme_malte()+
    ylab("Flow to the AN tank [m3/h]")+
    xlab("Time")
  
  p2 <- df %>% 
    filter_index(time_interval) %>% 
    select((airflow_PT4_m3_h)) %>% 
    autoplot()+
    theme_malte()+
    ylab("Airflow in the process tank 4 [Nm3/h]")+
    xlab("Time")
  
  p3 <- df %>% 
    filter_index(time_interval) %>% 
    select((ammonium_PT2_mg_L)) %>% 
    autoplot()+
    theme_malte()+
    ylab("Ammonium concentration in process tank 2 [m3/h]")+
    xlab("Time")
  
  gridExtra::grid.arrange(p1,p2,p3)
}


#-------------------------------------------------------------------------------
#Making the plots

#Months
#------
month <- "2021-12"

data_one_min %>% 
  noise_plot(month)

data_five_min %>% 
  noise_plot(month)

data_fifteen_min %>% 
  noise_plot(month)

data_thirty_min %>% 
  noise_plot(month)

data_hour %>% 
  noise_plot(month)

#Week
#------
week <- "2020-03-02"~"2020-03-08"

data_one_min %>% 
  noise_plot(week)

data_five_min %>% 
  noise_plot(week)

data_fifteen_min %>% 
  noise_plot(week)

data_thirty_min %>% 
  noise_plot(week)

data_hour %>% 
  noise_plot(week)

#Day
#------

day <- "2019-08-02"

data_one_min %>% 
  noise_plot(day)

data_five_min %>% 
  noise_plot(day)

data_fifteen_min %>% 
  noise_plot(day)

data_thirty_min %>% 
  noise_plot(day)

data_hour %>% 
  noise_plot(day)


