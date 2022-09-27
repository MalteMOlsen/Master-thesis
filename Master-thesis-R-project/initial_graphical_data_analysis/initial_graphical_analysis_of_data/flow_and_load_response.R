#///////////////////////////////////////////////////////////////////////////////
#Finding the times where the process tanks were insufficient
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find time where the process tanks were insufficient
#and to generate a intuitive understanding of the data

#The way the different functions works are described in the "functions_used_for_initial_graphical_analysis.R"


#Setup
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

#Making the load columns
data_thirty_min <- data_thirty_min %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h/1000)

#Making the load columns
data_one_min <- data_one_min %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h/1000)


#-------------------------------------------------------------------------------
#Making the plotting function

plot_flow_and_load_response_time <- function(df,
                                             time_period){
  p1 <- df %>%
    filter_index(time_period) %>% 
    select(rainfall_mm) %>% 
    autoplot()+
    theme_malte()+
    xlab("Time")+
    ylab("Rainfall [mm]")
  
   
  p2 <- df %>%
    filter_index(time_period) %>% 
    select(flow_AN_m3_h) %>% 
    autoplot()+
    theme_malte()+
    xlab("Time")+
    ylab(paste("Flow to the AN tank [m3/h]"))
  
  
  p3 <- df %>%
    filter_index(time_period) %>% 
    select(ammonium_load_AN_kg_h) %>% 
    autoplot()+
    theme_malte()+
    xlab("Time")+
    ylab("Ammonium load in the AN tank [kg/d]")
  
  
  gridExtra::grid.arrange(p1,p2,p3)
    
}


#-------------------------------------------------------------------------------
#Overall rain fall in one year

#2022
data_thirty_min %>%
  filter_index("2022") %>% 
  select(rainfall_mm) %>% 
  autoplot()+
  theme_malte()

#2021
data_thirty_min %>%
  filter_index("2021") %>% 
  select(rainfall_mm) %>% 
  autoplot()+
  theme_malte()

#2020

#2019

#2018


#-------------------------------------------------------------------------------

#2022
data_one_min %>%
  plot_flow_and_load_response_time("2022-04-04 05:00"~"2022-04-04 14:00")

#2021
data_one_min %>%
  plot_flow_and_load_response_time("2021-02-16 14:00:00"~"2021-02-16 22:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2021-04-05 00:30:00"~"2021-04-05 04:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2021-05-13 16:30:00"~"2021-05-13 19:00:00")


data_one_min %>%
  plot_flow_and_load_response_time("2021-08-17 01:00:00"~"2021-08-17 05:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2021-10-26 04:00:00"~"2021-10-26 06:00:00")

#2020
data_one_min %>%
  plot_flow_and_load_response_time("2020-01-09 09:30:00"~"2020-01-09 15:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2020-03-10 16:30:00"~"2020-03-10 22:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2020-07-14 05:30:00"~"2020-07-14 11:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2020-09-24 06:30:00"~"2020-09-24 12:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2020-12-10 03:30:00"~"2020-12-10 09:00:00")


#2019
data_one_min %>%
  plot_flow_and_load_response_time("2019-02-20 20:30:00"~"2019-02-21 02:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2019-05-17 04:30:00"~"2019-05-17 09:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2019-06-21 12:30:00"~"2019-06-21 16:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2019-08-27 16:30:00"~"2019-08-27 22:00:00")

data_one_min %>%
  plot_flow_and_load_response_time("2019-11-08 02:30:00"~"2019-11-08 08:30:00")

#2018
data_one_min %>%
  plot_flow_and_load_response_time("2018-02-19 06:30:00"~"2018-02-19 12:30:00")

data_one_min %>%
  plot_flow_and_load_response_time("2018-05-29 08:30:00"~"2018-05-29 13:30:00")

data_one_min %>%
  plot_flow_and_load_response_time("2018-07-28 19:30:00"~"2018-07-28 23:30:00")

data_one_min %>%
  plot_flow_and_load_response_time("2018-10-02 09:30:00"~"2018-10-02 15:30:00")

data_one_min %>%
  plot_flow_and_load_response_time("2018-11-13 13:30:00"~"2018-11-13 18:30:00")





