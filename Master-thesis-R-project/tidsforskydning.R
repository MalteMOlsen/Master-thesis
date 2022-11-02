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


flow_plot <- function(time_interval){
  data_one_min %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_one_min))+
    geom_line(aes(y=flow_influent_m3_h))+
    geom_line(aes(y=flow_effluent_m3_h, color="RED"))
}

flow_plot("2022-01-01")


flow_plot("2020-01-01")









data_one_min <- data_one_min%>%  
  mutate(total_N_effluent_mg_L=ammonium_effluent_mg_L+nitrate_effluent_mg_L) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h*10^-3) %>%
  mutate(ammonium_load_PT4_kg_h=ammonium_PT4_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT3_kg_h=ammonium_PT3_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT2_kg_h=ammonium_PT2_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT1_kg_h=ammonium_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)*10^-3) %>% 
  mutate(nitrate_load_PT4_kg_h=nitrate_PT4_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT3_kg_h=nitrate_PT3_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT2_kg_h=nitrate_PT2_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT1_kg_h=nitrate_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)*10^-3) %>% 
  mutate(total_N_load_PT4_kg_h=nitrate_load_PT4_kg_h+ammonium_load_PT4_kg_h) %>% 
  mutate(total_N_load_PT3_kg_h=nitrate_load_PT3_kg_h+ammonium_load_PT3_kg_h) %>% 
  mutate(total_N_load_PT2_kg_h=nitrate_load_PT2_kg_h+ammonium_load_PT2_kg_h) %>% 
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) %>% 
  mutate(ammonium_load_effluent_kg_h=ammonium_effluent_mg_L*flow_effluent_m3_h*10^-3) %>%
  mutate(nitrate_load_effluent_kg_h=nitrate_effluent_mg_L*flow_effluent_m3_h*10^-3) %>% 
  mutate(total_N_load_effluent_kg_h=nitrate_load_effluent_kg_h+ammonium_load_effluent_kg_h)



PT4_plot <- function(time_interval){
  data_one_min %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_one_min))+
    geom_line(aes(y=ammonium_PT4_mg_L))+
    geom_line(aes(y=ammonium_load_PT4_kg_h, color="RED"))
}
PT3_plot <- function(time_interval){
  data_one_min %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_one_min))+
    geom_line(aes(y=ammonium_PT3_mg_L))+
    geom_line(aes(y=ammonium_load_PT3_kg_h, color="RED"))
}
PT2_plot <- function(time_interval){
  data_one_min %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_one_min))+
    geom_line(aes(y=ammonium_PT2_mg_L))+
    geom_line(aes(y=ammonium_load_PT2_kg_h, color="RED"))
}
PT1_plot <- function(time_interval){
  data_one_min %>% 
    filter_index(time_interval) %>% 
    ggplot(aes(x=time_one_min))+
    geom_line(aes(y=ammonium_PT1_mg_L))+
    geom_line(aes(y=ammonium_load_PT1_kg_h, color="RED"))
}



PT4_plot("2021-11-27"~"2021-11-30")
PT3_plot("2021-03-14"~"2021-03-16")
PT2_plot("2021-03-14"~"2021-03-16")
PT1_plot("2021-11-27"~"2021-11-30")


flow_plot("2020-01-01")



data_one_min %>% 
  filter_index("2021-11") %>% 
  ggplot(aes(x=time_one_min))+
  geom_line(aes(y=ammonium_effluent_mg_L))




data_one_min %>% names()




