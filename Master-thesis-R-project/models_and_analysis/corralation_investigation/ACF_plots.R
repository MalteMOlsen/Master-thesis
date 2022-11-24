#///////////////////////////////////////////////////////////////////////////////
#Autocorrelation plots
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to produces the autocorrelation plots for the 
#13 chosen paramters.

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

source("setup_model_data.R")

#Influent
#-----------------
#Ammonium load AN tank
model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  fill_gaps() %>% 
  ACF(, lag_max = 72) %>% 
  autoplot()+
  theme_malte()+
  ylab("Correlation - ammonium load in the AN tank")+
  xlab("Lagged time steps (step size = 30min)")


model_data %>% 
  select(flow_AN_m3_h) %>% 
  fill_gaps() %>% 
  ACF(, lag_max = 72) %>% 
  autoplot()+
  theme_malte()+
  ylab("Correlation - flow to the AN tank")+
  xlab("Lagged time steps (step size = 30min)")


model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  fill_gaps() %>% 
  ACF(, lag_max = 72) %>% 
  autoplot()+
  theme_malte()+
  ylab("Correlation - ammonium concentration in the AN tank")+
  xlab("Lagged time steps (step size = 30min)")












