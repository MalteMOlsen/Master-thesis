#///////////////////////////////////////////////////////////////////////////////
#Seasonality investigation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to investigate the seasonality of the ammonium
#load to the AN tank.

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

source("setup_model_data.R")

#Seasonality plots

#Ammonium load 
model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_load_AN_kg_h, "day")+
  labs(x="Hours [h]",
       y='Ammonium load to the AN tank [kg/h]')+
  theme_malte()


model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_load_AN_kg_h, "week")+
  labs(x="Days [d]",
       y='Ammonium load to the AN tank [kg/h]')+
  theme_malte()


model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_load_AN_kg_h, "year")+
  labs(x="Months",
       y='Ammonium load to the AN tank [kg/h]')+
  theme_malte()


#Ammonium concentration
model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_to_AN_mg_L, "day")+
  labs(x="Hours [h]",
       y='Ammonium concentration in the AN tank [mg/L]')+
  theme_malte()


model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_to_AN_mg_L, "week")+
  labs(x="Days [d]",
       y='Ammonium concentration in the AN tank [mg/L]')+
  theme_malte()

model_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_to_AN_mg_L, "year")+
  labs(x="Months",
       y='Ammonium concentration in the AN tank [mg/L]')+
  theme_malte()


#Flow to the AN tank
model_data %>%  
  fill_gaps() %>% 
  gg_season(flow_AN_m3_h, "day")+
  labs(x="Hours [h]",
       y='Flow to the AN tank ['~m^3~'/h]')+
  theme_malte()


model_data %>%  
  fill_gaps() %>% 
  gg_season(flow_AN_m3_h, "week")+
  labs(x="Days [d]",
       y='Flow to the AN tank ['~m^3~'/h]')+
  theme_malte()

model_data %>%  
  fill_gaps() %>% 
  gg_season(flow_AN_m3_h, "year")+
  labs(x="Months",
       y='Flow to the AN tank ['~m^3~'/h]')+
  theme_malte()








#Finding an load ourlier in 2019

model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  autoplot()
model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

model_data %>% 
  filter_index("2019") %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

model_data %>% 
  filter_index("2019") %>% 
  select(ammonium_PT4_mg_L) %>% 
  autoplot()

model_data %>% 
  filter_index("2019") %>%
  select(ammonium_load_AN_kg_h) %>% 
  autoplot()
model_data %>% 
  filter_index("2019") %>%
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
model_data %>% 
  filter_index("2019") %>%
  select(flow_AN_m3_h) %>% 
  autoplot()
model_data %>% 
  filter_index("2019") %>%
  select(rainfall_mm) %>% 
  autoplot()
