#///////////////////////////////////////////////////////////////////////////////
#Removal investigation
#///////////////////////////////////////////////////////////////////////////////

#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

temp <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter(is.na(ammonium_load_AN_kg_h))


temp2 <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  filter(is.na(ammonium_load_AN_kg_h)) %>% 
  mutate(count=1) %>% 
  mutate(day=as.Date(time_thirty_min)) %>%
  as_tibble() %>% 
  select(-time_thirty_min) %>% 
  group_by(day) %>% 
  summarise(sum_count=sum(count))


model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2019-03-03"~"2019-03-12") %>% 
  autoplot()+
  theme_malte()+
  xlab("Date")+
  ylab("Ammonium load in the AN tank [kg/h]")


model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2018-10-28"~"2018-10-29") %>% 
  autoplot()+
  theme_malte()+
  xlab("Date")+
  ylab("Ammonium load in the AN tank [kg/h]")


model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2019-06-23"~"2019-07-18") %>% 
  autoplot()+
  theme_malte()+
  xlab("Date")+
  ylab("Ammonium load in the AN tank [kg/h]")


model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2021-06-01"~"2021-06-03") %>% 
  autoplot()+
  theme_malte()+
  xlab("Date")+
  ylab("Ammonium load in the AN tank [kg/h]")

