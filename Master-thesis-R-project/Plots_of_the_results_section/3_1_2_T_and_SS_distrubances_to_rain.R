#The purpose of this script is to make the plots for section 3.1.2
#Setup
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
source("functions_used_for_initial_graphical_analysis.R")

theme_malte <- function(){
  theme_light(base_size = 16)+
    theme(
      axis.line = element_line(),
      panel.border = element_blank(),
      axis.title=element_text(size=16),
      axis.text=element_text(size=12))
}

#Make a column additional columns that will be used later on
data_thirty_min <- data_thirty_min %>% 
  mutate(total_N_effluent_mg_L=ammonium_effluent_mg_L+nitrate_effluent_mg_L) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h/1000) %>%
  mutate(ammonium_load_PT4_kg_h=ammonium_PT4_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(ammonium_load_PT3_kg_h=ammonium_PT3_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(ammonium_load_PT2_kg_h=ammonium_PT2_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(ammonium_load_PT1_kg_h=ammonium_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)/1000) %>% 
  mutate(nitrate_load_PT4_kg_h=nitrate_PT4_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(nitrate_load_PT3_kg_h=nitrate_PT3_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(nitrate_load_PT2_kg_h=nitrate_PT2_mg_L*flow_AN_m3_h/1000) %>% 
  mutate(nitrate_load_PT1_kg_h=nitrate_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)/1000) %>% 
  mutate(total_N_load_PT4_kg_h=nitrate_load_PT4_kg_h+ammonium_load_PT4_kg_h) %>% 
  mutate(total_N_load_PT3_kg_h=nitrate_load_PT3_kg_h+ammonium_load_PT3_kg_h) %>% 
  mutate(total_N_load_PT2_kg_h=nitrate_load_PT2_kg_h+ammonium_load_PT2_kg_h) %>% 
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) %>% 
  mutate(ammonium_load_effluent_kg_h=ammonium_effluent_mg_L*flow_effluent_m3_h/1000) %>%
  mutate(nitrate_load_effluent_kg_h=nitrate_effluent_mg_L*flow_effluent_m3_h/1000) %>% 
  mutate(total_N_load_effluent_kg_h=nitrate_load_effluent_kg_h+ammonium_load_effluent_kg_h)

data_thirty_min <- data_thirty_min %>%  
  fill_gaps()

#Ammonium load 
p_T_distrubance <- data_thirty_min %>%
  filter_index("2020-01-08"~"2020-01-15") %>% 
  ggplot(aes(x=time_thirty_min,
             y=T_PT4_C))+
  geom_line()+
  labs(x=element_blank(),
       y='*1')+
  theme_malte()


p_SS_distrubance <- data_thirty_min %>%
  filter_index("2020-01-08"~"2020-01-15") %>% 
  ggplot(aes(x=time_thirty_min,
             y=SS_PT4_g_L))+
  geom_line()+
  labs(x=element_blank(),
       y='*2')+
  theme_malte()

p_rain_distrubance <- data_thirty_min %>%
  filter_index("2020-01-08"~"2020-01-15") %>% 
  ggplot(aes(x=time_thirty_min,
             y=rainfall_mm))+
  geom_line()+
  labs(x="Days [d]",
       y='*3',
       caption = "
       *1: Temperature in process tank 4 [°C]
       *2: Suspended solids concentration in process tank 4 [g/L]
       *3: Rain precipitation [mm")+
  theme_malte()

gridExtra::grid.arrange(p_T_distrubance,
                        p_SS_distrubance,
                        p_rain_distrubance)

