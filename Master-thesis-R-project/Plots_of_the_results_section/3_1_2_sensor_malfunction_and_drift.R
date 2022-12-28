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
      axis.title=element_text(size=12),
      axis.text=element_text(size=11),
      plot.tag = element_text(size = 14),
      plot.tag.position = c(1, 1))
}

# #Make a column additional columns that will be used later on
# data_thirty_min <- data_thirty_min %>% 
#   mutate(total_N_effluent_mg_L=ammonium_effluent_mg_L+nitrate_effluent_mg_L) %>% 
#   mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h/1000) %>%
#   mutate(ammonium_load_PT4_kg_h=ammonium_PT4_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(ammonium_load_PT3_kg_h=ammonium_PT3_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(ammonium_load_PT2_kg_h=ammonium_PT2_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(ammonium_load_PT1_kg_h=ammonium_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)/1000) %>% 
#   mutate(nitrate_load_PT4_kg_h=nitrate_PT4_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(nitrate_load_PT3_kg_h=nitrate_PT3_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(nitrate_load_PT2_kg_h=nitrate_PT2_mg_L*flow_AN_m3_h/1000) %>% 
#   mutate(nitrate_load_PT1_kg_h=nitrate_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)/1000) %>% 
#   mutate(total_N_load_PT4_kg_h=nitrate_load_PT4_kg_h+ammonium_load_PT4_kg_h) %>% 
#   mutate(total_N_load_PT3_kg_h=nitrate_load_PT3_kg_h+ammonium_load_PT3_kg_h) %>% 
#   mutate(total_N_load_PT2_kg_h=nitrate_load_PT2_kg_h+ammonium_load_PT2_kg_h) %>% 
#   mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) %>% 
#   mutate(ammonium_load_effluent_kg_h=ammonium_effluent_mg_L*flow_effluent_m3_h/1000) %>%
#   mutate(nitrate_load_effluent_kg_h=nitrate_effluent_mg_L*flow_effluent_m3_h/1000) %>% 
#   mutate(total_N_load_effluent_kg_h=nitrate_load_effluent_kg_h+ammonium_load_effluent_kg_h)
# 
# data_thirty_min <- data_thirty_min %>%  
#   fill_gaps()

#Ammonium load 
p_sensor_drift <- data_thirty_min %>%
  filter_index("2019-06-17"~"2019-07-02") %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_effluent_mg_L))+
  geom_line()+
  labs(x="Days [d]",
       y='Ammonium concentration 
       in the effluent [mg/L]',
       tag = "B")+
  theme_malte()


p_sensor_malfunction <- data_thirty_min %>%
  filter_index("2019-02-24"~"2019-03-25") %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_to_AN_mg_L))+
  geom_line()+
  labs(x=element_blank(),
       y='Ammonium concentration 
       in the AN tank [mg/L]',
       tag = "A")+
  theme_malte()

plot_grid(p_sensor_malfunction,
          p_sensor_drift,
          align = "v", 
          nrow = 2,
          ncol=1,
          rel_widths=c(1,1),
          rel_heights = c(14/30,16/30))

