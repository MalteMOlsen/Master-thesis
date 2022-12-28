#///////////////////////////////////////////////////////////////////////////////
#Section 3.1.2 - Qualitative learnings of rain events effect on wastewater treatment
#///////////////////////////////////////////////////////////////////////////////
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
# 
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
# 
# 
# 
# #Ammonium load 
# data_thirty_min %>%
#   filter_index("2020-07") %>% 
#   ggplot(aes(x=time_thirty_min,
#              y=ammonium_load_AN_kg_h))+
#   geom_line()+
#   labs(x="Hours [h]",
#        y='Ammonium load in the AN tank [kg/h]')+
#   theme_malte()
# 
# 
# 
# #Ammonium load 
# data_thirty_min %>%
#   filter_index("2020-07") %>% 
#   ggplot(aes(x=time_thirty_min,
#              y=ammonium_to_AN_mg_L))+
#   geom_line()+
#   labs(x="Hours [h]",
#        y='Ammonium concentration the AN tank [mg/L]')+
#   theme_malte()
# 
# 
# 
# #Sys.setenv("LANGUAGE"="En")
# Sys.setlocale("LC_ALL", "English")
#Ammonium load 
p_rain <- data_thirty_min %>%
  filter_index("2020-07-01"~"2020-07-11") %>% 
  ggplot(aes(x=time_thirty_min))+
  geom_line(aes(y=rainfall_mm),
            color= "Black")+
  labs(x=element_blank(),
       y="Precipitation [mm]",
       tag = "A")+
  theme_malte()


#Ammonium load 
p_ammonium_and_total_N_effluent <- data_thirty_min %>%
  filter_index("2020-07-01"~"2020-07-11") %>% 
  ggplot(aes(x=time_thirty_min))+
  geom_line(aes(y=ammonium_effluent_mg_L), 
            color="#BB3E03")+
  geom_line(aes(y=total_N_effluent_mg_L),
            color= "Black")+
  labs(x="Date",
       y="*3",
       caption = '*1: Rain precipitation [mm]
       *2: 
       *3: Ammonium and total N concentration in the effluent [mg/L] in orange and black, respectively',
       tag = "C")+
  theme_malte()


p_load_and_conc_AN_tank <- data_thirty_min %>%
  filter_index("2020-07-01"~"2020-07-11") %>% 
  ggplot(aes(x=time_thirty_min))+
  geom_line(aes(y=ammonium_load_AN_kg_h), 
            color="#BB3E03")+
  geom_line(aes(y=ammonium_to_AN_mg_L),
            color= "Black")+
  labs(x=element_blank(),
       y="The AN tank
       Black: ammonium conc. [mg/L]
       Orange: ammonium load [kg/h]",
       tag = "B")+
  theme_malte()

#library(gridExtra)
#library(cowplot)
#install.packages("cowplot")


plot_grid(p_rain,
                        p_load_and_conc_AN_tank,
                        p_ammonium_and_total_N_effluent,
                        align = "v", 
                        nrow = 3,
                        ncol=1,
                        rel_widths=c(1,1,1),
                        rel_heights = c(2/7, 2/7, 3/7))
























