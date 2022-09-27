#///////////////////////////////////////////////////////////////////////////////
#Finding the theme for the graphs to be used throught the project
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is finding the theme to be used in graphs throughout
#the thesis

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  autoplot()

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  autoplot()+
  theme_light()

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  autoplot()+
  theme_light()+
  theme(
    axis.line = element_line(),
    panel.border = element_blank())

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  autoplot()+
  theme_light()+
  theme(
    axis.line = element_line(colour = "Grey"),
    panel.border = element_blank())
