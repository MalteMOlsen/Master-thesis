#Finding the times where the process tanks were insufficient

#Setup
source("data_cleaning_remove_na.R")

#Make a column with total N in the effluent 

data_thirty_min <- data_thirty_min %>% 
  mutate(total_N_effluent_mg_L=ammonium_effluent_mg_L+nitrate_effluent_mg_L) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_AN_mg_L*flow_AN_m3_h/1000) %>% 
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
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) 
  


#Check the calculation
data_thirty_min %>% 
  filter_index("2020") %>% 
  ggplot(aes(x=time_thirty_min))+
    geom_line(aes(y=ammonium_effluent_mg_L),color="Red")+
    geom_line(aes(y=nitrate_effluent_mg_L),color="Blue")+
    geom_line(aes(y=total_N_effluent_mg_L),color="Green")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Creating the violation data frames
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Ammonium higher than the absolute value of 8 mg/L

over_8_ammonium <- data_thirty_min %>% 
  filter(ammonium_effluent_mg_L>=8)

over_8_ammonium %>% 
  ggplot(aes(x=time_thirty_min,
         y=ammonium_effluent_mg_L))+
  geom_point()


#Ammonium higher than the guiding value of 2 mg/L but under the absolute value of 8 mg/L

over_2_under_8_ammonium <- data_thirty_min %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(ammonium_effluent_mg_L>=2)

over_2_under_8_ammonium %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_effluent_mg_L))+
  geom_point()


#Total N higher than the absolute value of 8 mg/L

over_8_total_N <- data_thirty_min %>% 
  filter(total_N_effluent_mg_L>=8)

over_8_total_N %>% 
  ggplot(aes(x=time_thirty_min,
             y=total_N_effluent_mg_L))+
  geom_point()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#///////////////////////////////////////////////////////////////////////////////
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#-------------------------------------------------------------------------------
#Investigating the violation scenarios 
#-------------------------------------------------------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#///////////////////////////////////////////////////////////////////////////////
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#===============================================================================
#2022
#===============================================================================

data_thirty_min %>% 
  plot_N_requirements("2022")


#2022-04
#-------------------------------------------------------------------------------
data_thirty_min %>% 
  plot_N_requirements("2022-04-06"~"2022-04-11")


#
#=======================2022-04-05=======================
#Effluent
data_thirty_min %>% 
  plot_N_requirements("2022-04-04"~"2022-04-05")



#Influent and rain
data_thirty_min %>% 
  plot_influent_1("2022-04-01"~"2022-04-05")

data_thirty_min %>% 
  plot_influent_2("2022-04-04"~"2022-04-05")

data_thirty_min %>% 
  plot_influent_3("2022-04-01"~"2022-04-05")



#Process tank 4
data_thirty_min %>% 
  plot_process_tank_4_N_1("2022-04-04"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_4_N_2("2022-04-04"~"2022-04-04")

data_thirty_min %>% 
  plot_process_tank_4_air("2022-04-02"~"2022-04-05")



#Process tank 3
data_thirty_min %>% 
  plot_process_tank_3_N_1("2022-04-02"~"2022-04-04")

data_thirty_min %>% 
  plot_process_tank_3_N_2("2022-04-02"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_3_air("2022-04-02"~"2022-04-04")



#Process tank 2
data_thirty_min %>% 
  plot_process_tank_2_N_1("2022-04-02"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_2_N_2("2022-04-02"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_2_air("2022-04-02"~"2022-04-05")



#Process tank 1
data_thirty_min %>% 
  plot_process_tank_1_N_1("2022-04-02"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_1_N_2("2022-04-02"~"2022-04-05")

data_thirty_min %>% 
  plot_process_tank_1_air("2022-04-02"~"2022-04-05")

#Temperature
data_thirty_min %>%
  plot_process_tank_temperature("2022-04-02"~"2022-04-05")

#Sum up
data_thirty_min %>% 
  plot_process_tank_N_removal("2022-04-02"~"2022-04-05")

#=======================2022-05-07==================
#Effluent
data_thirty_min %>% 
  plot_N_requirements(day1~day2)


#Influent and rain
data_thirty_min %>% 
  plot_influent_1(day1~day2)

data_thirty_min %>% 
  plot_influent_2(day1~day2)

data_thirty_min %>% 
  plot_influent_3(day1~day2)



#Process tank 4
data_thirty_min %>% 
  plot_process_tank_4_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_4_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_4_air(day1~day2)



#Process tank 3
data_thirty_min %>% 
  plot_process_tank_3_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_3_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_3_air(day1~day2)



#Process tank 2
data_thirty_min %>% 
  plot_process_tank_2_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_2_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_2_air(day1~day2)



#Process tank 1
data_thirty_min %>% 
  plot_process_tank_1_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_1_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_1_air(day1~day2)

#Temperature
data_thirty_min %>%
  plot_process_tank_temperature(day1~day2)

#Sum up
data_thirty_min %>% 
  plot_process_tank_N_removal(day1~day2)




