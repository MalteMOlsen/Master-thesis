#Finding the times where the process tanks were insufficient

#Setup
#Setup
source("load_all_data.R")
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
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) %>% 
  mutate(ammonium_load_effluent_kg_h=ammonium_effluent_mg_L*flow_effluent_m3_h/1000) %>%
  mutate(nitrate_load_effluent_kg_h=nitrate_effluent_mg_L*flow_effluent_m3_h/1000) %>% 
  mutate(total_N_load_effluent_kg_h=nitrate_load_effluent_kg_h+ammonium_load_effluent_kg_h)
  


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


#Set up of the days, month, and year
day1 = "2022-01-11"
day2 = "2022-01-13"
month = "2022-01"
year = "2022"

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
#year
#===============================================================================

data_thirty_min %>% 
  plot_N_requirements(year)


#year-month
#-------------------------------------------------------------------------------
#data_thirty_min %>% 
 # plot_N_requirements(month)

data_thirty_min %>% 
#  filter(nitrate_effluent_mg_L<25) %>% 
  plot_influent_load_month()

data_thirty_min %>% 
  plot_process_tank_N_removal(month)

data_thirty_min %>%
  plot_process_tank_air_and_ss(month)

data_thirty_min %>%
  plot_process_tank_ss(month)

#=======================
#Effluent
data_thirty_min %>% 
  plot_N_requirements(day1~day2)

data_thirty_min %>%
  plot_process_tank_air_and_ss(day1~day2)

data_thirty_min %>%
  plot_process_tank_concentration_and_air(day1~day2)

data_thirty_min %>%
  plot_process_tank_ss(day1~day2)

data_one_min %>%
  plot_N_requirements_ONE_MIN(day1~day2)

temp1 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(ammonium_effluent_mg_L)

temp1 %>% autoplot()

temp7 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(nitrate_effluent_mg_L) %>% 
  filter(nitrate_effluent_mg_L<100)

temp7 %>% autoplot()

temp2 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(ammonium_PT1_mg_L)

temp2 %>% autoplot()

temp8 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(nitrate_PT1_mg_L)

temp8 %>% autoplot()

temp3 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(ammonium_PT2_mg_L)

temp3 %>% autoplot()

temp4 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(ammonium_AN_mg_L)

temp4 %>% autoplot()

temp5 <- data_one_min %>%
  filter_index(day1~day2) %>%
  select(flow_AN_m3_h)

temp5 %>% autoplot()

#REPEAT WITH OTHER DAYS

data_thirty_min %>%
  plot_effluent_1(day1~day2)

#=======================
#Influent and rain
data_thirty_min %>% 
  plot_influent_3(day1~day2)

data_thirty_min %>% 
  plot_influent_1(day1~day2)

# data_thirty_min %>% 
#   plot_influent_2(day1~day2)

#=======================
#Process tank 4
data_thirty_min %>% 
  plot_process_tank_4_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_4_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_4_air(day1~day2)



#=======================
#Process tank 3
data_thirty_min %>% 
  plot_process_tank_3_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_3_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_3_air(day1~day2)



#=======================
#Process tank 2
data_thirty_min %>% 
  plot_process_tank_2_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_2_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_2_air(day1~day2)



#=======================
#Process tank 1
data_thirty_min %>% 
  plot_process_tank_1_N_1(day1~day2)

data_thirty_min %>% 
  plot_process_tank_1_N_2(day1~day2)

data_thirty_min %>% 
  plot_process_tank_1_air(day1~day2)



#=======================
#Temperature
data_thirty_min %>%
  plot_process_tank_temperature(day1~day2)


#=======================
#Sum up
data_thirty_min %>% 
  plot_process_tank_N_removal(day1~day2)




