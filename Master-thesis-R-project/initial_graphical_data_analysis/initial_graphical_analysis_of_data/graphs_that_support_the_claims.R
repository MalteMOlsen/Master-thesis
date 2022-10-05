#///////////////////////////////////////////////////////////////////////////////
#Finding the times where the process tanks were insufficient
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find time where the process tanks were insufficient
#and to generate a intuitive understanding of the data

#The way the different functions works are discribed in the "functions_used_for_initial_graphical_analysis.R"

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

#-------------------------------------------------------------------------------
#Claim one: rain and ammonium load
#-------------------------------------------------------------------------------


rain_ammonium_load <- function(time_interval1,
                               time_interval2){
  p1 <- data_thirty_min %>% 
    filter_index(time_interval1) %>% 
    ggplot(aes(x=time_thirty_min))+
      geom_line(aes(y=ammonium_load_AN_kg_h))+
      geom_line(aes(y=rainfall_mm*15),
               color="Red")+ 
    #Create an second axis to the flow and scale it properly
    scale_y_continuous(
      name = "Ammonium load [kg/h]",
      sec.axis = sec_axis(~.*1/15, name="Rain [mm]"))+
      theme_malte()

  p2 <- data_thirty_min %>% 
    filter_index(time_interval2) %>% 
    ggplot(aes(x=time_thirty_min))+
    geom_line(aes(y=ammonium_load_AN_kg_h))+
    geom_line(aes(y=rainfall_mm*25),
              color="Red")+ 
    #Create an second axis to the flow and scale it properly
    scale_y_continuous(
      name = "Ammonium load [kg/h]",
      sec.axis = sec_axis(~.*1/25, name="Rain [mm]"))+
    theme_malte()
  
  gridExtra::grid.arrange(p1,p2)
}


rain_ammonium_load("2020-01"~"2020-03",
                   "2020-04"~"2020-06")


rain_ammonium_load("2020-02",
                   "2020-03")

rain_ammonium_load("2019-04"~"2019-06",
                   "2019-08"~"2019-10")


rain_ammonium_load("2021-07"~"2021-09",
                   "2021-10"~"2021-12")


#-------------------------------------------------------------------------------
#Claim two: rain and ammonium concentration in the AN tank
#-------------------------------------------------------------------------------

rain_ammonium_concentration <- function(time_interval1,
                               time_interval2){
  p1 <- data_thirty_min %>% 
    filter_index(time_interval1) %>% 
    ggplot(aes(x=time_thirty_min))+
    geom_line(aes(y=ammonium_to_AN_mg_L))+
    geom_line(aes(y=rainfall_mm*15),
              color="Red")+ 
    #Create an second axis to the flow and scale it properly
    scale_y_continuous(
      name = "Ammonium concentration [mg/L]",
      sec.axis = sec_axis(~.*1/15, name="Rain [mm]"))+
    theme_malte()
  
  p2 <- data_thirty_min %>% 
    filter_index(time_interval2) %>% 
    ggplot(aes(x=time_thirty_min))+
    geom_line(aes(y=ammonium_to_AN_mg_L))+
    geom_line(aes(y=rainfall_mm*25),
              color="Red")+ 
    #Create an second axis to the flow and scale it properly
    scale_y_continuous(
      name = "Ammonium concentration [mg/L]",
      sec.axis = sec_axis(~.*1/25, name="Rain [mm]"))+
    theme_malte()
  
  gridExtra::grid.arrange(p1,p2)
}



rain_ammonium_concentration("2020-01-04"~"2020-01-06",
                   "2020-04-10"~"2020-04-12")


rain_ammonium_concentration("2020-02-10"~"2020-02-17",
                   "2020-03-09"~"2020-03-16")

rain_ammonium_concentration("2019-04-20"~"2019-04-30",
                   "2019-08-03"~"2019-08-13")


#




















#Define the year, month and days/timeperiods
year="2022"
month="2022-02"
day1="03-03-2022"
day2="05-03-2022"


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
#To get an overview and plot the effluent in over one year
data_thirty_min %>% 
  plot_N_requirements(year)

#===============================================================================
#month
#===============================================================================
#The next three visualizations are used to get a more detail picture of the 
#operation based on one month of operation

data_thirty_min %>% 
  plot_influent_load_month()

data_thirty_min %>%
  plot_process_tank_air_and_ss(month)

data_thirty_min %>%
  plot_process_tank_ss(month)

#===============================================================================
#Time period from day 1 to day 2
#===============================================================================

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


data_thirty_min %>%
  plot_effluent_1(day1~day2)

#=======================
#Influent and rain
data_thirty_min %>% 
  plot_influent_3(day1~day2)

data_thirty_min %>% 
  plot_influent_1(day1~day2)

data_thirty_min %>% 
  plot_influent_2(day1~day2)

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


#=======================
#Investgation of the one minute data frame
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