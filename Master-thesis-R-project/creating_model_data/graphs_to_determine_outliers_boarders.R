#///////////////////////////////////////////////////////////////////////////////
#Graphs to determined the boarders of the outliers
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to make a graphical investigation of the different 
#data frames and determined where the hard cut off should be

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Run the Setup file
source("setup_for_model_data_creation.R")


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Adding the difference columns
source("adding_difference_columns.R")


#-------------------------------------------------------------------------------
#Making the graphs functions for this script

one_graph <- function(df, column){
  df %>% 
    select({{column}}) %>% 
    autoplot()+
    theme_malte()
}

two_graph <- function(df, column1, column2){
  p1<- df %>% 
    select({{column1}}) %>% 
    autoplot()+
    theme_malte()
  
  p2<- df %>% 
    select({{column2}}) %>% 
    autoplot()+
    theme_malte()
  
  gridExtra::grid.arrange(p1,p2)
}

three_graph <- function(df, column1, column2, column3){
  p1<- df %>% 
    select({{column1}}) %>% 
    autoplot()+
    theme_malte()
  
  p2<- df %>% 
    select({{column2}}) %>% 
    autoplot()+
    theme_malte()
  
  p3<- df %>% 
    select({{column3}}) %>% 
    autoplot()+
    theme_malte()
  
  gridExtra::grid.arrange(p1,p2,p3)
}

four_graph <- function(df, column1, column2, column3, column4){
  p1<- df %>% 
    select({{column1}}) %>% 
    autoplot()+
    theme_malte()
  
  p2<- df %>% 
    select({{column2}}) %>% 
    autoplot()+
    theme_malte()
  
  p3<- df %>% 
    select({{column3}}) %>% 
    autoplot()+
    theme_malte()
  
  p4<- df %>% 
    select({{column4}}) %>% 
    autoplot()+
    theme_malte()
  
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
}

#-------------------------------------------------------------------------------
#The influent

#Rain
data_one_min %>% 
  two_graph(rainfall_mm,
            diff_rainfall_mm)

#Flow (influent)
data_one_min %>% 
  two_graph(flow_influent_m3_h,
            diff_flow_influent_m3_h)

data_one_min %>% 
  filter(diff_flow_influent_m3_h<2500) %>% 
  filter(diff_flow_influent_m3_h>-2500) %>% 
  one_graph(diff_flow_influent_m3_h)


#Flow (AN tank)
data_one_min %>% 
  two_graph(flow_AN_m3_h,
            diff_flow_AN_m3_h)


data_one_min %>% 
  filter(abs(diff_flow_AN_m3_h)<250) %>%
  one_graph(diff_flow_AN_m3_h)


#Ammonium (AN tank)
data_one_min %>% 
  two_graph(ammonium_to_AN_mg_L,
            diff_ammonium_to_AN_mg_L)

temp <- data_one_min %>%
  select(ammonium_to_AN_mg_L,
         diff_ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L<250) %>% 
  filter(abs(diff_ammonium_to_AN_mg_L)<25)

temp %>% 
  two_graph(ammonium_to_AN_mg_L,
            diff_ammonium_to_AN_mg_L)

temp <- data_one_min %>%
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L<100) 
temp %>% 
  one_graph(ammonium_to_AN_mg_L)

temp <- data_one_min %>%
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L<85) 
temp %>% 
  one_graph(ammonium_to_AN_mg_L)


#Flow (Hydrolysis tank)
data_one_min %>% 
  two_graph(flow_HT_m3_h,
            diff_flow_HT_m3_h)


#SS (AN tank)
data_one_min %>% 
  two_graph(SS_to_AN_g_L,
            diff_SS_to_AN_g_L)

#-------------------------------------------------------------------------------
#The temperature

#All four process tank
data_one_min %>% 
  four_graph(T_PT1_C,
             T_PT2_C,
             T_PT3_C,
             T_PT4_C)


#Difference in all four process tank
data_one_min %>% 
  four_graph(diff_T_PT1_C,
             diff_T_PT2_C,
             diff_T_PT3_C,
             diff_T_PT4_C)

temp <- data_one_min %>%
  select(diff_T_PT1_C,
         diff_T_PT2_C,
         diff_T_PT3_C,
         diff_T_PT4_C) %>%  
  filter(abs(diff_T_PT1_C)<5) %>% 
  filter(abs(diff_T_PT2_C)<5) %>% 
  filter(abs(diff_T_PT3_C)<5) %>% 
  filter(abs(diff_T_PT4_C)<5)

temp %>% 
  four_graph(diff_T_PT1_C,
           diff_T_PT2_C,
           diff_T_PT3_C,
           diff_T_PT4_C)
#-------------------------------------------------------------------------------
#The ammonium in the process tank

#All four process tank
data_one_min %>% 
  four_graph(ammonium_PT1_mg_L,
             ammonium_PT2_mg_L,
             ammonium_PT3_mg_L,
             ammonium_PT4_mg_L)


#Difference in all four process tank
data_one_min %>% 
  four_graph(diff_ammonium_PT1_mg_L,
             diff_ammonium_PT2_mg_L,
             diff_ammonium_PT3_mg_L,
             diff_ammonium_PT4_mg_L)

temp <- data_one_min %>%
  select(diff_ammonium_PT1_mg_L,
         diff_ammonium_PT2_mg_L,
         diff_ammonium_PT3_mg_L,
         diff_ammonium_PT4_mg_L) %>%  
  filter(abs(diff_ammonium_PT1_mg_L)<25) %>% 
  filter(abs(diff_ammonium_PT2_mg_L)<25) %>% 
  filter(abs(diff_ammonium_PT3_mg_L)<25) %>% 
  filter(abs(diff_ammonium_PT4_mg_L)<25)

temp %>% 
  four_graph(diff_ammonium_PT1_mg_L,
             diff_ammonium_PT2_mg_L,
             diff_ammonium_PT3_mg_L,
             diff_ammonium_PT4_mg_L)

#-------------------------------------------------------------------------------
#The SS in the process tank
data_one_min %>% 
  two_graph(SS_PT1_g_L,
             SS_PT4_g_L)

data_one_min %>%  
  filter_index("2020") %>% 
  two_graph(SS_PT1_g_L,
            SS_PT4_g_L)

data_one_min %>%
  two_graph(diff_SS_PT1_g_L,
            diff_SS_PT4_g_L)


temp <- data_one_min %>%
  select(diff_SS_PT1_g_L,
       diff_SS_PT4_g_L) %>%  
  filter(abs(diff_SS_PT1_g_L)<5) %>% 
  filter(abs(diff_SS_PT4_g_L)<5)
  
temp %>% 
  two_graph(diff_SS_PT1_g_L,
            diff_SS_PT4_g_L)

#-------------------------------------------------------------------------------
#The aeration in the process tank

#All four process tank
data_one_min %>% 
  four_graph(airflow_PT1_m3_h,
             airflow_PT2_m3_h,
             airflow_PT3_m3_h,
             airflow_PT4_m3_h)

temp <- data_one_min %>%
  select(airflow_PT1_m3_h,
         airflow_PT2_m3_h,
         airflow_PT3_m3_h,
         airflow_PT4_m3_h) %>%  
  filter(abs(airflow_PT1_m3_h)<8000) %>% 
  filter(abs(airflow_PT2_m3_h)<8000) %>% 
  filter(abs(airflow_PT3_m3_h)<8000) %>% 
  filter(abs(airflow_PT4_m3_h)<8000)

temp %>% 
  four_graph(airflow_PT1_m3_h,
             airflow_PT2_m3_h,
             airflow_PT3_m3_h,
             airflow_PT4_m3_h)

#diff all four process tank
data_one_min %>% 
  four_graph(diff_airflow_PT1_m3_h,
             diff_airflow_PT2_m3_h,
             diff_airflow_PT3_m3_h,
             diff_airflow_PT4_m3_h)

temp <- data_one_min %>%
  select(diff_airflow_PT1_m3_h,
          diff_airflow_PT2_m3_h,
          diff_airflow_PT3_m3_h,
          diff_airflow_PT4_m3_h) %>%  
  filter(abs(diff_airflow_PT1_m3_h)<40000) %>% 
  filter(abs(diff_airflow_PT2_m3_h)<40000) %>% 
  filter(abs(diff_airflow_PT3_m3_h)<40000) %>% 
  filter(abs(diff_airflow_PT4_m3_h)<40000)

temp %>% 
  four_graph(diff_airflow_PT1_m3_h,
             diff_airflow_PT2_m3_h,
             diff_airflow_PT3_m3_h,
             diff_airflow_PT4_m3_h)


#-------------------------------------------------------------------------------
#The nitrate in the process tank

#All four process tank
data_one_min %>% 
  four_graph(nitrate_PT1_mg_L,
             nitrate_PT2_mg_L,
             nitrate_PT3_mg_L,
             nitrate_PT4_mg_L)

#Diff in all four process tank
data_one_min %>% 
  four_graph(diff_nitrate_PT1_mg_L,
             diff_nitrate_PT2_mg_L,
             diff_nitrate_PT3_mg_L,
             diff_nitrate_PT4_mg_L)



#-------------------------------------------------------------------------------
#The DO in the process tank

#All four process tank
data_one_min %>% 
  four_graph(DO_PT1_mg_L,
             DO_PT2_mg_L,
             DO_PT3_mg_L,
             DO_PT4_mg_L)

#diff in all four process tank
data_one_min %>% 
  four_graph(diff_DO_PT1_mg_L,
             diff_DO_PT2_mg_L,
             diff_DO_PT3_mg_L,
             diff_DO_PT4_mg_L)

#-------------------------------------------------------------------------------
#The ammonium in the effluent

data_one_min %>% 
  two_graph(ammonium_effluent_mg_L,
             diff_ammonium_effluent_mg_L)

#-------------------------------------------------------------------------------
#The nitrate in the effluent

data_one_min %>% 
  two_graph(nitrate_effluent_mg_L,
            diff_nitrate_effluent_mg_L)


#-------------------------------------------------------------------------------
#The flow in the effluent

data_one_min %>% 
  two_graph(flow_effluent_m3_h,
            diff_flow_effluent_m3_h)

