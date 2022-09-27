#///////////////////////////////////////////////////////////////////////////////
#Selecting the columns for modelling and lagging them
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to select the column that will be used in the 
#modelling and therefore also the columns which needs to be extra cleaned

data_one_min <- data_one_min %>% 
  select(rainfall_mm,
         flow_AN_m3_h,
         flow_influent_m3_h,
         flow_effluent_m3_h,
         flow_HT_m3_h,
         ammonium_to_AN_mg_L,
         T_PT1_C,
         T_PT2_C,
         T_PT3_C,
         T_PT4_C,
         ammonium_PT1_mg_L,
         ammonium_PT2_mg_L,
         ammonium_PT3_mg_L,
         ammonium_PT4_mg_L,
         SS_PT1_g_L,
         SS_PT4_g_L,
         SS_to_AN_g_L,
         airflow_PT1_m3_h,
         airflow_PT2_m3_h,
         airflow_PT3_m3_h,
         airflow_PT4_m3_h,
         nitrate_PT1_mg_L,
         nitrate_PT2_mg_L,
         nitrate_PT3_mg_L,
         nitrate_PT4_mg_L,
         DO_PT1_mg_L,
         DO_PT2_mg_L,
         DO_PT3_mg_L,
         DO_PT4_mg_L,
         ammonium_effluent_mg_L,
         nitrate_effluent_mg_L,
         drought,
         EU_PT_kWh_h,
         SC_operatingNumber) %>% 
  mutate(lag1_rainfall_mm=lag(rainfall_mm)) %>% 
  mutate(lag1_flow_AN_m3_h=lag(flow_AN_m3_h)) %>% 
  mutate(lag1_flow_influent_m3_h=lag(flow_influent_m3_h)) %>% 
  mutate(lag1_flow_effluent_m3_h=lag(flow_effluent_m3_h)) %>% 
  mutate(lag1_flow_HT_m3_h=lag(flow_HT_m3_h)) %>% 
  mutate(lag1_ammonium_to_AN_mg_L=lag(ammonium_to_AN_mg_L)) %>% 
  mutate(lag1_T_PT1_C=lag(T_PT1_C)) %>% 
  mutate(lag1_T_PT2_C=lag(T_PT2_C)) %>% 
  mutate(lag1_T_PT3_C=lag(T_PT3_C)) %>% 
  mutate(lag1_T_PT4_C=lag(T_PT4_C)) %>% 
  mutate(lag1_ammonium_PT1_mg_L=lag(ammonium_PT1_mg_L)) %>% 
  mutate(lag1_ammonium_PT2_mg_L=lag(ammonium_PT2_mg_L)) %>% 
  mutate(lag1_ammonium_PT3_mg_L=lag(ammonium_PT3_mg_L)) %>% 
  mutate(lag1_ammonium_PT4_mg_L=lag(ammonium_PT4_mg_L)) %>% 
  mutate(lag1_SS_PT1_g_L=lag(SS_PT1_g_L)) %>% 
  mutate(lag1_SS_PT4_g_L=lag(SS_PT4_g_L)) %>% 
  mutate(lag1_SS_to_AN_g_L=lag(SS_to_AN_g_L)) %>% 
  mutate(lag1_airflow_PT1_m3_h=lag(airflow_PT1_m3_h)) %>% 
  mutate(lag1_airflow_PT2_m3_h=lag(airflow_PT2_m3_h)) %>% 
  mutate(lag1_airflow_PT3_m3_h=lag(airflow_PT3_m3_h)) %>% 
  mutate(lag1_airflow_PT4_m3_h=lag(airflow_PT4_m3_h)) %>% 
  mutate(lag1_nitrate_PT1_mg_L=lag(nitrate_PT1_mg_L)) %>% 
  mutate(lag1_nitrate_PT2_mg_L=lag(nitrate_PT2_mg_L)) %>% 
  mutate(lag1_nitrate_PT3_mg_L=lag(nitrate_PT3_mg_L)) %>% 
  mutate(lag1_nitrate_PT4_mg_L=lag(nitrate_PT4_mg_L)) %>% 
  mutate(lag1_DO_PT1_mg_L=lag(DO_PT1_mg_L)) %>% 
  mutate(lag1_DO_PT2_mg_L=lag(DO_PT2_mg_L)) %>% 
  mutate(lag1_DO_PT3_mg_L=lag(DO_PT3_mg_L)) %>% 
  mutate(lag1_DO_PT4_mg_L=lag(DO_PT4_mg_L)) %>% 
  mutate(lag1_ammonium_effluent_mg_L=lag(ammonium_effluent_mg_L)) %>% 
  mutate(lag1_nitrate_effluent_mg_L=lag(nitrate_effluent_mg_L)) 

         
         
         
         