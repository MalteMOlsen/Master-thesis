data_thirty_min %>% 
  ggplot(aes(x=flow_AN_m3_h)) +
  geom_point(aes(y=ammonium_effluent_mg_L),
            color="black")



data_thirty_min %>% 
  ggplot(aes(x=ammonium_load_AN_kg_h)) +
  geom_point(aes(y=ammonium_effluent_mg_L),
             color="black")


data_thirty_min %>% 
  ggplot(aes(x=rainfall_mm)) +
  geom_point(aes(y=ammonium_effluent_mg_L),
             color="black")

data_thirty_min %>% 
  ggplot(aes(x=rainfall_mm)) +
  geom_point(aes(y=ammonium_load_AN_kg_h))

data_thirty_min %>% 
  ggplot(aes(x=flow_AN_m3_h)) +
  geom_point(aes(y=ammonium_load_AN_kg_h))

data_thirty_min %>% 
  ggplot(aes(x=ammonium_effluent_mg_L)) +
  geom_point(aes(y=ammonium_load_AN_kg_h))


data_thirty_min %>% 
  ggplot(aes(x=rainfall_mm)) +
  geom_point(aes(y=ammonium_load_AN_kg_h))

data_thirty_min %>% 
  ggplot(aes(x=rain_one_day_accumulated)) +
  geom_point(aes(y=ammonium_load_AN_kg_h))

data_thirty_min %>% 
  ggplot(aes(x=ammonium_effluent_mg_L)) +
  geom_point(aes(y=airflow_PT1_m3_h))
  

data_thirty_min %>% 
  ggplot(aes(x=ammonium_load_AN_kg_h)) +
  geom_point(aes(y=airflow_PT1_m3_h))

data_thirty_min %>% 
  filter(airflow_PT4_m3_h<5000) %>% 
  ggplot(aes(x=ammonium_load_AN_kg_h)) +
  geom_point(aes(y=airflow_PT4_m3_h))



cor(temp$ammonium_effluent_mg_L,temp[-c(1)])

cor(temp$flow_effluent_m3_h,temp[-c(1)])
