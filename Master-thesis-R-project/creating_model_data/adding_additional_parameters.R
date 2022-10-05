#///////////////////////////////////////////////////////////////////////////////
#Adding additonal parameters to the half hourly data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is add the following paramters


#Making the load of ammonium and nitrate
model_data <- model_data %>%  
  mutate(total_N_effluent_mg_L=ammonium_effluent_mg_L+nitrate_effluent_mg_L) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_to_AN_mg_L*flow_AN_m3_h*10^-3) %>%
  mutate(ammonium_load_PT4_kg_h=ammonium_PT4_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT3_kg_h=ammonium_PT3_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT2_kg_h=ammonium_PT2_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(ammonium_load_PT1_kg_h=ammonium_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)*10^-3) %>% 
  mutate(nitrate_load_PT4_kg_h=nitrate_PT4_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT3_kg_h=nitrate_PT3_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT2_kg_h=nitrate_PT2_mg_L*flow_AN_m3_h*10^-3) %>% 
  mutate(nitrate_load_PT1_kg_h=nitrate_PT1_mg_L*(flow_AN_m3_h+flow_HT_m3_h)*10^-3) %>% 
  mutate(total_N_load_PT4_kg_h=nitrate_load_PT4_kg_h+ammonium_load_PT4_kg_h) %>% 
  mutate(total_N_load_PT3_kg_h=nitrate_load_PT3_kg_h+ammonium_load_PT3_kg_h) %>% 
  mutate(total_N_load_PT2_kg_h=nitrate_load_PT2_kg_h+ammonium_load_PT2_kg_h) %>% 
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h) %>% 
  mutate(ammonium_load_effluent_kg_h=ammonium_effluent_mg_L*flow_effluent_m3_h*10^-3) %>%
  mutate(nitrate_load_effluent_kg_h=nitrate_effluent_mg_L*flow_effluent_m3_h*10^-3) %>% 
  mutate(total_N_load_effluent_kg_h=nitrate_load_effluent_kg_h+ammonium_load_effluent_kg_h)

#Making the rain difference column
model_data <- model_data %>%  
  mutate(rain_difference=difference(rainfall_mm))

#Making the month and year dummy parameter
model_data <- model_data %>% 
  mutate(month_measurement=month(time_thirty_min)) %>% 
  mutate(year_measurement=year(time_thirty_min))

#Removal of ammonium in each tank
model_data<- model_data %>% 
  mutate(PT4_ammonium_removal_kg_h=ammonium_load_AN_kg_h - ammonium_load_PT4_kg_h) %>%  
  mutate(PT3_ammonium_removal_kg_h=ammonium_load_PT4_kg_h - ammonium_load_PT3_kg_h) %>% 
  mutate(PT2_ammonium_removal_kg_h=ammonium_load_PT3_kg_h - ammonium_load_PT2_kg_h) %>% 
  mutate(PT1_ammonium_removal_kg_h=ammonium_load_PT2_kg_h - ammonium_load_PT1_kg_h) %>%
  mutate(sand_filter_ammonium_removal_kg_h=ammonium_load_PT1_kg_h - ammonium_load_effluent_kg_h) %>% 
  mutate(percentage_PT4_ammonium_removal_kg_h=PT4_ammonium_removal_kg_h/(ammonium_load_AN_kg_h - ammonium_load_effluent_kg_h)*100) %>%  
  mutate(percentage_PT3_ammonium_removal_kg_h=PT3_ammonium_removal_kg_h/(ammonium_load_AN_kg_h - ammonium_load_effluent_kg_h)*100) %>% 
  mutate(percentage_PT2_ammonium_removal_kg_h=PT2_ammonium_removal_kg_h/(ammonium_load_AN_kg_h - ammonium_load_effluent_kg_h)*100) %>% 
  mutate(percentage_PT1_ammonium_removal_kg_h=PT1_ammonium_removal_kg_h/(ammonium_load_AN_kg_h - ammonium_load_effluent_kg_h)*100) %>%
  mutate(percentage_sand_filter_ammonium_removal_kg_h=sand_filter_ammonium_removal_kg_h/(ammonium_load_AN_kg_h - ammonium_load_effluent_kg_h)*100)

#Accumulated rain over 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8, 9, 
#10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 hours

model_data <- model_data %>%
  mutate(rain_one_hour_accumulated = rollapplyr(rainfall_mm, 
                                               width=2, 
                                               FUN=sum, 
                                               partial=T))%>%
  mutate(rain_one_and_a_half_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=3, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_two_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=4, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_two_and_a_half_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=5, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_three_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=6, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_three_and_a_half_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=7, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_four_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=8, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_four_and_a_half_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=9, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_five_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=10, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_five_and_a_half_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=11, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_six_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=12, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_seven_hour_accumulated = rollapplyr(rainfall_mm, 
                                                width=14, 
                                                FUN=sum, 
                                                partial=T))%>%
  mutate(rain_eight_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=16, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_nine_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=18, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_ten_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=20, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_twelve_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=24, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_thriteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=26, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_fourteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=28, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_fifteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=30, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_sixteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=32, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_seventeen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=34, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_eighteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=36, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_nineteen_hour_accumulated = rollapplyr(rainfall_mm, 
                                                  width=38, 
                                                  FUN=sum, 
                                                  partial=T))%>%
  mutate(rain_twenty_hour_accumulated = rollapplyr(rainfall_mm, 
                                                     width=40, 
                                                     FUN=sum, 
                                                     partial=T))%>%
  mutate(rain_twentyone_hour_accumulated = rollapplyr(rainfall_mm, 
                                                     width=42, 
                                                     FUN=sum, 
                                                     partial=T))%>%
  mutate(rain_tweentytwo_hour_accumulated = rollapplyr(rainfall_mm, 
                                                     width=44, 
                                                     FUN=sum, 
                                                     partial=T))%>%
  mutate(rain_twentythree_hour_accumulated = rollapplyr(rainfall_mm, 
                                                     width=46, 
                                                     FUN=sum, 
                                                     partial=T))


