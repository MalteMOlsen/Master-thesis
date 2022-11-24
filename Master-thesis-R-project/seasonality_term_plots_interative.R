
#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")



training_data <- model_data %>% 
  filter_index(~"2021") %>% 
  fill_gaps()



training_data %>%
  gg_season(ammonium_load_AN_kg_h, "day")+
  theme_malte()


training_data %>%
  gg_season(ammonium_load_AN_kg_h, "week")+
  theme_malte()

training_data %>%
  gg_season(ammonium_load_AN_kg_h, "year")+
  theme_malte()

training_data %>%
  gg_season(ammonium_load_AN_kg_h, "year")+
  ylim(0,100)+
  theme_malte()

#Train Fourier models daily

FT_function <- training_data %>%
  model(
    model_1 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 2))
  ) %>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble() %>% 
  fill_gaps()

plot_data <- training_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  left_join(FT_function)

plot_data %>%
  gg_season(ammonium_load_AN_kg_h, "day")+
  geom_line(aes(y=.fitted),color="Black")+
  theme_malte()+
  labs(x="Hours [h]",
       y='Ammonium load in the AN tank [kg/h]')+
  ylim(0,150)
  



FT_function <- training_data %>%
  model(
    model_1 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6))
  ) %>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble() %>% 
  fill_gaps()

plot_data <- training_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  left_join(FT_function)

plot_data %>%
  gg_season(ammonium_load_AN_kg_h, "week")+
  geom_line(aes(y=.fitted),color="Black")+
  theme_malte()+
  labs(x="Days [d]",
       y='Ammonium load in the AN tank [kg/h]')+
  ylim(0,150)



#Making new training data frame and saving it as CSV
Fourier_model_training_data <- training_data %>%
  model(
    TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6)+
           fourier(period = "week", K = 30))
  ) %>% 
  augment() %>% 
  select(-.model,-.innov)%>% 
  filter_index("2018-01-26"~"2021") %>% 
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h)) %>% 
  mutate(.fitted=na.approx(.fitted)) %>% 
  mutate(.resid=na.approx(.resid)) %>% 
  rename("fitted"=.fitted) %>% 
  rename("resid"=.resid)

#Save as csv
#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(Fourier_model_training_data, 
          "seasonallity_adjusted_training_data_for_influent.csv")


#making the predicted seasonality for the valid data set

valid_data <- model_data %>% 
  filter_index("2022-01"~"2022-03")


Fourier_model_training_data <- training_data %>%
  model(
    TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6)+
           fourier(period = "week", K = 30))) 

fc_Fourier_model_valid_data <- forecast(Fourier_model_training_data,
                                        h=nrow(valid_data)) 

fc_Fourier_model_valid_data <- fc_Fourier_model_valid_data %>% 
  as_tsibble(index = time_thirty_min) %>% 
  select(-.model) %>% 
  select(-ammonium_load_AN_kg_h) %>% 
  rename("predicted"=.mean)


#Save the data as a csv file
write_csv(fc_Fourier_model_valid_data, 
          "seasonallity_prediction_for_validation_data_for_influent.csv")
