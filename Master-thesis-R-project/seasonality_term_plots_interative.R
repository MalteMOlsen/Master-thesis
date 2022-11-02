
#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")



training_data <- model_data %>% 
  filter_index(~"2021") %>% 
  fill_gaps()




plotting_FT_function(day_term, week_term, year_term){
  
  FT_function <- training_data %>% 
    model(
      model_1 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = day_term) +
                    fourier(period = "week", K = week_term) +
                    fourier(period = "year", K = year_term)),
    
      model_1X2 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 2*day_term) +
                     fourier(period = "week", K = 2*week_term) +
                     fourier(period = "year", K = 2*year_term)),
    
      model_1X3 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 3*day_term) +
                     fourier(period = "week", K = 3*week_term) +
                     fourier(period = "year", K = 3*year_term))
    
              )
    
    p1 <- training_data %>%  
      fill_gaps() %>% 
      gg_season(ammonium_load_AN_kg_h, "day")+
      labs(x="Hours [h]",
           y='Ammonium load to the AN tank [kg/h]')+
      geom_line(data = fitted(FT_function),
                aes(y = .fitted, colour = .model))+
      theme_malte()
    
    
    p2 <- training_data %>%  
      fill_gaps() %>% 
      gg_season(ammonium_load_AN_kg_h, "week")+
      labs(x="Days [d]",
           y='Ammonium load to the AN tank [kg/h]')+
      geom_line(data = fitted(FT_function),
                aes(y = .fitted, colour = .model))+
      theme_malte()
    
    
    p3 <- training_data %>%  
      fill_gaps() %>% 
      gg_season(ammonium_load_AN_kg_h, "year")+
      labs(x="Months",
           y='Ammonium load to the AN tank [kg/h]')+
      geom_line(data = fitted(FT_function),
                aes(y = .fitted, colour = .model))+
      theme_malte()
    
    gridExtra::grid.arrange(p1,p2,p3)
  
}

plotting_FT_function(2,2,2)




FT_function <- training_data %>%
  filter(ammonium_load_AN_kg_h<100) %>% 
  model(
    model_1 = TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6) +
                     fourier(period = "week", K = 40) +
                     fourier(period = "year", K = 6))
   ) 

temp <- FT_function%>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble()
    

# temp %>% 
#   gg_season(.fitted, "day")+
#   theme_malte()

# temp %>% 
#   gg_season(.fitted, "week")+
#   theme_malte()
temp %>%
  fill_gaps() %>% 
  gg_season(.fitted, "year")+
  ylim(0,60)+
  theme_malte()



# training_data %>%  
#   fill_gaps() %>% 
#   gg_season(ammonium_load_AN_kg_h, "day")+
#   labs(x="Hours [h]",
#        y='Ammonium load to the AN tank [kg/h]')+
#   ylim(0,100)+
#   theme_malte()

training_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_load_AN_kg_h, "week")+
  labs(x="Days [d]",
       y='Ammonium load to the AN tank [kg/h]')+
  theme_malte()


training_data %>%  
  fill_gaps() %>% 
  gg_season(ammonium_load_AN_kg_h, "year")+
  labs(x="Months",
       y='Ammonium load to the AN tank [kg/h]')+
  ylim(0,60)+
  theme_malte()



FT_without_yearly <- training_data %>% 
  model(TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6) +
               fourier(period = "week", K = 30)))%>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble()

temp <- model_data %>% 
  left_join(FT_with_yearly) %>% 
  mutate(test=ammonium_load_AN_kg_h-.fitted)

temp %>% 
  filter_index("2021-01"~"2021-06") %>% 
  ggplot(aes(x=time_thirty_min))+
  geom_line(aes(y=test))+
  geom_line(aes(y=ammonium_load_AN_kg_h), 
            color="Blue")#+
# geom_line(aes(y=.fitted),
#           color="Red")
