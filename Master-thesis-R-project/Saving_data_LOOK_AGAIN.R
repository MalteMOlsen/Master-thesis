
#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

#-------------------------------------------------------------------------------
#One-Hot encoder for week days
model_data <- model_data %>% 
  mutate(weekday=weekdays(time_thirty_min)) %>% 
  mutate(monday=if_else(weekday=="mandag",
                         1,
                         0))%>% 
  mutate(tuesday=if_else(weekday=="tirsdag",
                         1,
                         0))%>% 
  mutate(wednesday=if_else(weekday=="onsdag",
                           1,
                           0))%>% 
  mutate(thursday=if_else(weekday=="torsdag",
                          1,
                          0))%>% 
  mutate(friday=if_else(weekday=="fredag",
                        1,
                        0))%>% 
  mutate(saturday=if_else(weekday=="lørdag",
                          1,
                          0))%>% 
  mutate(sunday=if_else(weekday=="søndag",
                        1,
                        0)) %>% 
  select(-weekday)

#Making the export data
model_data <- model_data %>% 
  select(time_thirty_min,
         drought,
         ammonium_load_AN_kg_h,
         rainfall_mm,
         year_measurement,
         month_measurement,
         monday,
         tuesday,
         wednesday,
         thursday,
         friday,
         saturday,
         sunday)



#For the data frame without seasonality adjustment
training_data <- model_data %>% 
  filter_index("2018-01-26"~"2021") %>% 
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h))


test_data <- model_data %>% 
  filter_index("2022-01"~"2022-03") %>% 
  na.approx(ammonium_load_AN_kg_h) 




#-------------------------------------------------------------------------------
#Saving model data
#---------
#save model data without 

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(training_data, 
          "training_data_for_influent.csv")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(test_data, 
          "validation_data_for_influent.csv")

