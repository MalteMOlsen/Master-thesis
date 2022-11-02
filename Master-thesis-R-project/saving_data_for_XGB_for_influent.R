
#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project")

#Load set up file
source("cleaning_data_for_XGB_of_influent.R")

#For the data frame without seasonality adjustment
training_data <- model_data %>% 
  filter_index("2018"~"2021")

test_data <- model_data %>% 
  filter_index("2022-01"~"2022-03")


#Training the seasonality models
FT_with_yearly <- training_data %>% 
  model(TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6) +
                     fourier(period = "week", K = 30) +
                     fourier(period = "year", K = 12))) %>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble()


FT_without_yearly <- training_data %>% 
  model(TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 6) +
               fourier(period = "week", K = 30)))%>% 
  augment() %>% 
  select(time_thirty_min,.fitted) %>% 
  as_tsibble()


#Making the data frames

model_data_FT_subtracted <- model_data %>% 
  left_join(FT_with_yearly) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_load_AN_kg_h-.fitted)


model_data_FT_without_yearly_subtracted <- model_data %>% 
  left_join(FT_without_yearly) %>% 
  mutate(ammonium_load_AN_kg_h=ammonium_load_AN_kg_h-.fitted)

#-------------------------------------------------------------------------------
#Saving data which is ready for XGB training in three data frame

#-----
#Spitting into training and validation set




#----------
#For the data frame with seasonality adjustment(yearly seasonality is included)
training_data_FT_subtracted <- model_data_FT_subtracted %>% 
  filter_index("2018"~"2021")

test_data_FT_subtracted <- model_data_FT_subtracted %>% 
  filter_index("2022-01"~"2022-03")


#--------
#For the data frame with seasonality adjustment(yearly seasonality is NOT included)
training_data_FT_without_yearly_subtracted <- model_data_FT_without_yearly_subtracted %>% 
  filter_index("2018"~"2021")

test_data_FT_without_yearly_subtracted <- model_data_FT_without_yearly_subtracted %>% 
  filter_index("2022-01"~"2022-03")

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

#----------
#For the data frame with seasonality adjustment(yearly seasonality is included)

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(training_data_FT_subtracted, 
          "training_data_FT_subtracted_for_influent.csv")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(test_data_FT_subtracted, 
          "validation_data_fo_FT_subtractedr_influent.csv")


#---------
#For the data frame with seasonality adjustment(yearly seasonality is NOT included)
#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(training_data_FT_without_yearly_subtracted, 
          "training_data_FT_without_yearly_subtracted_for_influent.csv")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(test_data_FT_without_yearly_subtracted, 
          "validation_data_FT_without_yearly_subtracted_for_influent.csv")







