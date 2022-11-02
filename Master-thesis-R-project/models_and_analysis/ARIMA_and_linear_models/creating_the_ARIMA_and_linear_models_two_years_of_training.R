#///////////////////////////////////////////////////////////////////////////////
#Training the linear models and the ARIMA models - two years of training
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to train the ARIMA models and the linear models
#on two years of training data.

#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis")

source("adding_lag_columns.R")


#Defining the training and validation data
#-------
training_data <- model_data %>% 
  filter_index("2020"~"2021") %>%  
  mutate(rain_elleven_hour_accumulated = rollapplyr(rainfall_mm, 
                                                    width=22, 
                                                    FUN=sum, 
                                                    partial=T)) %>%
  mutate(DIxR1H=rain_one_hour_accumulated*drought) %>% 
  mutate(DIxR1hH=rain_one_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR2H=rain_two_hour_accumulated*drought) %>%
  mutate(DIxR2hH=rain_two_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR3H=rain_three_hour_accumulated*drought) %>%
  mutate(DIxR3hH=rain_three_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR4H=rain_four_day_accumulated*drought) %>%
  mutate(DIxR4hH=rain_four_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR5H=rain_five_day_accumulated*drought) %>%
  mutate(DIxR5hH=rain_five_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR6H=rain_six_hour_accumulated*drought) %>%
  mutate(DIxR7H=rain_seven_hour_accumulated*drought) %>%
  mutate(DIxR8H=rain_eight_hour_accumulated*drought) %>%
  mutate(DIxR9H=rain_nine_hour_accumulated*drought) %>%
  mutate(DIxR10H=rain_ten_hour_accumulated*drought) %>%
  mutate(DIxR11H=rain_elleven_hour_accumulated*drought) %>%
  mutate(DIxR12H=rain_twelve_hour_accumulated*drought) %>%
  mutate(DIxR13H=rain_thriteen_hour_accumulated*drought) %>%
  mutate(DIxR14H=rain_fourteen_hour_accumulated*drought) %>%
  mutate(DIxR15H=rain_fifteen_hour_accumulated*drought) %>%
  mutate(DIxR16H=rain_sixteen_hour_accumulated*drought) %>%
  mutate(DIxR17H=rain_seventeen_hour_accumulated*drought) %>%
  mutate(DIxR18H=rain_eighteen_hour_accumulated*drought) %>%
  mutate(DIxR19H=rain_nineteen_hour_accumulated*drought) %>%
  mutate(DIxR20H=rain_twenty_hour_accumulated*drought) %>%
  mutate(DIxR21H=rain_twentyone_hour_accumulated*drought) %>%
  mutate(DIxR22H=rain_tweentytwo_hour_accumulated*drought) %>%
  mutate(DIxR23H=rain_twentythree_hour_accumulated*drought) %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  select(-flow_effluent_m3_h)

validation_data <- model_data %>% 
  mutate(diff_NH_load=difference(ammonium_load_AN_kg_h)) %>% 
  mutate(lag1_diff_NH_load_AN=lag(diff_NH_load,1))%>% 
  mutate(lag2_diff_NH_load_AN=lag(diff_NH_load,2))%>% 
  mutate(lag3_diff_NH_load_AN=lag(diff_NH_load,3))%>% 
  mutate(lag4_diff_NH_load_AN=lag(diff_NH_load,4))%>% 
  mutate(lag5_diff_NH_load_AN=lag(diff_NH_load,5))%>%  
  filter_index("2022-01-01"~"2022-03-31") %>% 
  mutate(rain_elleven_hour_accumulated = rollapplyr(rainfall_mm, 
                                                    width=22, 
                                                    FUN=sum, 
                                                    partial=T)) %>%
  mutate(DIxR1H=rain_one_hour_accumulated*drought) %>% 
  mutate(DIxR1hH=rain_one_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR2H=rain_two_hour_accumulated*drought) %>%
  mutate(DIxR2hH=rain_two_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR3H=rain_three_hour_accumulated*drought) %>%
  mutate(DIxR3hH=rain_three_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR4H=rain_four_day_accumulated*drought) %>%
  mutate(DIxR4hH=rain_four_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR5H=rain_five_day_accumulated*drought) %>%
  mutate(DIxR5hH=rain_five_and_a_half_hour_accumulated*drought) %>%
  mutate(DIxR6H=rain_six_hour_accumulated*drought) %>%
  mutate(DIxR7H=rain_seven_hour_accumulated*drought) %>%
  mutate(DIxR8H=rain_eight_hour_accumulated*drought) %>%
  mutate(DIxR9H=rain_nine_hour_accumulated*drought) %>%
  mutate(DIxR10H=rain_ten_hour_accumulated*drought) %>%
  mutate(DIxR11H=rain_elleven_hour_accumulated*drought) %>%
  mutate(DIxR12H=rain_twelve_hour_accumulated*drought) %>%
  mutate(DIxR13H=rain_thriteen_hour_accumulated*drought) %>%
  mutate(DIxR14H=rain_fourteen_hour_accumulated*drought) %>%
  mutate(DIxR15H=rain_fifteen_hour_accumulated*drought) %>%
  mutate(DIxR16H=rain_sixteen_hour_accumulated*drought) %>%
  mutate(DIxR17H=rain_seventeen_hour_accumulated*drought) %>%
  mutate(DIxR18H=rain_eighteen_hour_accumulated*drought) %>%
  mutate(DIxR19H=rain_nineteen_hour_accumulated*drought) %>%
  mutate(DIxR20H=rain_twenty_hour_accumulated*drought) %>%
  mutate(DIxR21H=rain_twentyone_hour_accumulated*drought) %>%
  mutate(DIxR22H=rain_tweentytwo_hour_accumulated*drought) %>%
  mutate(DIxR23H=rain_twentythree_hour_accumulated*drought) %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  select(-flow_effluent_m3_h)


#specify the rain in the future
future_values <- validation_data %>% 
  fill_gaps()




#Reporting function
fit1_report <- function(df){
  df %>% report()
  
  print("AIC,BIC")  
  df %>% 
    glance() %>% 
    select(adj_r_squared,AIC,BIC,)
}

#Reporting function
fit2_report <- function(df){
  print("Training - RMSE,MAE,MAPE")
  df %>% 
    accuracy()  
}

#Reporting function
fit3_report <- function(df){
  
  print("Validataion - RMSE,MAE,MAPE")
  #Make a forecast
  fc <- forecast(df, 
                 new_data = future_values)
  accuracy(fc, validation_data) %>%
    select(.model, RMSE,MAE,MAPE)  
}

#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#Ammonium load in the AN tanks models
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////


#-------------------------------------------------------------------------------
#If you own a ammonium sensor and a flow sensor
#-------------------------------------------------------------------------------

#----------------------------------
#ARIMA_d_0
#----------------------------------
ARIMA_d_0 <- training_data %>%
  model(
    ARIMA(ammonium_load_AN_kg_h ~ PDQ(0, 0, 0) + pdq(d = 0))
  )


#Report the accuracy
ARIMA_d_0 %>% 
  fit1_report() 
ARIMA_d_0 %>% 
  fit2_report()
ARIMA_d_0 %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_d_0 %>% 
  saveRDS("ARIMA_d_0_two_years.rds")


#----------------------------------
#ARIMA_500
#----------------------------------
ARIMA_500 <- training_data %>%
  model(
    TSLM(ammonium_load_AN_kg_h ~ 1+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5)
  )

#Report the accuracy
ARIMA_500 %>% 
  fit1_report() 
ARIMA_500 %>% 
  fit2_report()
ARIMA_500 %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_500 %>% 
  saveRDS("ARIMA_5000_two_years.rds")


#----------------------------------
#Naive
#----------------------------------
NAIVE_model <- training_data %>% 
  model(
    NAIVE(ammonium_load_AN_kg_h)
  )

#Report the accuracy
NAIVE_model %>% 
  fit1_report() 
NAIVE_model %>% 
  fit2_report()
NAIVE_model %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
NAIVE_model %>% 
  saveRDS("NAIVE_model_two_years.rds")







#----------------------------------
#NNETAR_model!!!!!!!!!
#----------------------------------
set.seed(123)
NNETAR_model <- training_data %>% 
  model(
    NNETAR(ammonium_load_AN_kg_h,n_nodes = 5)
  )

#Report the accuracy
NNETAR_model %>% 
  fit1_report() 
NNETAR_model %>% 
  fit2_report()
NNETAR_model %>% 
  fit3_report()

fc <- forecast(NNETAR_model, 
               new_data = future_values)
accuracy(fc, validation_data) %>%
  select(.model, RMSE,MAE,MAPE) 


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
NNETAR_model %>% 
  saveRDS("NNETAR_model_two_years.rds")




#----------------------------------
#ARIMA_d_0_w_FT
#----------------------------------
ARIMA_d_0_w_FT <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ 0+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

#Report the accuracy
ARIMA_d_0_w_FT %>% 
  fit1_report() 
ARIMA_d_0_w_FT %>% 
  fit2_report()
ARIMA_d_0_w_FT %>% 
  fit3_report()


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_d_0_w_FT %>% 
  saveRDS("ARIMA_d_0_w_FT_two_years.rds")




#----------------------------------
#ARIMA_d_0_w_FT_large!!!!!!!
#----------------------------------
ARIMA_d_0_w_FT_large <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ 0+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           fourier(period = "day", K = 24) +
           fourier(period = "week", K = 15) +
           fourier(period = "year", K = 9))
  )

#Report the accuracy
ARIMA_d_0_w_FT_large %>% 
  fit1_report() 
ARIMA_d_0_w_FT_large %>% 
  fit2_report()
ARIMA_d_0_w_FT_large %>% 
  fit3_report()


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_d_0_w_FT_large %>% 
  saveRDS("ARIMA_d_0_w_FT_large_two_years.rds")



#----------------------------------

#----------------------------------
#ARIMA_48
#----------------------------------
ARIMA_48 <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           load_ammonium_AN_lag6+
           load_ammonium_AN_lag7+
           load_ammonium_AN_lag8+
           load_ammonium_AN_lag9+
           load_ammonium_AN_lag10+
           load_ammonium_AN_lag11+
           load_ammonium_AN_lag12+
           load_ammonium_AN_lag13+
           load_ammonium_AN_lag14+
           load_ammonium_AN_lag15+
           load_ammonium_AN_lag16+
           load_ammonium_AN_lag17+
           load_ammonium_AN_lag18+
           load_ammonium_AN_lag19+
           load_ammonium_AN_lag20+
           load_ammonium_AN_lag21+
           load_ammonium_AN_lag22+
           load_ammonium_AN_lag23+
           load_ammonium_AN_lag24+
           load_ammonium_AN_lag25+
           load_ammonium_AN_lag26+
           load_ammonium_AN_lag27+
           load_ammonium_AN_lag28+
           load_ammonium_AN_lag29+
           load_ammonium_AN_lag30+
           load_ammonium_AN_lag31+
           load_ammonium_AN_lag32+
           load_ammonium_AN_lag33+
           load_ammonium_AN_lag34+
           load_ammonium_AN_lag35+
           load_ammonium_AN_lag36+
           load_ammonium_AN_lag37+
           load_ammonium_AN_lag38+
           load_ammonium_AN_lag39+
           load_ammonium_AN_lag40+
           load_ammonium_AN_lag41+
           load_ammonium_AN_lag42+
           load_ammonium_AN_lag43+
           load_ammonium_AN_lag44+
           load_ammonium_AN_lag45+
           load_ammonium_AN_lag46+
           load_ammonium_AN_lag47+
           load_ammonium_AN_lag48)
  )

#Report the accuracy
ARIMA_48 %>% 
  fit1_report() 
ARIMA_48 %>% 
  fit2_report()
ARIMA_48 %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_48 %>% 
  saveRDS("ARIMA_48_two_years.rds")






#----------------------------------
#ARIMA_48_w_FT
#----------------------------------
ARIMA_48_w_FT <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           load_ammonium_AN_lag6+
           load_ammonium_AN_lag7+
           load_ammonium_AN_lag8+
           load_ammonium_AN_lag9+
           load_ammonium_AN_lag10+
           load_ammonium_AN_lag11+
           load_ammonium_AN_lag12+
           load_ammonium_AN_lag13+
           load_ammonium_AN_lag14+
           load_ammonium_AN_lag15+
           load_ammonium_AN_lag16+
           load_ammonium_AN_lag17+
           load_ammonium_AN_lag18+
           load_ammonium_AN_lag19+
           load_ammonium_AN_lag20+
           load_ammonium_AN_lag21+
           load_ammonium_AN_lag22+
           load_ammonium_AN_lag23+
           load_ammonium_AN_lag24+
           load_ammonium_AN_lag25+
           load_ammonium_AN_lag26+
           load_ammonium_AN_lag27+
           load_ammonium_AN_lag28+
           load_ammonium_AN_lag29+
           load_ammonium_AN_lag30+
           load_ammonium_AN_lag31+
           load_ammonium_AN_lag32+
           load_ammonium_AN_lag33+
           load_ammonium_AN_lag34+
           load_ammonium_AN_lag35+
           load_ammonium_AN_lag36+
           load_ammonium_AN_lag37+
           load_ammonium_AN_lag38+
           load_ammonium_AN_lag39+
           load_ammonium_AN_lag40+
           load_ammonium_AN_lag41+
           load_ammonium_AN_lag42+
           load_ammonium_AN_lag43+
           load_ammonium_AN_lag44+
           load_ammonium_AN_lag45+
           load_ammonium_AN_lag46+
           load_ammonium_AN_lag47+
           load_ammonium_AN_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )


#Report the accuracy
ARIMA_48_w_FT %>% 
  fit1_report() 
ARIMA_48_w_FT %>% 
  fit2_report()
ARIMA_48_w_FT %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_48_w_FT %>% 
  saveRDS("ARIMA_48_w_FT_two_years.rds")




#----------------------------------
#ARIMA_48_w_FT_large!!!!
#----------------------------------
ARIMA_48_w_FT_large <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           load_ammonium_AN_lag6+
           load_ammonium_AN_lag7+
           load_ammonium_AN_lag8+
           load_ammonium_AN_lag9+
           load_ammonium_AN_lag10+
           load_ammonium_AN_lag11+
           load_ammonium_AN_lag12+
           load_ammonium_AN_lag13+
           load_ammonium_AN_lag14+
           load_ammonium_AN_lag15+
           load_ammonium_AN_lag16+
           load_ammonium_AN_lag17+
           load_ammonium_AN_lag18+
           load_ammonium_AN_lag19+
           load_ammonium_AN_lag20+
           load_ammonium_AN_lag21+
           load_ammonium_AN_lag22+
           load_ammonium_AN_lag23+
           load_ammonium_AN_lag24+
           load_ammonium_AN_lag25+
           load_ammonium_AN_lag26+
           load_ammonium_AN_lag27+
           load_ammonium_AN_lag28+
           load_ammonium_AN_lag29+
           load_ammonium_AN_lag30+
           load_ammonium_AN_lag31+
           load_ammonium_AN_lag32+
           load_ammonium_AN_lag33+
           load_ammonium_AN_lag34+
           load_ammonium_AN_lag35+
           load_ammonium_AN_lag36+
           load_ammonium_AN_lag37+
           load_ammonium_AN_lag38+
           load_ammonium_AN_lag39+
           load_ammonium_AN_lag40+
           load_ammonium_AN_lag41+
           load_ammonium_AN_lag42+
           load_ammonium_AN_lag43+
           load_ammonium_AN_lag44+
           load_ammonium_AN_lag45+
           load_ammonium_AN_lag46+
           load_ammonium_AN_lag47+
           load_ammonium_AN_lag48+
           fourier(period = "day", K = 20) +
           fourier(period = "week", K = 15) +
           fourier(period = "year", K = 9))
  )

#Report the accuracy
ARIMA_48_w_FT_large %>% 
  fit1_report() 
ARIMA_48_w_FT_large %>% 
  fit2_report()
ARIMA_48_w_FT_large %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
ARIMA_48_w_FT_large %>% 
  saveRDS("ARIMA_48_w_FT_large_two_years.rds")



#----------------------------------
#FT
#----------------------------------
FT <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
FT %>% 
  saveRDS("FT_two_years.rds")


#Report the accuracy
FT %>% 
  fit1_report() 
FT %>% 
  fit2_report()
FT %>% 
  fit3_report()


#----------------------------------
#FT_large
#----------------------------------
FT_large <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h ~ fourier(period = "day", K = 20) +
           fourier(period = "week", K = 15) +
           fourier(period = "year", K = 9))
  )

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
FT_large %>% 
  saveRDS("FT_large_two_years.rds")


#Report the accuracy
FT_large %>% 
  fit1_report() 
FT_large %>% 
  fit2_report()
FT_large %>% 
  fit3_report()



#-------------------------------------------------------------------------------
#If you only know the rain precipitation 
#-------------------------------------------------------------------------------



#----------------------------------
#TSLM(R)
#----------------------------------
TSLM_R <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm)
  )


#Report the accuracy
TSLM_R %>% 
  fit1_report() 
TSLM_R %>% 
  fit2_report()
TSLM_R %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R %>% 
  saveRDS("TSLM_R_two_years.rds")





#----------------------------------
#TSLM_R_R1_R2_R3_R4_R5_R6_R7
#----------------------------------
TSLM_R_R1_R2_R3_R4_R5_R6_R7 <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated)
  )

#Report the accuracy
TSLM_R_R1_R2_R3_R4_R5_R6_R7 %>% 
  fit1_report() 
TSLM_R_R1_R2_R3_R4_R5_R6_R7 %>% 
  fit2_report()
TSLM_R_R1_R2_R3_R4_R5_R6_R7 %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_R1_R2_R3_R4_R5_R6_R7 %>% 
  saveRDS("TSLM_R_R1_R2_R3_R4_R5_R6_R7_two_years.rds")



#----------------------------------
#TSLM_R_AllLagR_AllAccumR
#----------------------------------
TSLM_R_AllLagR_AllAccumR <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48)
  )


#Report the accuracy
TSLM_R_AllLagR_AllAccumR %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_two_years.rds")



#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_two_years.rds")



#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_DI
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT_DI <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3)+
           drought)
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT_DI %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT_DI %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT_DI %>% 
  fit3_report()


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT_DI %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_DI_two_years.rds")




#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3)+
           drought+
           DIxR1H+
           DIxR1hH+
           DIxR2H+
           DIxR2hH+
           DIxR3H+
           DIxR3hH+
           DIxR4H+
           DIxR4hH+
           DIxR5H+
           DIxR5hH+
           DIxR6H+
           DIxR7H+
           DIxR8H+
           DIxR9H+
           DIxR10H+
           DIxR11H+
           DIxR12H+
           DIxR13H+
           DIxR14H+
           DIxR15H+
           DIxR16H+
           DIxR17H+
           DIxR18H+
           DIxR19H+
           DIxR20H+
           DIxR21H+
           DIxR22H+
           DIxR23H+
           DIxR1)
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR_two_years.rds")




#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxR_w_ARIMA(5,0,0)_w_mean_errors_ONLY_AS_TSLM
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~1+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3)+
           drought+
           DIxR1H+
           DIxR1hH+
           DIxR2H+
           DIxR2hH+
           DIxR3H+
           DIxR3hH+
           DIxR4H+
           DIxR4hH+
           DIxR5H+
           DIxR5hH+
           DIxR6H+
           DIxR7H+
           DIxR8H+
           DIxR9H+
           DIxR10H+
           DIxR11H+
           DIxR12H+
           DIxR13H+
           DIxR14H+
           DIxR15H+
           DIxR16H+
           DIxR17H+
           DIxR18H+
           DIxR19H+
           DIxR20H+
           DIxR21H+
           DIxR22H+
           DIxR23H+
           DIxR1)
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_500_w_mean_errors_ONLY_AS_TSLM_two_years.rds")






#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~1+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           load_ammonium_AN_lag6+
           load_ammonium_AN_lag7+
           load_ammonium_AN_lag8+
           load_ammonium_AN_lag9+
           load_ammonium_AN_lag10+
           load_ammonium_AN_lag11+
           load_ammonium_AN_lag12+
           load_ammonium_AN_lag13+
           load_ammonium_AN_lag14+
           load_ammonium_AN_lag15+
           load_ammonium_AN_lag16+
           load_ammonium_AN_lag17+
           load_ammonium_AN_lag18+
           load_ammonium_AN_lag19+
           load_ammonium_AN_lag20+
           load_ammonium_AN_lag21+
           load_ammonium_AN_lag22+
           load_ammonium_AN_lag23+
           load_ammonium_AN_lag24+
           load_ammonium_AN_lag25+
           load_ammonium_AN_lag26+
           load_ammonium_AN_lag27+
           load_ammonium_AN_lag28+
           load_ammonium_AN_lag29+
           load_ammonium_AN_lag30+
           load_ammonium_AN_lag31+
           load_ammonium_AN_lag32+
           load_ammonium_AN_lag33+
           load_ammonium_AN_lag34+
           load_ammonium_AN_lag35+
           load_ammonium_AN_lag36+
           load_ammonium_AN_lag37+
           load_ammonium_AN_lag38+
           load_ammonium_AN_lag39+
           load_ammonium_AN_lag40+
           load_ammonium_AN_lag41+
           load_ammonium_AN_lag42+
           load_ammonium_AN_lag43+
           load_ammonium_AN_lag44+
           load_ammonium_AN_lag45+
           load_ammonium_AN_lag46+
           load_ammonium_AN_lag47+
           load_ammonium_AN_lag48+
           rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3)+
           drought+
           DIxR1H+
           DIxR1hH+
           DIxR2H+
           DIxR2hH+
           DIxR3H+
           DIxR3hH+
           DIxR4H+
           DIxR4hH+
           DIxR5H+
           DIxR5hH+
           DIxR6H+
           DIxR7H+
           DIxR8H+
           DIxR9H+
           DIxR10H+
           DIxR11H+
           DIxR12H+
           DIxR13H+
           DIxR14H+
           DIxR15H+
           DIxR16H+
           DIxR17H+
           DIxR18H+
           DIxR19H+
           DIxR20H+
           DIxR21H+
           DIxR22H+
           DIxR23H+
           DIxR1)
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_DI_w_AllDIxRw_ARIMA_48_w_mean_errors_two_years.rds")





#----------------------------------
#TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors
#----------------------------------
TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors <- training_data %>% 
  model(
    TSLM(ammonium_load_AN_kg_h~1+
           load_ammonium_AN_lag1+
           load_ammonium_AN_lag2+
           load_ammonium_AN_lag3+
           load_ammonium_AN_lag4+
           load_ammonium_AN_lag5+
           load_ammonium_AN_lag6+
           load_ammonium_AN_lag7+
           load_ammonium_AN_lag8+
           load_ammonium_AN_lag9+
           load_ammonium_AN_lag10+
           load_ammonium_AN_lag11+
           load_ammonium_AN_lag12+
           load_ammonium_AN_lag13+
           load_ammonium_AN_lag14+
           load_ammonium_AN_lag15+
           load_ammonium_AN_lag16+
           load_ammonium_AN_lag17+
           load_ammonium_AN_lag18+
           load_ammonium_AN_lag19+
           load_ammonium_AN_lag20+
           load_ammonium_AN_lag21+
           load_ammonium_AN_lag22+
           load_ammonium_AN_lag23+
           load_ammonium_AN_lag24+
           load_ammonium_AN_lag25+
           load_ammonium_AN_lag26+
           load_ammonium_AN_lag27+
           load_ammonium_AN_lag28+
           load_ammonium_AN_lag29+
           load_ammonium_AN_lag30+
           load_ammonium_AN_lag31+
           load_ammonium_AN_lag32+
           load_ammonium_AN_lag33+
           load_ammonium_AN_lag34+
           load_ammonium_AN_lag35+
           load_ammonium_AN_lag36+
           load_ammonium_AN_lag37+
           load_ammonium_AN_lag38+
           load_ammonium_AN_lag39+
           load_ammonium_AN_lag40+
           load_ammonium_AN_lag41+
           load_ammonium_AN_lag42+
           load_ammonium_AN_lag43+
           load_ammonium_AN_lag44+
           load_ammonium_AN_lag45+
           load_ammonium_AN_lag46+
           load_ammonium_AN_lag47+
           load_ammonium_AN_lag48+
           rainfall_mm+
           rain_one_hour_accumulated+
           rain_one_and_a_half_hour_accumulated+
           rain_two_hour_accumulated+
           rain_two_and_a_half_hour_accumulated+
           rain_three_hour_accumulated+
           rain_three_and_a_half_hour_accumulated+
           rain_four_day_accumulated+
           rain_four_and_a_half_hour_accumulated+
           rain_five_day_accumulated+
           rain_five_and_a_half_hour_accumulated+
           rain_six_hour_accumulated+
           rain_seven_hour_accumulated+
           rain_eight_hour_accumulated+
           rain_nine_hour_accumulated+
           rain_ten_hour_accumulated+
           rain_elleven_hour_accumulated+
           rain_twelve_hour_accumulated+
           rain_thriteen_hour_accumulated+
           rain_fourteen_hour_accumulated+
           rain_fifteen_hour_accumulated+
           rain_sixteen_hour_accumulated+
           rain_seventeen_hour_accumulated+
           rain_eighteen_hour_accumulated+
           rain_nineteen_hour_accumulated+
           rain_twenty_hour_accumulated+
           rain_twentyone_hour_accumulated+
           rain_tweentytwo_hour_accumulated+
           rain_twentythree_hour_accumulated+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           rain_lag1+
           rain_lag2+
           rain_lag3+
           rain_lag4+
           rain_lag5+
           rain_lag6+
           rain_lag7+
           rain_lag8+
           rain_lag9+
           rain_lag10+
           rain_lag11+
           rain_lag12+
           rain_lag13+
           rain_lag14+
           rain_lag15+
           rain_lag16+
           rain_lag17+
           rain_lag18+
           rain_lag19+
           rain_lag20+
           rain_lag21+
           rain_lag22+
           rain_lag23+
           rain_lag24+
           rain_lag25+
           rain_lag26+
           rain_lag27+
           rain_lag28+
           rain_lag29+
           rain_lag30+
           rain_lag31+
           rain_lag32+
           rain_lag33+
           rain_lag34+
           rain_lag35+
           rain_lag36+
           rain_lag37+
           rain_lag38+
           rain_lag39+
           rain_lag40+
           rain_lag41+
           rain_lag42+
           rain_lag43+
           rain_lag44+
           rain_lag45+
           rain_lag46+
           rain_lag47+
           rain_lag48+
           fourier(period = "day", K = 24) +
           fourier(period = "week", K = 15) +
           fourier(period = "year", K = 9)+
           drought+
           DIxR1H+
           DIxR1hH+
           DIxR2H+
           DIxR2hH+
           DIxR3H+
           DIxR3hH+
           DIxR4H+
           DIxR4hH+
           DIxR5H+
           DIxR5hH+
           DIxR6H+
           DIxR7H+
           DIxR8H+
           DIxR9H+
           DIxR10H+
           DIxR11H+
           DIxR12H+
           DIxR13H+
           DIxR14H+
           DIxR15H+
           DIxR16H+
           DIxR17H+
           DIxR18H+
           DIxR19H+
           DIxR20H+
           DIxR21H+
           DIxR22H+
           DIxR23H+
           DIxR1)
  )

#Report the accuracy
TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit1_report() 
TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit2_report()
TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  fit3_report()

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models/two_year_of_training_data")
#Save the model 
TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors %>% 
  saveRDS("TSLM_R_AllLagR_AllAccumR_w_FT_large_DI_w_AllDIxRw_ARIMA_48_w_mean_errors_two_years.rds")








