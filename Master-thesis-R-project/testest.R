


#Load the models
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/ARIMA_and_linear_models/saved_models")
#Save the model 
ARIMA_48_test3 <- readRDS("ARIMA_d_0_w_FT.rds")


#Report the accuracy
ARIMA_48_test3 %>% 
  fit1_report() 
ARIMA_48_test3 %>% 
  fit2_report()
ARIMA_48_test3 %>% 
  fit3_report()
fc <- forecast(ARIMA_48_test3, 
               h=10)
accuracy(fc, validation_data) %>%
  select(.model, RMSE,MAE,MAPE)  