#Plotting the accuracy



model_parameter = "AC_0_to_1000_accumulated_ammonium_load"










#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code")
forecasting_steps = seq(2,48)
iteration_steps = c(2,3,4,5,10)



assign(paste("forecast",1,"step_accuracy_step",1,model_parameter, sep = "_"), 
       read_csv(paste(1,"_step_forecast_", model_parameter,"_iteration_step_",1,".csv", sep="")))

for(j in iteration_steps){
  
  assign(paste("forecast",1,"step_accuracy_step",j,model_parameter, sep = "_"), 
         read_csv(paste(1,"_step_forecast_", model_parameter,"_iteration_step_",j,".csv", sep="")))
}

for (i in forecasting_steps){
  for(j in iteration_steps){
    
    
    assign(paste("forecast_step",i,"accuracy_step",j,model_parameter, sep = "_"), 
           read_csv(paste(i,"_step_forecast_", model_parameter,"_iteration_step_",j,".csv", sep="")))
  
    }  
}

df_list <- assign(paste("forecast_step",2,"accuracy_step",j,model_parameter, sep = "_"))
counter <- 1
for (j in iteration_steps){
  df_list[counter] <- list(assign(paste("forecast_step",2,"accuracy_step",j,model_parameter, sep = "_"))) 
  counter <- counter+1
  assign(paste("combined_forecast_step",2,"accuracy_step",model_parameter, sep = "_",
               reduce(full_join, by='Model_terms')))
    
}


#Plot the 48 models number of features
 for (i in forecasting_steps){
   
   assign(paste("Plot_of_model_at_forecasting_step",i,sep="_")) <- forecast_10_step_accuracy_step_10_AC_0_to_1000_accumulated_ammonium_load
 }



colnames(fourty_egith_step_accuracy_step_2) <- paste("step_2_step_48", colnames(fourty_egith_step_accuracy_step_2), sep = "_") 
fourty_egith_step_accuracy_step_2 <- fourty_egith_step_accuracy_step_2%>% 
  rename(model_terms=step_2_step_48_Model_terms)



colnames(one_step_accuracy_step_1) <- paste("step_1", colnames(one_step_accuracy_step_1), sep = "_") 
one_step_accuracy_step_1 <- one_step_accuracy_step_1%>% 
  rename(model_terms=step_1_Model_terms)

colnames(one_step_accuracy_step_2) <- paste("step_2", colnames(one_step_accuracy_step_2), sep = "_") 
one_step_accuracy_step_2 <- one_step_accuracy_step_2%>% 
  rename(model_terms=step_2_Model_terms)

colnames(one_step_accuracy_step_3) <- paste("step_3", colnames(one_step_accuracy_step_3), sep = "_") 
one_step_accuracy_step_3 <- one_step_accuracy_step_3%>% 
  rename(model_terms=step_3_Model_terms)

colnames(one_step_accuracy_step_4) <- paste("step_4", colnames(one_step_accuracy_step_4), sep = "_") 
one_step_accuracy_step_4 <- one_step_accuracy_step_4%>% 
  rename(model_terms=step_4_Model_terms)

colnames(one_step_accuracy_step_5) <- paste("step_5", colnames(one_step_accuracy_step_5), sep = "_") 
one_step_accuracy_step_5 <- one_step_accuracy_step_5%>% 
  rename(model_terms=step_5_Model_terms)

colnames(one_step_accuracy_step_10) <- paste("step_10", colnames(one_step_accuracy_step_10), sep = "_") 
one_step_accuracy_step_10 <- one_step_accuracy_step_10%>% 
  rename(model_terms=step_10_Model_terms)

accuracy <- one_step_accuracy_step_1 %>% 
  left_join((one_step_accuracy_step_2))

accuracy <- accuracy %>% 
  left_join((one_step_accuracy_step_3))

accuracy <- accuracy %>% 
  left_join((one_step_accuracy_step_4))

accuracy <- accuracy %>% 
  left_join((one_step_accuracy_step_5))

accuracy <- accuracy %>% 
  left_join((one_step_accuracy_step_10))




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_r2_training_set))+
  geom_point(aes(y=step_2_r2_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_r2_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_r2_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_r2_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_r2_training_set), color="Purple", na.rm = T)+
  theme_malte()



accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_mae_training_set))+
  geom_point(aes(y=step_2_mae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_mae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_mae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_mae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_mae_training_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_r2_valid_set))+
  geom_point(aes(y=step_2_r2_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_r2_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_r2_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_r2_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_r2_valid_set), color="Purple", na.rm = T)+
  theme_malte()



accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_mae_valid_set))+
  geom_point(aes(y=step_2_mae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_mae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_mae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_mae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_mae_valid_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_rmse_training_set))+
  geom_point(aes(y=step_2_rmse_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_rmse_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_rmse_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_rmse_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_rmse_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_rmse_valid_set))+
  geom_point(aes(y=step_2_rmse_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_rmse_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_rmse_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_rmse_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_rmse_valid_set), color="Purple", na.rm = T)+
  theme_malte()




accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_median_ae_training_set))+
  geom_point(aes(y=step_2_median_ae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_median_ae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_median_ae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_median_ae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_median_ae_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_median_ae_valid_set))+
  geom_point(aes(y=step_2_median_ae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_median_ae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_median_ae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_median_ae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_median_ae_valid_set), color="Purple", na.rm = T)+
  theme_malte()



accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_max_ae_training_set))+
  geom_point(aes(y=step_2_max_ae_training_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_max_ae_training_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_max_ae_training_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_max_ae_training_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_max_ae_training_set), color="Purple", na.rm = T)+
  theme_malte()

accuracy %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_1_max_ae_valid_set))+
  geom_point(aes(y=step_2_max_ae_valid_set), color="Red", na.rm = F)+
  geom_point(aes(y=step_3_max_ae_valid_set), color="Yellow", na.rm = T)+
  geom_point(aes(y=step_4_max_ae_valid_set), color="Green", na.rm = T)+
  geom_point(aes(y=step_5_max_ae_valid_set), color="Blue", na.rm = T)+
  geom_point(aes(y=step_10_max_ae_valid_set), color="Purple", na.rm = T)+
  theme_malte()


temptemp <- paste("af","fsd",6)

temp <- left_join(one_step_accuracy_step_2,fourty_egith_step_accuracy_step_2)


temp %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_2_mae_valid_set))+
  geom_line(aes(y=step_2_step_48_mae_valid_set), color="Red")+
  theme_malte()



temp %>%  
  ggplot(aes(x=model_terms))+
  geom_line(aes(y=step_2_max_ae_valid_set))+
  geom_line(aes(y=step_2_step_48_max_ae_valid_set), color="Red")+
  theme_malte()


model_data %>% 
  select(DO_PT1_mg_L) %>% autoplot()


model_data %>% 
  select(DO_PT4_mg_L) %>% autoplot()


























# one_step_accuracy_step_1 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_1.csv")
# 
# #choosing the the working directory to be here
# setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Python_code")
# one_step_accuracy_step_1 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_1.csv")
# one_step_accuracy_step_2 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_2.csv")
# one_step_accuracy_step_3 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_3.csv")
# one_step_accuracy_step_4 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_4.csv")
# one_step_accuracy_step_5 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_5.csv")
# one_step_accuracy_step_10 <- read_csv("1_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_10.csv")
# 
# 
# 
# fourty_egith_step_accuracy_step_2 <- read_csv("48_step_forecast_AC_0_to_1000_accumulated_ammonium_load_iteration_step_2.csv")
