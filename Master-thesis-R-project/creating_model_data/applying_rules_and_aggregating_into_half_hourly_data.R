#///////////////////////////////////////////////////////////////////////////////
#Data cleaning - creating model data
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script to visualize the difference time series of flow
#ammonium concentration, temperature and their difference in the values.

#Setup the data
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

#Flow and diff flow
#-------------------------------------------------------------------------------
#Apply the flow rule which are:
#All flow which are zero
#All difference flow which are greater than +-250 


#Count the NA values before all the removal in the half hourly data
NA_counter_before_removal <- data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  filter(is.na(flow_AN_m3_h))

#Plot the time series
data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()

#Applying the rules to the one minute data frame
model_data_one_minut <- data_one_min %>% 
  select(flow_AN_m3_h, diff_flow_AN) %>% 
  filter(flow_AN_m3_h>0) %>% 
  filter(diff_flow_AN<250) %>% 
  filter(diff_flow_AN>-250)

#Aggregating all one minute data frame to a half hourly data frame
model_data_half_hourly <- aggregate_into_half_hourly_model_data(model_data_one_minut,
                                        flow_AN_m3_h)

#Counting the NA values after the aggregation
NA_counter_after_removal <- model_data_half_hourly %>% 
  select(flow_AN_m3_h) %>% 
  filter(is.na(flow_AN_m3_h))
         
#Plot the time series
model_data_half_hourly %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()



#Ammonium concentration and difference in ammonium concentration
#-------------------------------------------------------------------------------
#Apply the flow rule which are:


data_one_min <- data_one_min %>% 
  mutate(lag_ammonium=lag(ammonium_to_AN_mg_L))%>% 
  mutate(lag_diff_ammonium=difference(lag_ammonium))
#Count the NA values before all the removal in the half hourly data
NA_counter_before_removal <- data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(is.na(ammonium_to_AN_mg_L))

#Plot the time series
data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>%  
  filter_index("2021-05") %>%
  filter(ammonium_to_AN_mg_L<80) %>% 
  autoplot()

data_thirty_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(ammonium_to_AN_mg_L<100) %>% 
  autoplot()

#Applying the rules to the one minute data frame
model_data_one_minut <- data_one_min %>% 
  select(ammonium_to_AN_mg_L, 
         diff_ammonium_AN) %>% 
  filter(ammonium_to_AN_mg_L>0) %>% 
  filter(ammonium_to_AN_mg_L<80) %>% 
  filter(diff_ammonium_AN<1) %>% 
  filter(diff_ammonium_AN>-1)%>% 
  filter(lag_diff_ammonium<1) %>% 
  filter(lag_diff_ammonium>-1)

#Aggregating all one minute data frame to a half hourly data frame
temp_model_data <- aggregate_into_half_hourly_model_data(model_data_one_minut,
                                                         ammonium_to_AN_mg_L)

#Counting the NA values after the aggregation
NA_counter_after_removal <- temp_model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter(is.na(ammonium_to_AN_mg_L))

#Plot the time series
temp_model_data %>% 
  select(ammonium_to_AN_mg_L) %>% 
  filter_index("2021-05") %>% 
  autoplot()

#Combine into one data frame 

temp_model_data <- temp_model_data %>% 
  mutate(diff_ammonium_AN=difference(ammonium_to_AN_mg_L))

temp_model_data %>% 
  select(diff_ammonium_AN) %>%
  autoplot()

temp_model_data %>% 
  select(diff_ammonium_AN) %>% 
  filter_index("2021-05") %>% 
  autoplot()

value <- c(1,2,5)
temp <- as.data.frame(value) %>% 
  mutate(lag=lag(value)) %>% 
  mutate(diff=difference(value))
  

#-------------------------------------------------------------------------------
#Apply the flow rule which are:

#Count the NA values before all the removal in the half hourly data

#Plot the time series

#Applying the rules to the one minute data frame

#Aggregating all one minute data frame to a half hourly data frame

#Counting the NA values after the aggregation

#Plot the time series


#-------------------------------------------------------------------------------
#Apply the flow rule which are:

#Count the NA values before all the removal in the half hourly data

#Plot the time series

#Applying the rules to the one minute data frame

#Aggregating all one minute data frame to a half hourly data frame

#Counting the NA values after the aggregation

#Plot the time series

