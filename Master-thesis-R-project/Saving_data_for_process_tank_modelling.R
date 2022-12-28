library(zoo)
library(tidyverse)
library(fpp3)
#Cleaning up the data, so it can be used for process tank modelling 
#set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data")

#read the data from a csv file 
model_data <- read_csv("model_data.csv") %>% 
  #convert the date time to a right time format
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  #convert the data frame to a tsibble
  as_tsibble()

model_data <- model_data %>% 
  select(time_thirty_min,
         rainfall_mm,
         flow_AN_m3_h,
         flow_effluent_m3_h,
         flow_influent_m3_h,
         flow_HT_m3_h,
         ammonium_effluent_mg_L,
         ammonium_PT1_mg_L,
         ammonium_PT2_mg_L,
         ammonium_PT3_mg_L,
         ammonium_PT4_mg_L,
         ammonium_to_AN_mg_L,
         ammonium_load_AN_kg_h,
         ammonium_load_PT4_kg_h,
         ammonium_load_PT3_kg_h,
         ammonium_load_PT2_kg_h,
         ammonium_load_PT1_kg_h,
         ammonium_load_effluent_kg_h,
         T_PT1_C,
         T_PT2_C,
         T_PT3_C,
         T_PT4_C,
         airflow_PT1_m3_h,
         airflow_PT2_m3_h,
         airflow_PT3_m3_h,
         airflow_PT4_m3_h,
         DO_PT1_mg_L,
         DO_PT2_mg_L,
         DO_PT3_mg_L,
         DO_PT4_mg_L,
         SS_PT1_g_L,
         SS_PT4_g_L,
         drought,
         nitrate_PT4_mg_L,
         nitrate_PT3_mg_L,
         nitrate_PT2_mg_L,
         nitrate_PT1_mg_L,
         nitrate_effluent_mg_L,
         nitrate_load_PT4_kg_h,
         nitrate_load_PT3_kg_h,
         nitrate_load_PT2_kg_h,
         nitrate_load_PT1_kg_h,
         nitrate_load_effluent_kg_h)

total_na=0
for (i in seq(from = 1, to = 43, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na = total_na+nrow(temp)
  
  print("# NA in")
  print(names(training_data[,i]))
  print(nrow(temp))
}



#Process tank 1
model_data$airflow_PT1_m3_h[model_data$airflow_PT1_m3_h>4000] <- NA


#Process tank 2
model_data$airflow_PT2_m3_h[model_data$airflow_PT2_m3_h>4000] <- NA


#Process tank 3
model_data$airflow_PT3_m3_h[model_data$airflow_PT3_m3_h>4000] <- NA


#Process tank 4
model_data$airflow_PT4_m3_h[model_data$airflow_PT4_m3_h>4000] <- NA


#DO in process tank 1
model_data$DO_PT1_mg_L[model_data$DO_PT1_mg_L>5] <- NA


#DO in process tank 2
model_data$DO_PT2_mg_L[model_data$DO_PT2_mg_L>5] <- NA

#DO in process tank 3
model_data$DO_PT3_mg_L[model_data$DO_PT3_mg_L>5] <- NA


#DO in process tank 4
model_data$DO_PT4_mg_L[model_data$DO_PT4_mg_L>5] <- NA

total_na2=0
for (i in seq(from = 1, to = 43, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na2 = total_na+nrow(temp)
  
  print("# NA in")
  print(names(training_data[,i]))
  print(nrow(temp))
}
model_data <- model_data %>% 
  filter_index("2018-01-29"~.) %>%   
  mutate(flow_AN_m3_h=na.approx(flow_AN_m3_h)) %>%
  mutate(flow_effluent_m3_h=na.approx(flow_effluent_m3_h)) %>%    
  mutate(flow_influent_m3_h=na.approx(flow_influent_m3_h)) %>%    
  mutate(flow_HT_m3_h=na.approx(flow_HT_m3_h)) %>%    
  mutate(ammonium_effluent_mg_L=na.approx(ammonium_effluent_mg_L)) %>%    
  mutate(ammonium_PT1_mg_L=na.approx(ammonium_PT1_mg_L)) %>%    
  mutate(ammonium_PT2_mg_L=na.approx(ammonium_PT2_mg_L)) %>%    
  mutate(ammonium_PT3_mg_L=na.approx( ammonium_PT3_mg_L)) %>%    
  mutate(ammonium_PT4_mg_L=na.approx(ammonium_PT4_mg_L)) %>%    
  mutate(ammonium_to_AN_mg_L=na.approx(ammonium_to_AN_mg_L)) %>%    
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h)) %>%    
  mutate(ammonium_load_PT4_kg_h=na.approx(ammonium_load_PT4_kg_h)) %>%    
  mutate(ammonium_load_PT3_kg_h=na.approx(ammonium_load_PT3_kg_h))%>%    
  mutate(ammonium_load_PT2_kg_h=na.approx(ammonium_load_PT2_kg_h)) %>%    
  mutate(ammonium_load_PT1_kg_h=na.approx(ammonium_load_PT1_kg_h)) %>%    
  mutate(ammonium_load_effluent_kg_h=na.approx(ammonium_load_effluent_kg_h)) %>%    
  mutate(T_PT1_C=na.approx(T_PT1_C)) %>%    
  mutate(T_PT2_C=na.approx(T_PT2_C)) %>%    
  mutate(T_PT3_C=na.approx(T_PT3_C)) %>%    
  mutate(T_PT4_C=na.approx(T_PT4_C)) %>%    
  mutate(airflow_PT1_m3_h=na.approx(airflow_PT1_m3_h)) %>%    
  mutate(airflow_PT2_m3_h=na.approx(airflow_PT2_m3_h)) %>%    
  mutate(airflow_PT3_m3_h=na.approx(airflow_PT3_m3_h)) %>%    
  mutate(airflow_PT4_m3_h=na.approx(airflow_PT4_m3_h)) %>%    
  mutate(DO_PT1_mg_L=na.approx(DO_PT1_mg_L)) %>%    
  mutate(DO_PT2_mg_L=na.approx(DO_PT2_mg_L)) %>%    
  mutate(DO_PT3_mg_L=na.approx(DO_PT3_mg_L)) %>%    
  mutate(DO_PT4_mg_L=na.approx(DO_PT4_mg_L)) %>%    
  mutate(SS_PT1_g_L=na.approx(SS_PT1_g_L)) %>%    
  mutate(SS_PT4_g_L=na.approx(SS_PT4_g_L)) %>%   
  mutate(nitrate_PT4_mg_L=na.approx(nitrate_PT4_mg_L)) %>%    
  mutate(nitrate_PT3_mg_L=na.approx(nitrate_PT3_mg_L)) %>%    
  mutate(nitrate_PT2_mg_L=na.approx(nitrate_PT2_mg_L)) %>%    
  mutate(nitrate_PT1_mg_L=na.approx(nitrate_PT1_mg_L)) %>%    
  mutate(nitrate_effluent_mg_L=na.approx(nitrate_effluent_mg_L)) %>%    
  mutate(nitrate_load_PT4_kg_h=na.approx(nitrate_load_PT4_kg_h)) %>%    
  mutate(nitrate_load_PT3_kg_h=na.approx(nitrate_load_PT3_kg_h)) %>%    
  mutate(nitrate_load_PT2_kg_h=na.approx(nitrate_load_PT2_kg_h)) %>%    
  mutate(nitrate_load_PT1_kg_h=na.approx(nitrate_load_PT1_kg_h)) %>%    
  mutate(nitrate_load_effluent_kg_h=na.approx(nitrate_load_effluent_kg_h)) %>% 
  mutate(month=month(time_thirty_min))
  
  
  
total_na3=0
for (i in seq(from = 1, to = 43, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na3 = total_na3+nrow(temp)
  
  print("# NA in")
  print(names(model_data[,i]))
  print(nrow(temp))
}

wd <- "C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project"

#Making a list of all the csv file names that should be used
list_of_file_DMI_drought_data <- list.files(paste(wd,
                                                  "/DMI_drought_data_extra",
                                                  sep=""))

#Defining the first month in the list of month manually 
data_DMI_drought <- data.frame()

#Resetting the counter i to 0
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_file_DMI_drought_data){
  #Loading and pre-processing the data
  df1 <- read_delim(paste(wd,
                          "/DMI_drought_data_extra/",
                          file,
                          sep=""), 
                    delim=";",
                    col_types = cols(.default = "c"))
  
  #Skip the first manually assigned file 
  data_DMI_drought <- bind_rows(df1,
                                data_DMI_drought)
  #Add one to the counter
  i=i+1  
  
}

#Creating a data frame on the data extracted from DMI
data_DMI_drought <- data_DMI_drought %>% 
  #Renaming the columns
  rename(day=DateTime, 
         drought2="Tørkeindex") %>% 
  #Converting the time column to the correct format
  mutate(day=as.Date(day)) %>%
  #Replacing comma with dot and converting to numeric
  mutate(drought2= as.numeric(gsub(",", 
                                   ".",  
                                   as.character(drought2)))) %>% 
  #Removing duplicates based on time
  distinct(day,
           .keep_all = T) %>% 
  #Convert to tibble
  as_tibble()

#Adding the drought data to the data frame
model_data <- model_data %>% 
  #Creating a column with time in a day format
  mutate(day=as.Date(time_thirty_min)) %>% 
  #Joining drought index to the data frame
  left_join(data_DMI_drought)  %>%  
  #Replace the NA values in the rain column and leave values which is not NA
  mutate(drought=if_else(is.na(drought), 
                         drought2, 
                         drought))%>% 
  #Removing the day time column 
  select(-day) %>% 
  select(-drought2)

total_na4=0
for (i in seq(from = 1, to = 43, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na4 = total_na4+nrow(temp)
  
  print("# NA in")
  print(names(model_data[,i]))
  print(nrow(temp))
}


training_data <- model_data %>% 
  filter_index("2018-01-29"~"2021-12-31")

valid_data <- model_data %>% 
  filter_index("2022-01"~"2022-03")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB")

#Save the data as a csv file
write_csv(training_data, 
          "training_data_for_process_tanks.csv")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB")

#Save the data as a csv file
write_csv(valid_data, 
          "validation_data_for_process_tanks.csv")

