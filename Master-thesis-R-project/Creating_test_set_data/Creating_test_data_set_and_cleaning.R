#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
library(zoo)

#///////////////////////////////////////////////////////////////////////////////
#Function
#///////////////////////////////////////////////////////////////////////////////
rename_data <- function(df){
  #set working directory
  wd <-"C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/"
  
  #Load the new names from a separate csv file
  name_of_columns <- read_delim(paste(wd,"/data/Name_converter.csv",sep = ""), 
                                delim=";")
  #Defining the old names as the column "Sensors"
  oldnames <- name_of_columns$Sensors
  #Defining the new names as the column New_names 
  newnames <- name_of_columns$New_names
  
  #Matching the column names in the data frame with the sensors names in the csv file,
  #this creates a new vector of the position of all the matches, and returns NA
  #if a column is not matched with the sensors names in the csv file
  existing <- match(oldnames,names(df))
  
  #The if sentence is a security statement, that returns a warring if one or more
  #of the coulmns is not matched with the sensor names in the csv file
  if(sum(is.na(existing)) > 0)
    #If all the names are matches the sum of NA would be 0, therefore a warring message
    #is printed if the sum is larger than 0
  {
    #The warring message prints both the indices number and the name of the sensor
    print("The following indices did not match any columns:")
    print(which(is.na(existing)))
    print("Corresponding to these columns in oldnames")
    print(oldnames[which(is.na(existing))])
  }
  #using the dplyr rename_with function to rename comparing the column name
  #with the new names form csv file and replace them.
  df %>% 
    rename_with(~ newnames,
                all_of(oldnames))
}





#Defining a list of all the months of data, expect for 2018-01
months_meausred <- c("2022-04",
                     "2022-05",
                     "2022-06",
                     "2022-07",
                     "2022-08",
                     "2022-09",
                     "2022-10",
                     "2022-11",
                     "2022-12"
)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Defining the functions that will be used in the aggregation
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Averaging over time intervals function

#Define the function and specify the variables that shall be used
#df= data frame with smaller timer intervals, 
#time_interval=the time interval that the new data frame should have, needs a string
#old_time_column_name= the name of the time column in the df
#rain_fall_column= the name of the rain column of the df
average_over_time_interval <-function(df,
                                      time_interval,
                                      old_time_column_name,
                                      rain_fall_column){
  #As the rain is measured as accumulated rain over a year the new rain column
  #should be based the sum of rain, not the average as with the other columns
  df1 <- df %>% 
    #Converting to tibble to make the functions work better
    as_tibble() %>% 
    #Selecting the time column and the rain_fall column, as all other columns should be subjected to averaging
    select({{old_time_column_name}},{{rain_fall_column}})%>% 
    #Defining a new time column where the time are round to down to the nearest time interval
    mutate(temp_name1=
             floor_date({{old_time_column_name}}, 
                        unit=time_interval))%>% 
    #Removing the old time column
    select(-{{old_time_column_name}}) %>% 
    #Grouping by the new time column 
    group_by(temp_name1) %>% 
    #Creating a new rain column which is the sum in each time interval
    summarise(sum_rain= sum({{rain_fall_column}})) %>% 
    #Converting the data back to a tsibble
    as_tsibble()
  
  #Converting all other columns expect the rain column to a average over the time interval
  df2 <- df %>% 
    #Converting to tibble to make the functions work better
    as_tibble() %>% 
    #Remove the rain fall column as this column has been subjected to summation in df1
    select(-{{rain_fall_column}}) %>% 
    #Creating a new time column with values round down to the nearest time interval
    mutate(temp_name2=
             floor_date({{old_time_column_name}},
                        unit=time_interval))%>% 
    #Removing the old time column
    select(-{{old_time_column_name}}) %>% 
    #Grouping by the new time column
    group_by(temp_name2) %>% 
    #Overwrite all the existing columns with the average over the given time interval
    #this is done for all columns which are numeric, the na.rm=T makes the averaging robust to missing values
    summarise(across(where(is.numeric), mean, na.rm=T)) %>%
    #Convert the back to a tsibble
    as_tsibble()
  
  #Combine the to data frames by the columns
  df <- bind_cols(df1,df2) %>% 
    #Remove one of the time columns
    select(-temp_name2)
}


#///////////////////////////////////////////////////////////////////////////////
#Script
#///////////////////////////////////////////////////////////////////////////////




#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Load the data in a for loop
parrent_folder="C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/Creating_test_set_data"
folder_name = "Data"

#Making a list of all the csv file names that should be used
list_of_filenames <- list.files(paste(parrent_folder,
                                      folder_name,
                                      sep = "/"))


#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
#Check length need to be 288
#!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!


wd <- paste(parrent_folder,folder_name,sep="/")

drops1 <- c("...1")
data_one_min <- data.frame()


#set working directory
for (file in list_of_filenames){
  
  df <- read_delim(paste(wd,file,sep = "/"), 
                   delim=";",
                   col_types = cols(.default = "c"))
  
  # Change "," to "." and converting all columns to numeric, expect in the time column
  df[,-1] <- lapply(df[,-1], 
                    function(x) 
                      as.numeric(gsub(",", ".",  as.character(x))))
  
  # Change the time column to the year-month-day hour-minute-second format
  df <- df %>% 
    mutate(DATETIME=ymd_hms(DATETIME))
  
  #df <- df[ , !(names(df) %in% drops1)]
  
  data_one_min <- bind_rows(df,
                            data_one_min)
}


data_one_min <- data_one_min %>% 
  rename_data()

data_one_min <- data_one_min %>% 
  distinct(DATETIME,
           .keep_all = T)

#Renaming the time column
data_one_min <- data_one_min%>% 
  rename(time_one_min=DATETIME)

data_one_min <- data_one_min %>% 
  as_tsibble()

data_one_min <- data_one_min %>% 
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
  mutate(total_N_load_PT1_kg_h=nitrate_load_PT1_kg_h+ammonium_load_PT1_kg_h)


data_one_min <- data_one_min %>% 
  select(time_one_min,
         rainfall_mm,
         flow_AN_m3_h,
         flow_influent_m3_h,
         flow_HT_m3_h,
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
         nitrate_PT4_mg_L,
         nitrate_PT3_mg_L,
         nitrate_PT2_mg_L,
         nitrate_PT1_mg_L,
         nitrate_load_PT4_kg_h,
         nitrate_load_PT3_kg_h,
         nitrate_load_PT2_kg_h,
         nitrate_load_PT1_kg_h)

missing <- data_one_min %>% 
  select(rainfall_mm) %>% 
  filter(is.na(rainfall_mm))


#Taking the difference of the accumulated rain, to get actual rain fall
data_one_min <- data_one_min %>% 
  mutate(rainfall_mm=difference(rainfall_mm))

#Analyzing the data, removing negative values and outliers
#Finding the negative values
rain_negative_values <- data_one_min %>% 
  filter(rainfall_mm<0)

#Replacing the the negative values with zero, found in the weather achieve to be true
data_one_min$rainfall_mm[data_one_min$rainfall_mm<0] <- 0

#Peaks give large values in negative and positive direction
#As it is unlikely to have 5.5 mm rain in one minute this is the value threshold
#due to the danish record of rain is 5.4 mm/min
rain_large_values <- data_one_min %>% 
  filter(rainfall_mm>5.5)

#Replacing the the values over 10 to zero, found in the weather achieve to be true
data_one_min$rainfall_mm[data_one_min$rainfall_mm>5.5] <- 0




folder_name2="drought_data"
wd <- paste(parrent_folder,folder_name2,sep="/")

#Making a list of all the csv file names that should be used
list_of_file_DMI_drought_data <- list.files(paste(wd))

#Defining the first month in the list of month manually 
data_DMI_drought <-data.frame()

#Resetting the counter i to 0
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_file_DMI_drought_data){
  #Loading and pre-processing the data
  df1 <- read_delim(paste(wd,
                          file,
                          sep="/"), 
                    delim=";",
                    col_types = cols(.default = "c"))
  
  #Skip the first manually assigned file 
  
  data_DMI_drought <- bind_rows(df1,
                                data_DMI_drought)

  i=i+1  
  
}

#Creating a data frame on the data extracted from DMI
data_DMI_drought <- data_DMI_drought %>% 
  #Renaming the columns
  rename(day=DateTime, 
         drought="Tørkeindex") %>% 
  #Converting the time column to the correct format
  mutate(day=as.Date(day)) %>%
  #Replacing comma with dot and converting to numeric
  mutate(drought= as.numeric(gsub(",", 
                                  ".",  
                                  as.character(drought)))) %>% 
  #Removing duplicates based on time
  distinct(day,
           .keep_all = T) %>% 
  #Convert to tsibble
  as_tsibble(index=day) %>% 
  #Removing data after 2022-04-19
  filter_index(~"2022-12-15") %>% 
  #Convert to tibble
  as_tibble()

#Adding the drought data to the data frame
data_one_min <- data_one_min %>% 
  #Creating a column with time in a day format
  mutate(day=as.Date(time_one_min)) %>% 
  #Joining drought index to the data frame
  left_join(data_DMI_drought) %>% 
  #Removing the day time column 
  select(-day)


#///////////////////////////////////////////////////////////////////////////////

data_one_min <- data_one_min %>% 
  mutate(lag1_rainfall_mm=lag(rainfall_mm)) %>% 
  mutate(lag1_flow_AN_m3_h=lag(flow_AN_m3_h)) %>% 
  mutate(lag1_flow_influent_m3_h=lag(flow_influent_m3_h)) %>% 
  mutate(lag1_flow_HT_m3_h=lag(flow_HT_m3_h)) %>% 
  mutate(lag1_ammonium_to_AN_mg_L=lag(ammonium_to_AN_mg_L)) %>% 
  mutate(lag1_T_PT1_C=lag(T_PT1_C)) %>% 
  mutate(lag1_T_PT2_C=lag(T_PT2_C)) %>% 
  mutate(lag1_T_PT3_C=lag(T_PT3_C)) %>% 
  mutate(lag1_T_PT4_C=lag(T_PT4_C)) %>% 
  mutate(lag1_ammonium_PT1_mg_L=lag(ammonium_PT1_mg_L)) %>% 
  mutate(lag1_ammonium_PT2_mg_L=lag(ammonium_PT2_mg_L)) %>% 
  mutate(lag1_ammonium_PT3_mg_L=lag(ammonium_PT3_mg_L)) %>% 
  mutate(lag1_ammonium_PT4_mg_L=lag(ammonium_PT4_mg_L)) %>% 
  mutate(lag1_SS_PT1_g_L=lag(SS_PT1_g_L)) %>% 
  mutate(lag1_SS_PT4_g_L=lag(SS_PT4_g_L)) %>% 
  mutate(lag1_airflow_PT1_m3_h=lag(airflow_PT1_m3_h)) %>% 
  mutate(lag1_airflow_PT2_m3_h=lag(airflow_PT2_m3_h)) %>% 
  mutate(lag1_airflow_PT3_m3_h=lag(airflow_PT3_m3_h)) %>% 
  mutate(lag1_airflow_PT4_m3_h=lag(airflow_PT4_m3_h)) %>% 
  mutate(lag1_nitrate_PT1_mg_L=lag(nitrate_PT1_mg_L)) %>% 
  mutate(lag1_nitrate_PT2_mg_L=lag(nitrate_PT2_mg_L)) %>% 
  mutate(lag1_nitrate_PT3_mg_L=lag(nitrate_PT3_mg_L)) %>% 
  mutate(lag1_nitrate_PT4_mg_L=lag(nitrate_PT4_mg_L)) %>% 
  mutate(lag1_DO_PT1_mg_L=lag(DO_PT1_mg_L)) %>% 
  mutate(lag1_DO_PT2_mg_L=lag(DO_PT2_mg_L)) %>% 
  mutate(lag1_DO_PT3_mg_L=lag(DO_PT3_mg_L)) %>% 
  mutate(lag1_DO_PT4_mg_L=lag(DO_PT4_mg_L))

#///////////////////////////////////////////////////////////////////////////////





data_one_min <- data_one_min %>% 
  mutate(diff_rainfall_mm=difference(rainfall_mm)) %>% 
  mutate(diff_flow_AN_m3_h=difference(flow_AN_m3_h)) %>%
  mutate(diff_flow_influent_m3_h=difference(flow_influent_m3_h)) %>%
  mutate(diff_flow_HT_m3_h=difference(flow_HT_m3_h)) %>%
  mutate(diff_ammonium_to_AN_mg_L=difference(ammonium_to_AN_mg_L)) %>%
  mutate(diff_T_PT1_C=difference(T_PT1_C)) %>%
  mutate(diff_T_PT2_C=difference(T_PT2_C)) %>%
  mutate(diff_T_PT3_C=difference(T_PT3_C)) %>%
  mutate(diff_T_PT4_C=difference(T_PT4_C)) %>%
  mutate(diff_ammonium_PT1_mg_L=difference(ammonium_PT1_mg_L)) %>%
  mutate(diff_ammonium_PT2_mg_L=difference(ammonium_PT2_mg_L)) %>%
  mutate(diff_ammonium_PT3_mg_L=difference(ammonium_PT3_mg_L)) %>%
  mutate(diff_ammonium_PT4_mg_L=difference(ammonium_PT4_mg_L)) %>%
  mutate(diff_SS_PT1_g_L=difference(SS_PT1_g_L)) %>%
  mutate(diff_SS_PT4_g_L=difference(SS_PT4_g_L)) %>%
  mutate(diff_airflow_PT1_m3_h=difference(airflow_PT1_m3_h)) %>%
  mutate(diff_airflow_PT2_m3_h=difference(airflow_PT2_m3_h)) %>%
  mutate(diff_airflow_PT3_m3_h=difference(airflow_PT3_m3_h)) %>%
  mutate(diff_airflow_PT4_m3_h=difference(airflow_PT4_m3_h)) %>%
  mutate(diff_nitrate_PT1_mg_L=difference(nitrate_PT1_mg_L)) %>%
  mutate(diff_nitrate_PT2_mg_L=difference(nitrate_PT2_mg_L)) %>%
  mutate(diff_nitrate_PT3_mg_L=difference(nitrate_PT3_mg_L)) %>%
  mutate(diff_nitrate_PT4_mg_L=difference(nitrate_PT4_mg_L)) %>%
  mutate(diff_DO_PT1_mg_L=difference(DO_PT1_mg_L)) %>%
  mutate(diff_DO_PT2_mg_L=difference(DO_PT2_mg_L)) %>%
  mutate(diff_DO_PT3_mg_L=difference(DO_PT3_mg_L)) %>%
  mutate(diff_DO_PT4_mg_L=difference(DO_PT4_mg_L)) %>%
  mutate(diff_lag1_flow_AN_m3_h=difference(lag1_flow_AN_m3_h)) %>%
  mutate(diff_lag1_flow_influent_m3_h=difference(lag1_flow_influent_m3_h)) %>%
  mutate(diff_lag1_flow_HT_m3_h=difference(lag1_flow_HT_m3_h)) %>%
  mutate(diff_lag1_ammonium_to_AN_mg_L=difference(lag1_ammonium_to_AN_mg_L)) %>%
  mutate(diff_lag1_T_PT1_C=difference(lag1_T_PT1_C)) %>%
  mutate(diff_lag1_T_PT2_C=difference(lag1_T_PT2_C)) %>%
  mutate(diff_lag1_T_PT3_C=difference(lag1_T_PT3_C)) %>%
  mutate(diff_lag1_T_PT4_C=difference(lag1_T_PT4_C)) %>%
  mutate(diff_lag1_ammonium_PT1_mg_L=difference(lag1_ammonium_PT1_mg_L)) %>%
  mutate(diff_lag1_ammonium_PT2_mg_L=difference(lag1_ammonium_PT2_mg_L)) %>%
  mutate(diff_lag1_ammonium_PT3_mg_L=difference(lag1_ammonium_PT3_mg_L)) %>%
  mutate(diff_lag1_ammonium_PT4_mg_L=difference(lag1_ammonium_PT4_mg_L)) %>%
  mutate(diff_lag1_SS_PT1_g_L=difference(lag1_SS_PT1_g_L)) %>%
  mutate(diff_lag1_SS_PT4_g_L=difference(lag1_SS_PT4_g_L)) %>%
  mutate(diff_lag1_airflow_PT1_m3_h=difference(lag1_airflow_PT1_m3_h)) %>%
  mutate(diff_lag1_airflow_PT2_m3_h=difference(lag1_airflow_PT2_m3_h)) %>%
  mutate(diff_lag1_airflow_PT3_m3_h=difference(lag1_airflow_PT3_m3_h)) %>%
  mutate(diff_lag1_airflow_PT4_m3_h=difference(lag1_airflow_PT4_m3_h)) %>%
  mutate(diff_lag1_nitrate_PT1_mg_L=difference(lag1_nitrate_PT1_mg_L)) %>%
  mutate(diff_lag1_nitrate_PT2_mg_L=difference(lag1_nitrate_PT2_mg_L)) %>%
  mutate(diff_lag1_nitrate_PT3_mg_L=difference(lag1_nitrate_PT3_mg_L)) %>%
  mutate(diff_lag1_nitrate_PT4_mg_L=difference(lag1_nitrate_PT4_mg_L)) %>%
  mutate(diff_lag1_DO_PT1_mg_L=difference(lag1_DO_PT1_mg_L)) %>%
  mutate(diff_lag1_DO_PT2_mg_L=difference(lag1_DO_PT2_mg_L)) %>%
  mutate(diff_lag1_DO_PT3_mg_L=difference(lag1_DO_PT3_mg_L)) %>%
  mutate(diff_lag1_DO_PT4_mg_L=difference(lag1_DO_PT4_mg_L))

#///////////////////////////////////////////////////////////////////////////////

#----
#Ammonium concentration
#------------------------------------
#Max ammonium concentration is 85 mg/L, plus minus 10 in diff and the lag diff column

#Ammonium in the AN tank
data_one_min$ammonium_to_AN_mg_L[data_one_min$ammonium_to_AN_mg_L>85] <- NA
data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L>10] <- NA
data_one_min$ammonium_to_AN_mg_L[data_one_min$diff_ammonium_to_AN_mg_L< -10] <- NA

#Ammonium in process tank 1
data_one_min$ammonium_PT1_mg_L[data_one_min$ammonium_PT1_mg_L>85] <- NA
data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L>10] <- NA
data_one_min$ammonium_PT1_mg_L[data_one_min$diff_ammonium_PT1_mg_L< -10] <- NA
data_one_min$ammonium_PT1_mg_L[data_one_min$diff_lag1_ammonium_PT1_mg_L>10] <- NA
data_one_min$ammonium_PT1_mg_L[data_one_min$diff_lag1_ammonium_PT1_mg_L< -10] <- NA

#Ammonium in process tank 2
data_one_min$ammonium_PT2_mg_L[data_one_min$ammonium_PT2_mg_L>85] <- NA
data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L>10] <- NA
data_one_min$ammonium_PT2_mg_L[data_one_min$diff_ammonium_PT2_mg_L< -10] <- NA
data_one_min$ammonium_PT2_mg_L[data_one_min$diff_lag1_ammonium_PT2_mg_L>10] <- NA
data_one_min$ammonium_PT2_mg_L[data_one_min$diff_lag1_ammonium_PT2_mg_L< -10] <- NA

#Ammonium in process tank 3
data_one_min$ammonium_PT3_mg_L[data_one_min$ammonium_PT3_mg_L>85] <- NA
data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L>10] <- NA
data_one_min$ammonium_PT3_mg_L[data_one_min$diff_ammonium_PT3_mg_L< -10] <- NA
data_one_min$ammonium_PT3_mg_L[data_one_min$diff_lag1_ammonium_PT3_mg_L>10] <- NA
data_one_min$ammonium_PT3_mg_L[data_one_min$diff_lag1_ammonium_PT3_mg_L< -10] <- NA

#Ammonium in process tank 4
data_one_min$ammonium_PT4_mg_L[data_one_min$ammonium_PT4_mg_L>85] <- NA
data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L>10] <- NA
data_one_min$ammonium_PT4_mg_L[data_one_min$diff_ammonium_PT4_mg_L< -10] <- NA
data_one_min$ammonium_PT4_mg_L[data_one_min$diff_lag1_ammonium_PT4_mg_L>10] <- NA
data_one_min$ammonium_PT4_mg_L[data_one_min$diff_lag1_ammonium_PT4_mg_L< -10] <- NA

#----
#Temperature
#------------------------------------
#The values above 25 and under 2 degrees are removed, and a difference of +-1

#Temperature in process tank 1
data_one_min$T_PT1_C[data_one_min$T_PT1_C>25] <- NA
data_one_min$T_PT1_C[data_one_min$T_PT1_C< 2] <- NA
data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C>1] <- NA
data_one_min$T_PT1_C[data_one_min$diff_T_PT1_C< -1] <- NA
data_one_min$T_PT1_C[data_one_min$diff_lag1_T_PT1_C>1] <- NA
data_one_min$T_PT1_C[data_one_min$diff_lag1_T_PT1_C< -1] <- NA

#Temperature in process tank 2
data_one_min$T_PT2_C[data_one_min$T_PT2_C>25] <- NA
data_one_min$T_PT2_C[data_one_min$T_PT2_C< 2] <- NA
data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C>1] <- NA
data_one_min$T_PT2_C[data_one_min$diff_T_PT2_C< -1] <- NA
data_one_min$T_PT2_C[data_one_min$diff_lag1_T_PT2_C>1] <- NA
data_one_min$T_PT2_C[data_one_min$diff_lag1_T_PT2_C< -1] <- NA

#Temperature in process tank 3
data_one_min$T_PT3_C[data_one_min$T_PT3_C>25] <- NA
data_one_min$T_PT3_C[data_one_min$T_PT3_C< 2] <- NA
data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C>1] <- NA
data_one_min$T_PT3_C[data_one_min$diff_T_PT3_C< -1] <- NA
data_one_min$T_PT3_C[data_one_min$diff_lag1_T_PT3_C>1] <- NA
data_one_min$T_PT3_C[data_one_min$diff_lag1_T_PT3_C< -1] <- NA

#Temperature in process tank 4
data_one_min$T_PT4_C[data_one_min$T_PT4_C>25] <- NA
data_one_min$T_PT4_C[data_one_min$T_PT4_C< 2] <- NA
data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C>1] <- NA
data_one_min$T_PT4_C[data_one_min$diff_T_PT4_C< -1] <- NA
data_one_min$T_PT4_C[data_one_min$diff_lag1_T_PT4_C>1] <- NA
data_one_min$T_PT4_C[data_one_min$diff_lag1_T_PT4_C< -1] <- NA

#----
#Aeration
#------------------------------------
#The max broader is set to be 8000 m3/h no difference cleaning
#Process tank 1
data_one_min$airflow_PT1_m3_h[data_one_min$airflow_PT1_m3_h>4000] <- NA

#Process tank 2
data_one_min$airflow_PT2_m3_h[data_one_min$airflow_PT2_m3_h>4000] <- NA

#Process tank 3
data_one_min$airflow_PT3_m3_h[data_one_min$airflow_PT3_m3_h>4000] <- NA

#Process tank 4
data_one_min$airflow_PT4_m3_h[data_one_min$airflow_PT4_m3_h>4000] <- NA

#----
#Dissolved oxygen
#------------------------------------
#The engeering tool box is stating that the fresh water solubility is 13.9 mg/L
#at 2 degrees and salinity reduces the solubility, and the difference broader is
#set to be 5 mg/L based on how the data looks

#DO in process tank 1
data_one_min$DO_PT1_mg_L[data_one_min$DO_PT1_mg_L>5] <- NA
data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L>5] <- NA
data_one_min$DO_PT1_mg_L[data_one_min$diff_DO_PT1_mg_L< -5] <- NA
data_one_min$DO_PT1_mg_L[data_one_min$diff_lag1_DO_PT1_mg_L>5] <- NA
data_one_min$DO_PT1_mg_L[data_one_min$diff_lag1_DO_PT1_mg_L< -5] <- NA

#DO in process tank 2
data_one_min$DO_PT2_mg_L[data_one_min$DO_PT2_mg_L>5] <- NA
data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L>5] <- NA
data_one_min$DO_PT2_mg_L[data_one_min$diff_DO_PT2_mg_L< -5] <- NA
data_one_min$DO_PT2_mg_L[data_one_min$diff_lag1_DO_PT2_mg_L>5] <- NA
data_one_min$DO_PT2_mg_L[data_one_min$diff_lag1_DO_PT2_mg_L< -5] <- NA

#DO in process tank 3
data_one_min$DO_PT3_mg_L[data_one_min$DO_PT3_mg_L>5] <- NA
data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L>5] <- NA
data_one_min$DO_PT3_mg_L[data_one_min$diff_DO_PT3_mg_L< -5] <- NA
data_one_min$DO_PT3_mg_L[data_one_min$diff_lag1_DO_PT3_mg_L>5] <- NA
data_one_min$DO_PT3_mg_L[data_one_min$diff_lag1_DO_PT3_mg_L< -5] <- NA

#DO in process tank 4
data_one_min$DO_PT4_mg_L[data_one_min$DO_PT4_mg_L>5] <- NA
data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L>5] <- NA
data_one_min$DO_PT4_mg_L[data_one_min$diff_DO_PT4_mg_L< -5] <- NA
data_one_min$DO_PT4_mg_L[data_one_min$diff_lag1_DO_PT4_mg_L>5] <- NA
data_one_min$DO_PT4_mg_L[data_one_min$diff_lag1_DO_PT4_mg_L< -5] <- NA

#----
#SS concentration
#------------------------------------
#The max concentration is chosen to be 10 mg/L and minimum od 0 and the 
#max diff is set to be 2 mg/L both are only based on the graphs, and a
#physical insistent

#SS in process tank 1 
data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L>10] <- NA
data_one_min$SS_PT1_g_L[data_one_min$SS_PT1_g_L<=0] <- NA
data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L>2] <- NA
data_one_min$SS_PT1_g_L[data_one_min$diff_SS_PT1_g_L< -2] <- NA

#SS in process tank 4 
data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L>10] <- NA
data_one_min$SS_PT4_g_L[data_one_min$SS_PT4_g_L<=0] <- NA
data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L>2] <- NA
data_one_min$SS_PT4_g_L[data_one_min$diff_SS_PT4_g_L< -2] <- NA




#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
model_data <- data.frame()

#Creating the 30 min time interval
for(months in months_meausred){
  
  df1 <- data_one_min %>% 
    #Selecting the month
    filter_index(months)%>% 
    #Averaging (and summing) over the 30 minutes interval
    average_over_time_interval("30 mins", 
                               time_one_min, 
                               rainfall_mm) 
  
  #Bind the months back to one data frame
  model_data <- bind_rows(df1, 
                          model_data)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
model_data <- model_data %>% 
  rename(time_thirty_min=temp_name1)

model_data <- model_data %>% 
  rename(rainfall_mm=sum_rain)


#Removing the duplicated time values and keeping the first value and removing the rest
model_data <- model_data%>%  
  distinct(time_thirty_min,
           .keep_all = T)



#----
#Ammonium concentration
#------------------------------------
#Max ammonium concentration plus minus 1 in diff

#Ammonium in the AN tank
model_data$ammonium_to_AN_mg_L[model_data$diff_ammonium_to_AN_mg_L>1] <- NA
model_data$ammonium_to_AN_mg_L[model_data$diff_ammonium_to_AN_mg_L< -1] <- NA


#Ammonium in process tank 1
model_data$ammonium_PT1_mg_L[model_data$diff_ammonium_PT1_mg_L>1] <- NA
model_data$ammonium_PT1_mg_L[model_data$diff_ammonium_PT1_mg_L< -1] <- NA


#Ammonium in process tank 2
model_data$ammonium_PT2_mg_L[model_data$diff_ammonium_PT2_mg_L>1] <- NA
model_data$ammonium_PT2_mg_L[model_data$diff_ammonium_PT2_mg_L< -1] <- NA


#Ammonium in process tank 3
model_data$ammonium_PT3_mg_L[model_data$diff_ammonium_PT3_mg_L>1] <- NA
model_data$ammonium_PT3_mg_L[model_data$diff_ammonium_PT3_mg_L< -1] <- NA


#Ammonium in process tank 4
model_data$ammonium_PT4_mg_L[model_data$diff_ammonium_PT4_mg_L>1] <- NA
model_data$ammonium_PT4_mg_L[model_data$diff_ammonium_PT4_mg_L< -1] <- NA


#----
#Nitrate
#------------------------------------
#Max nitrate concentration plus minus 1 in diff

#process tank 1
model_data$nitrate_PT1_mg_L[model_data$diff_nitrate_PT1_mg_L>1] <- NA
model_data$nitrate_PT1_mg_L[model_data$diff_nitrate_PT1_mg_L< -1] <- NA


#process tank 2
model_data$nitrate_PT2_mg_L[model_data$diff_nitrate_PT2_mg_L>1] <- NA
model_data$nitrate_PT2_mg_L[model_data$diff_nitrate_PT2_mg_L< -1] <- NA


#process tank 3
model_data$nitrate_PT3_mg_L[model_data$diff_nitrate_PT3_mg_L>1] <- NA
model_data$nitrate_PT3_mg_L[model_data$diff_nitrate_PT3_mg_L< -1] <- NA


#process tank 4
model_data$nitrate_PT4_mg_L[model_data$diff_nitrate_PT4_mg_L>1] <- NA
model_data$nitrate_PT4_mg_L[model_data$diff_nitrate_PT4_mg_L< -1] <- NA




#----
#Temperature
#------------------------------------
#The values with a difference of +-0.1

#Temperature in process tank 1
model_data$T_PT1_C[model_data$diff_T_PT1_C>0.1] <- NA
model_data$T_PT1_C[model_data$diff_T_PT1_C< -0.1] <- NA


#Temperature in process tank 2
model_data$T_PT2_C[model_data$diff_T_PT2_C>0.1] <- NA
model_data$T_PT2_C[model_data$diff_T_PT2_C< -0.1] <- NA


#Temperature in process tank 3
model_data$T_PT3_C[model_data$diff_T_PT3_C>0.1] <- NA
model_data$T_PT3_C[model_data$diff_T_PT3_C< -0.1] <- NA


#Temperature in process tank 4
model_data$T_PT4_C[model_data$diff_T_PT4_C>0.1] <- NA
model_data$T_PT4_C[model_data$diff_T_PT4_C< -0.1] <- NA


#----
#Dissolved oxygen
#------------------------------------
#The difference broader is set to be 0.2 based on how the data looks

#DO in process tank 1
model_data$DO_PT1_mg_L[model_data$diff_DO_PT1_mg_L>0.2] <- NA
model_data$DO_PT1_mg_L[model_data$diff_DO_PT1_mg_L< -0.2] <- NA


#DO in process tank 2
model_data$DO_PT2_mg_L[model_data$diff_DO_PT2_mg_L>0.2] <- NA
model_data$DO_PT2_mg_L[model_data$diff_DO_PT2_mg_L< -0.2] <- NA


#DO in process tank 3
model_data$DO_PT3_mg_L[model_data$diff_DO_PT3_mg_L>0.2] <- NA
model_data$DO_PT3_mg_L[model_data$diff_DO_PT3_mg_L< -0.2] <- NA


#DO in process tank 4
model_data$DO_PT4_mg_L[model_data$diff_DO_PT4_mg_L>0.2] <- NA
model_data$DO_PT4_mg_L[model_data$diff_DO_PT4_mg_L< -0.2] <- NA







model_data <- model_data %>% 
  select(time_thirty_min,
         rainfall_mm,
         flow_AN_m3_h,
         flow_influent_m3_h,
         flow_HT_m3_h,
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
         nitrate_PT4_mg_L,
         nitrate_PT3_mg_L,
         nitrate_PT2_mg_L,
         nitrate_PT1_mg_L,
         nitrate_load_PT4_kg_h,
         nitrate_load_PT3_kg_h,
         nitrate_load_PT2_kg_h,
         nitrate_load_PT1_kg_h,
         drought)
  







model_data <- model_data %>%
  mutate(timestamp = as.POSIXct(time_thirty_min)) %>% 
  # complete sequence to full sequence from min to max by second
  complete(timestamp = seq.POSIXt(min(timestamp), max(timestamp), by = '30 min')) 

model_data <- model_data %>% 
  select(-time_thirty_min) %>% 
  rename(time_thirty_min=timestamp) %>% 
  as_tsibble()









model_data <- model_data %>%   
  mutate(flow_AN_m3_h=na.approx(flow_AN_m3_h)) %>%  
  mutate(flow_influent_m3_h=na.approx(flow_influent_m3_h)) %>%    
  mutate(flow_HT_m3_h=na.approx(flow_HT_m3_h)) %>%   
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
  mutate(nitrate_load_PT4_kg_h=na.approx(nitrate_load_PT4_kg_h)) %>%    
  mutate(nitrate_load_PT3_kg_h=na.approx(nitrate_load_PT3_kg_h)) %>%    
  mutate(nitrate_load_PT2_kg_h=na.approx(nitrate_load_PT2_kg_h)) %>%    
  mutate(nitrate_load_PT1_kg_h=na.approx(nitrate_load_PT1_kg_h)) %>%
  mutate(month=month(time_thirty_min))

model_data[1,2] <- 0

missing <- model_data %>% 
  select(rainfall_mm,time_thirty_min) %>% 
  filter(is.na(rainfall_mm))


#Set working directory
wd <- "C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/Creating_test_set_data"


#Making a list of all the csv file names that should be used
list_of_file_DMI_data <- list.files(paste(wd,
                                          "/DMI_rain_data",
                                          sep=""))

#Defining the first month in the list of month manually 
data_DMI_rain <- data.frame()

#Resetting the counter i to 0
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_file_DMI_data){
  #Loading and pre-processing the data
  df1 <- read_delim(paste(wd,
                          "/DMI_rain_data/",
                          file,
                          sep=""), 
                    delim=";",
                    col_types = cols(.default = "c"))
  
  #Skip the first manually assigned file 
 
  data_DMI_rain <- bind_rows(df1, 
                             data_DMI_rain)
  #Add one to the counter
  i=i+1  
  
}

#Creating a data frame on the data extracted from DMI
data_DMI_rain <- data_DMI_rain %>% 
  #Renaming the columns
  rename(hour=DateTime,rainfall_new=Nedbør) %>% 
  #Converting the time column to the correct format
  mutate(hour=ymd_hms(hour)) %>% 
  #Replacing comma with dot and converting to numeric
  mutate(rainfall_new= as.numeric(gsub(",",
                                       ".", 
                                       as.character(rainfall_new)))) %>% 
  #Converting the rain data from rain pr. hour to rain pr. minute
  mutate(rainfall_new=rainfall_new/2) %>%  
  distinct(hour,
           .keep_all = T)


#overwriting the NA rain values
model_data <- model_data %>% 
  #convert the time to a date instead of a ymd_hms
  mutate(hour=floor_date(time_thirty_min,
                         "hour")) %>% 
  #Join the data from DMI's weather archives to the data frame
  left_join(data_DMI_rain) %>%  
  #Replace the NA values in the rain column and leave values which is not NA
  mutate(rainfall_mm=if_else(is.na(rainfall_mm), 
                             rainfall_new, 
                             rainfall_mm)) %>%
  #remove the hourly time column and the DMI's weather archive data
  select(-hour,-rainfall_new)





total_na3=0
for (i in seq(from = 1, to = 38, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na3 = total_na3+nrow(temp)
  
  print("# NA in")
  print(names(model_data[,i]))
  print(nrow(temp))
}

model_data[12433,38] <- 0
model_data <- model_data %>%   
  mutate(drought=na.approx(drought))



#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project")

#Save the data as a csv file
write_csv(model_data, 
          "test_data.csv")
