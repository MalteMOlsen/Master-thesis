#///////////////////////////////////////////////////////////////////////////////
#Aggregate the cleaned one minute data into a half hourly data
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to aggregate the cleaned one minute data into 
#a cleaned data frame with the half hourly time aggregation

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Run the Setup file
source("setup_for_model_data_creation.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Adding the difference columns
source("adding_difference_columns.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Run the Setup file
source("removing_outliers_from_one_minute_data_frame.R")

#Defining a list of all the months of data, expect for 2018-01
months_meausred <- c("2018-02","2018-03","2018-04",
                     "2018-05","2018-06","2018-07",
                     "2018-08","2018-09","2018-10",
                     "2018-11","2018-12",
                     "2019-01","2019-02","2019-03",
                     "2019-04","2019-05","2019-06",
                     "2019-07","2019-08","2019-09",
                     "2019-10","2019-11","2019-12",
                     "2020-01","2020-02","2020-03",
                     "2020-04","2020-05","2020-06",
                     "2020-07","2020-08","2020-09",
                     "2020-10","2020-11","2020-12",
                     "2021-01","2021-02","2021-03",
                     "2021-04","2021-05","2021-06",
                     "2021-07","2021-08","2021-09",
                     "2021-10","2021-11","2021-12",
                     "2022-01","2022-02","2022-03",
                     "2022-04"
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

#-------------------------------------------------------------------------------
#summing over 1 to 7 dayes of rain column 

#Defining the function
sum_over_rain_data <- function(df, rain_column_name, time_intervals_pr_day){
  
  df <- df %>%
    #Creating a column with accumulated rain for one day with the rollappplyr function
    mutate(rain_one_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                 width=1*time_intervals_pr_day, 
                                                 FUN=sum, 
                                                 partial=T))%>%
    #Creating a column with accumulated rain for two day with the rollappplyr function
    mutate(rain_two_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                 width=2*time_intervals_pr_day, 
                                                 FUN=sum, 
                                                 partial=T))%>%
    #Creating a column with accumulated rain for three day with the rollappplyr function
    mutate(rain_three_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                   width=3*time_intervals_pr_day, 
                                                   FUN=sum, 
                                                   partial=T))%>%
    #Creating a column with accumulated rain for four day with the rollappplyr function
    mutate(rain_four_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=4*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T))%>%
    #Creating a column with accumulated rain for five day with the rollappplyr function
    mutate(rain_five_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=5*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T))%>%
    #Creating a column with accumulated rain for six day with the rollappplyr function
    mutate(rain_six_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                 width=6*time_intervals_pr_day, 
                                                 FUN=sum, 
                                                 partial=T)) %>%
    #Creating a column with accumulated rain for seven day with the rollappplyr function
    mutate(rain_seven_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                   width=7*time_intervals_pr_day, 
                                                   FUN=sum, 
                                                   partial=T))
}


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 30 minutes interval
#-------------------------------------------
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
model_data <- data_one_min %>%
  #Selecting the first month
  filter_index("2018-01") %>% 
  #Averaging (and summing) over the 30 minutes interval
  average_over_time_interval("30 mins", 
                             time_one_min, 
                             rainfall_mm) %>% 
  #Creating the accumulated rain columns (48 30-min pr. day)
  sum_over_rain_data(sum_rain,
                     48)

#Creating the 30 min time interval
for(months in months_meausred){
  
  df1 <- data_one_min %>% 
    #Selecting the month
    filter_index(months)%>% 
    #Averaging (and summing) over the 30 minutes interval
    average_over_time_interval("30 mins", 
                               time_one_min, 
                               rainfall_mm) %>% 
    #Creating the accumulated rain columns (48 30-min pr. day)
    sum_over_rain_data(sum_rain,
                       48)
  
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

#Removing the duplicated time values and keeping the first value and removing the rest
model_data <- model_data%>%  
  distinct(time_thirty_min,
           .keep_all = T)

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data")

#Save the data as a csv file
write_csv(model_data, 
          "model_data.csv")
