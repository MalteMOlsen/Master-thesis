#///////////////////////////////////////////////////////////////////////////////
#Functions for data merging
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script to aggregated the cleaned one minute data frame into 
#a half hourly data frame.



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Averaging over time intervals function
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Define the function and specify the variables that shall be used
#df= data frame with smaller timer intervals, 
#time_interval=the time interval that the new data frame should have, needs a string
#old_time_column_name= the name of the time column in the df
#rain_fall_column= the name of the rain column of the df
aggregate_into_half_hourly_model_data <-function(df,
                                      column_name){

  #Converting the select columns to a average over the time interval
  df <- df %>% 
    #Select the column that need to be aggregated
    select({{column_name}}) %>% 
    #Converting to tibble to make the functions work better
    as_tibble() %>% 
    #Creating a new time column with values round down to the nearest time interval
    mutate(time_thirty_min=
             floor_date(time_one_min,
                        unit="30mins"))%>% 
    #Removing the old time column
    select(-time_one_min) %>% 
    #Grouping by the new time column
    group_by(time_thirty_min) %>% 
    #Overwrite all the existing columns with the average over the given time interval
    #this is done for all columns which are numeric, the na.rm=T makes the averaging robust to missing values
    summarise(across(where(is.numeric), mean, na.rm=T)) %>%
    #Convert the back to a tsibble
    as_tsibble()
}
