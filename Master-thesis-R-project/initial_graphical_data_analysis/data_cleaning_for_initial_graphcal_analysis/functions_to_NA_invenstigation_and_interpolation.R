#///////////////////////////////////////////////////////////////////////////////
#Functions used for the initial data cleaning and NA investigation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create many standard functions which can be 
#used for the NA investigation and the linear overwriting of these gaps.



#Approximate NA values in a time interval with at linear interpolation
#-------------------------------------------------------------------------------
NA_linear_approximate <- function(df, 
                                  time_periode, 
                                  column_name){
  df2 <- df %>% 
    #Select the time period of interest
    filter_index(time_periode) %>% 
    #Select the column of interest
    select({{column_name}}) %>% 
    #Overwrite the data gap with a linear approximation and name the column: temp_coulmn
    mutate(temp_column=na.approx({{column_name}})) %>% 
    #Remove the selected column containing the gap
    select(-{{column_name}})
}

#Overwriting NA values with data from another data frame
#-------------------------------------------------------------------------------
overwrite_NA_values <- function(df_with_NA,
                                df_with_values_in_the_gap,
                                column_name){
  df <- df_with_NA %>% 
    #joining the data with a left_join, the functions finds a best guess of a key
    #which in time column, and then add a column with linear interpolation values
    left_join(df_with_values_in_the_gap) %>%  
    #Replace values in df_with_NA if the value are NA
    mutate(temp=if_else(is.na({{column_name}}), 
                        temp_column, 
                        {{column_name}})) %>% 
    #Remove the column with the interpolated data
    select(-temp_column) %>% 
    #Remove the column which still include the NA values
    select(-{{column_name}}) %>% 
    #Rename the column where the NA has been replaced to the name of the column
    rename({{column_name}}:=temp)
}


#Finding how many hours have a NA in a day
#-------------------------------------------------------------------------------
counting_values_are_NA_in_a_day <- function(df, 
                                            column)
{
  df <- df %>% 
    #Select the column of interest
    select({{column}}) %>% 
    #Select only the data where the value is NA
    filter(is.na({{column}})) %>% 
    #Convert to a tibble
    as_tibble() %>% 
    #Create a new column where all the values are 1
    mutate(counter=1) %>% 
    #Creating a column which only include the day of measurement
    mutate(day_time=floor_date(time_thirty_min,"day")) %>% 
    #Removing the column with the half hourly time format
    select(-time_thirty_min) %>%
    #Grouping the data based on the day of measurement
    group_by(day_time) %>% 
    #Summing the counter each day and return a data frame which only includes
    #the accumulated counter and the day
    summarise(day_count=sum(counter))
}


