
#Functions


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Data cleaning functions
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Approximate NA values in a time interval with at linear regression
NA_linear_approximate <- function(df, time_periode, column_name){
  df2 <- df %>% 
    filter_index(time_periode) %>% 
    select({{column_name}}) %>% 
    mutate(temp_column=na.approx({{column_name}})) %>% 
    select(-{{column_name}})
}

#Overwriting NA values with data from another data frame
overwrite_NA_values <- function(df_with_NA,
                                df_to_overwrite,
                                column_name){
  df <- df_with_NA %>% 
    left_join(df_to_overwrite) %>%  
    mutate(temp=if_else(is.na({{column_name}}), 
                        temp_column, 
                        {{column_name}})) %>% 
    select(-temp_column) %>% 
    select(-{{column_name}}) %>% 
    rename({{column_name}}:=temp)
}


#Finding how many hours have a NA in a day
counting_values_are_NA_in_a_day <- function(df, 
                                            column)
{
  df <- df %>% 
    select({{column}}) %>% 
    filter(is.na({{column}})) %>% 
    as_tibble() %>% 
    mutate(counter=1) %>% 
    mutate(day_time=floor_date(time_thirty_min,"day")) %>% 
    select(-time_thirty_min) %>%
    group_by(day_time) %>% 
    summarise(day_count=sum(counter))
}










