#///////////////////////////////////////////////////////////////////////////////
#Adding extra DIMS data to the one minute data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to add extra DIMS data to the one minute data frame


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
                        Value, 
                        {{column_name}})) %>% 
    #Remove the column with the interpolated data
    select(-Value) %>% 
    #Remove the column which still include the NA values
    select(-{{column_name}}) %>% 
    #Rename the column where the NA has been replaced to the name of the column
    rename({{column_name}}:=temp)
}

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")

#The extra ammonium in the effluent data
#-------------------------------------------------------------------------------
#Load the data from the csv file with every value as a character 
temp_ammonium_effluent <- read_delim(here::here("data",
                                                "extra_data_ammonium_effluent.csv"), 
                                     delim=";",
                                     col_types = cols(.default = "c"))


#Change , to . and make data numeric
temp_ammonium_effluent[,-1] <- lapply(temp_ammonium_effluent[,-1], 
                                      function(x) 
                                        as.numeric(gsub(",",
                                                        ".",  
                                                        as.character(x))))


#Change the time column to the year-month-day hour-minute-second format
temp_ammonium_effluent <- temp_ammonium_effluent %>% 
  mutate(DATETIME=dmy_hm(DATETIME))%>% 
  #remove data which have an NA value
  na.omit() %>% 
  rename(time_one_min=DATETIME)


#The extra nitrate in the effluent data
#-------------------------------------------------------------------------------
#Load the data from the csv file with every value as a character 
temp_nitrate_effluent <- read_delim(here::here("data",
                                               "extra_data_nitrate_effluent.csv"), 
                                    delim=";",
                                    col_types = cols(.default = "c"))


#Change , to . and make data numeric
temp_nitrate_effluent[,-1] <- lapply(temp_nitrate_effluent[,-1], 
                                     function(x) 
                                       as.numeric(gsub(",", 
                                                       ".", 
                                                       as.character(x))))


#Change the time column to the year-month-day hour-minute-second format
temp_nitrate_effluent <- temp_nitrate_effluent %>% 
  mutate(DATETIME=dmy_hm(DATETIME)) %>% 
  #remove data which have an NA value
  na.omit() %>% 
  rename(time_one_min=DATETIME)

#Add the extra data fround in DIMS to the half hourly data frame
#-------------------------------------------------------------------------------
#Adding the extra ammonium data
data_one_min <- overwrite_NA_values(data_one_min,
                                    temp_ammonium_effluent,
                                    ammonium_effluent_mg_L)

#Adding the extra nitrate data
data_one_min <- overwrite_NA_values(data_one_min,
                                    temp_nitrate_effluent,
                                    nitrate_effluent_mg_L)
