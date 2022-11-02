#///////////////////////////////////////////////////////////////////////////////
#Adding extra data found in DIMS to the half hourly data frame
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is add the extra data found in DIMS to the half 
#hourly data frame

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")

#The extra ammonium in the effluent data
#-------------------------------------------------------------------------------
#Load the data from the csv file with every value as a character 
temp_ammonium_effluent <- read_delim("data/extra_data_ammonium_effluent.csv", 
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
  na.omit()


#Aggregate into 30 minutes interval
temp_ammonium_effluent_30min <- temp_ammonium_effluent %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Creating a new time column with values round down to the nearest time interval
  mutate(temp_name2=
           floor_date(DATETIME,
                      unit="30 mins"))%>%
  #Removing the old time column
  select(-DATETIME) %>% 
  #Grouping by the new time column
  group_by(temp_name2) %>%
  #Overwrite all the existing columns with the average of the given time interval
  #this is done for all columns which are numeric, the na.rm=T makes the 
  #averaging robust towards missing values
  summarise(temp_column=mean(Value))%>% 
  #Convert the back to a tsibble
  as_tsibble() %>% 
  #Rename time column
  rename(time_thirty_min=temp_name2) %>% 
  #Remove duplicates
  distinct(time_thirty_min, .keep_all = T)



#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")

#The extra nitrate in the effluent data
#-------------------------------------------------------------------------------
#Load the data from the csv file with every value as a character 
temp_nitrate_effluent <- read_delim("data/extra_data_nitrate_effluent.csv", 
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
  na.omit()


#Aggregate into 30 minutes interval
temp_nitrate_effluent_30min <- temp_nitrate_effluent %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Creating a new time column with values round down to the nearest time interval
  mutate(temp_name2=
           floor_date(DATETIME,
                      unit="30 mins"))%>%
  #Removing the old time column
  select(-DATETIME) %>% 
  #Grouping by the new time column
  group_by(temp_name2) %>%
  #Overwrite all the existing columns with the average over the given time interval
  #this is done for all columns which are numeric, the na.rm=T makes the 
  #averaging robust towards missing values
  summarise(temp_column=mean(Value))%>% 
  #Convert the back to a tsibble
  as_tsibble() %>% 
  #Rename time column
  rename(time_thirty_min=temp_name2) %>% 
  #Remove duplicates
  distinct(time_thirty_min, .keep_all = T)


#Add the extra data fround in DIMS to the half hourly data frame
#-------------------------------------------------------------------------------
#Adding the extra ammonium data
data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp_ammonium_effluent_30min,
                                       ammonium_effluent_mg_L)

#Adding the extra nitrate data
data_thirty_min <- overwrite_NA_values(data_thirty_min,
                                       temp_nitrate_effluent_30min,
                                       nitrate_effluent_mg_L)
