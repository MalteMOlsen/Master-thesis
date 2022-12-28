
#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")

#-------------------------------------------------------------------------------
#One-Hot encoder for week days
model_data <- model_data %>% 
  mutate(weekday=weekdays(time_thirty_min)) %>% 
  mutate(monday=if_else(weekday=="mandag",
                         1,
                         0))%>% 
  mutate(tuesday=if_else(weekday=="tirsdag",
                         1,
                         0))%>% 
  mutate(wednesday=if_else(weekday=="onsdag",
                           1,
                           0))%>% 
  mutate(thursday=if_else(weekday=="torsdag",
                          1,
                          0))%>% 
  mutate(friday=if_else(weekday=="fredag",
                        1,
                        0))%>% 
  mutate(saturday=if_else(weekday=="lørdag",
                          1,
                          0))%>% 
  mutate(sunday=if_else(weekday=="søndag",
                        1,
                        0)) %>% 
  select(-weekday)

#Making the export data
model_data <- model_data %>% 
  select(time_thirty_min,
         drought,
         ammonium_load_AN_kg_h,
         rainfall_mm,
         year_measurement,
         month_measurement,
         monday,
         tuesday,
         wednesday,
         thursday,
         friday,
         saturday,
         sunday)



#For the data frame without seasonality adjustment
training_data <- model_data %>% 
  filter_index("2018-01-26"~"2021") %>% 
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h))


test_data <- model_data %>% 
  filter_index("2022-01"~"2022-03") %>% 
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h))

missing_drought <- training_data %>% 
  filter(is.na(drought))




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
         drought2="T�rkeindex") %>% 
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
  training_data <- training_data %>% 
  #Creating a column with time in a day format
  mutate(day=as.Date(time_thirty_min)) %>% 
  #Joining drought index to the data frame
  left_join(data_DMI_drought)  %>%  
  #Replace the NA values in the rain column and leave values which is not NA
  mutate(drought=if_else(is.na(drought), 
                             drought2, 
                             drought))%>% 
  #Removing the day time column 
  select(-day)













#-------------------------------------------------------------------------------
#Saving model data
#---------
#save model data without 

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(training_data, 
          "training_data_for_influent.csv")

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB/Influent_modelling")

#Save the data as a csv file
write_csv(test_data, 
          "validation_data_for_influent.csv")

