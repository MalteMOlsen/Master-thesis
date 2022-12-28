

#Setup
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")


model_data <- model_data %>%
  mutate(timestamp = as.POSIXct(time_thirty_min)) %>% 
  # complete sequence to full sequence from min to max by second
  complete(timestamp = seq.POSIXt(min(timestamp), max(timestamp), by = '30 min')) 

model_data <- model_data %>% 
  select(timestamp,
         ammonium_load_AN_kg_h,
         rainfall_mm,
         drought) %>% 
  rename(time_thirty_min=timestamp) %>% 
  as_tsibble() %>% 
  filter_index("2018-01-26"~"2022-03")

total_na3=0
for (i in seq(from = 1, to = 3, by = 1)){
  
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
for (i in seq(from = 1, to = 3, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na4 = total_na4+nrow(temp)
  
  print("# NA in")
  print(names(model_data[,i]))
  print(nrow(temp))
}

model_data <- model_data %>%  
  mutate(rainfall_mm=na.approx(rainfall_mm))%>%    
  mutate(drought=na.approx(drought)) %>%    
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h)) 


total_na4=0
for (i in seq(from = 1, to = 3, by = 1)){
  
  temp <- model_data %>%
    filter(is.na(model_data[,i]))
  total_na4 = total_na4+nrow(temp)
  
  print("# NA in")
  print(names(model_data[,i]))
  print(nrow(temp))
}

model_data %>% nrow()
model_data %>% filter_index("2020"~"2022-03") %>% nrow()
model_data %>% filter_index("2021"~"2022-03") %>% nrow()
model_data %>% filter_index("2021-07"~"2022-03") %>% nrow()
model_data %>% filter_index("2021-10"~"2022-03") %>% nrow()


#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data_for_XGB")

#Save the data as a csv file
write_csv(model_data, 
          "training_data_for_influent_test_set_runs.csv")