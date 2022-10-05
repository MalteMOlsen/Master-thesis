#///////////////////////////////////////////////////////////////////////////////
#Set up model data 
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create a tsibble with the half hourly data 
#read from a cleaned up csv file

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load the packages
source("load_packages.R")


#set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")
#Load the data
source("load_model_data.R")


#Defining the theme
theme_malte <- function(){
  theme_light()+
    theme(
      axis.line = element_line(),
      panel.border = element_blank())
}


