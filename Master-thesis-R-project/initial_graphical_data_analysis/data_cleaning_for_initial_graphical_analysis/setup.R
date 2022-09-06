#///////////////////////////////////////////////////////////////////////////////
#Set-up script
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to load the setup the required data and load the
#required packages

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#-------------------------------------------------------------------------------
#Load the packages
source("load_packages.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Import other script to setup the code properly 
#-------------------------------------------------------------------------------
#Import loaded data
source("load_half_hourly_data.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Import data cleaning
source("data_cleaning_remove_NA.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Insert the additional data found in DIMS
source("adding_extra_DIMS_data.R")





