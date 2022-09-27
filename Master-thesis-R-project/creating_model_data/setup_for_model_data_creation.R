#///////////////////////////////////////////////////////////////////////////////
#Setup for models data creation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is setup the one minute data frame which is then 
#used to created the hopefully cleaned model data

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#-------------------------------------------------------------------------------
#Load the packages
source("load_packages.R")


#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#-------------------------------------------------------------------------------
#Load the packages
source("load_all_data.R")

remove(data_five_min)
remove(data_fifteen_min)
remove(data_thirty_min)
remove(data_hour)

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Load the packages
source("adding_extra_DIMS_data_to_the_one_minute_data_frame.R")

remove(temp_ammonium_effluent)
remove(temp_nitrate_effluent)

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/creating_model_data")
#-------------------------------------------------------------------------------
#Load the packages
source("selecting_data_columns_and_lagging_values.R")


#Defining the theme
theme_malte <- function(){
  theme_light()+
    theme(
      axis.line = element_line(),
      panel.border = element_blank())
}

