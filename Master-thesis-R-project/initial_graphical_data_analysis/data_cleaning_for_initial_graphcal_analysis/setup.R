#///////////////////////////////////////////////////////////////////////////////
#Set-up script
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to load the setup the required data and load the
#required packages



#Load the required packages
#-------------------------------------------------------------------------------
#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
library(zoo)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Import other script to setup the code properly 
#-------------------------------------------------------------------------------
#Import loaded data
source("load_half_hourly_data.R")

#Insert the additional data found in DIMS
source("adding_extra_DIMS_data.R")

#Import data cleaning
source("data_cleaning_remove_NA.R")



