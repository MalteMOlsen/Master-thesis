
#Load data and packages

#Load the required packages
#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
library(zoo)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

data_thirty_min <- read_csv("data/data_thirty_min.csv") %>% 
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  as_tsibble()