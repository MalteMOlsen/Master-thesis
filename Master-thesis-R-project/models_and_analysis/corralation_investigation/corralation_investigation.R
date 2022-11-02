#///////////////////////////////////////////////////////////////////////////////
#Correlation test
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to produces tables that can be investigates
#the correlation between different parameters.

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

source("setup_model_data.R")

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis")

source("adding_lag_columns.R")

cor_data <- model_data %>% 
  as_tibble %>% 
  select(-time_thirty_min) %>% 
  na.omit()

remove(model_data)

round(cor(cor_data),
      digits = 2 # rounded to 2 decimals
)
ct <- round(cor(cor_data),
      digits = 2 # rounded to 2 decimals
)

ct2 <- ct %>% as.data.frame()

ammonium_load_AN_tank_AN <- ct2$ammonium_load_AN_kg_h %>% 
  as.data.frame()
ct3 <- tibble::rownames_to_column(ct2, "Names")

ammonium_load_AN_tank_AN <- ct3 %>%
  select(ammonium_load_AN_kg_h,Names)
  as.data.frame()

#Set working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/data")
  
#Save the data as a csv file
write_csv(ct3, 
          "correlation_table.csv")

correlation_table 
ammonium_load_AN_tank_AN <- ct3 %>%
  select(Names,ammonium_load_AN_kg_h,ammonium_to_AN_mg_L, flow_AN_m3_h)
as.data.frame()


effluent <- ct3 %>%
  select(Names, ammonium_effluent_mg_L,total_N_effluent_mg_L)
as.data.frame()
