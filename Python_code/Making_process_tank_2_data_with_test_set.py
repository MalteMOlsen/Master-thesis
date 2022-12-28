# import pandas for data wrangling
import pandas as pd

from pathlib import Path
 

data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB"
  
training_df = pd.read_csv(data_dir_path / "training_data_for_process_tanks_test_set_runs.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
test_df = pd.read_csv(data_dir_path / "test_data.csv",
    parse_dates=['time_thirty_min'])

 
# iterating the columns
for col in test_df.columns:
    print(col) 
#Column
#Target column
target_column = "ammonium_load_PT2_kg_h"

#List of features  
list_of_features=["drought",
                  "month",
                  "rainfall_mm",
                  "SS_PT4_g_L",
                  "ammonium_load_PT3_kg_h",
                  "T_PT2_C",
                  "airflow_PT2_m3_h"]


list_of_features_to_be_lagged = ["rainfall_mm",
                  "SS_PT4_g_L",
                  "ammonium_load_PT3_kg_h",
                  "T_PT2_C",
                  "airflow_PT2_m3_h"]

#Feature one to be lagged 
lag_broader = 350+1


#////////////////
target_training = training_df.loc[:, [target_column]]

target_test = test_df.loc[:, [target_column]]


#Removing columns that are not desired in the modelling
X_training = training_df.loc[:, list_of_features]
X_test = test_df.loc[:, list_of_features]

extra_test_data = X_training[-lag_broader+1:]

X_test= pd.concat([extra_test_data, 
                    X_test])
        

for feature in list_of_features_to_be_lagged:
    for lag in range(1,lag_broader,1):
        #Print modelling progress
        print(f"AR terms: {lag} and {feature}")
        
        #Lagging or accumulating the chosen column
        X_training.loc[:,feature+"_lag_"+str(lag)] = X_training[feature].shift(lag)
        X_test.loc[:,feature+"_lag_"+str(lag)] = X_test[feature].shift(lag)
        
        
X_training = X_training.dropna()
#X_training = X_training[:-forecasting_horizon]

target_training=target_training[lag_broader-1:]

X_test = X_test.dropna()
#X_valid = X_valid[:-forecasting_horizon]

X_training.to_csv("X_training_process_tank_2_with_test_set.csv")
X_test.to_csv("X_test_process_tank_2_with_test_set.csv")
target_training.to_csv("target_training_process_tank_2_with_test_set.csv")
target_test.to_csv("target_valid_process_tank_2_with_test_set.csv")
