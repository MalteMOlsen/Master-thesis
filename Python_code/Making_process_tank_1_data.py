# import pandas for data wrangling
import pandas as pd

from pathlib import Path


data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB"

training_df = pd.read_csv(data_dir_path / "training_data_for_process_tanks.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
valid_df = pd.read_csv(data_dir_path / "validation_data_for_process_tanks.csv",
    parse_dates=['time_thirty_min'])
 
# iterating the columns
for col in training_df.columns:
    print(col) 
#Column
#Target column
target_column = "ammonium_load_PT1_kg_h"

#List of features  
list_of_features=["drought",
                  "month",
                  "rainfall_mm",
                  "ammonium_load_PT2_kg_h",
                  "T_PT1_C",
                  "airflow_PT1_m3_h",
                  "SS_PT1_g_L",
                  "flow_HT_m3_h"]


list_of_features_to_be_lagged = ["rainfall_mm",
                  "ammonium_load_PT2_kg_h",
                  "T_PT1_C",
                  "airflow_PT1_m3_h",
                  "SS_PT1_g_L",
                  "flow_HT_m3_h"]

#Feature one to be lagged 
lag_broader = 350+1


#////////////////


#

#Resahping the training data
target_training = training_df.loc[:, [target_column]]
#target_training.loc[:,target_column+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = training_df[target_column].shift(-forecasting_horizon)
#target_training=target_training.drop(target_column, axis=1)
#target_training=target_training.dropna()

target_valid = valid_df.loc[:, [target_column]]
#target_valid.loc[:,target_column+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = valid_df[target_column].shift(-forecasting_horizon)
#target_valid=target_valid.drop(target_column, axis=1)
#target_valid=target_valid.dropna()



#Removing columns that are not desired in the modelling
X_training = training_df.loc[:, list_of_features]
X_valid = valid_df.loc[:, list_of_features]

extra_valid_data = X_training[-lag_broader+1:]

X_valid= pd.concat([extra_valid_data, 
                    X_valid])
        

for feature in list_of_features_to_be_lagged:
    for lag in range(1,lag_broader,1):
        #Print modelling progress
        print(f"AR terms: {lag} and {feature}")
        
        #Lagging or accumulating the chosen column
        X_training.loc[:,feature+"_lag_"+str(lag)] = X_training[feature].shift(lag)
        X_valid.loc[:,feature+"_lag_"+str(lag)] = X_valid[feature].shift(lag)
        
        
X_training = X_training.dropna()
#X_training = X_training[:-forecasting_horizon]

target_training=target_training[lag_broader-1:]

X_valid = X_valid.dropna()
#X_valid = X_valid[:-forecasting_horizon]


X_training.to_csv("X_training_process_tank_1.csv")
X_valid.to_csv("X_valid_process_tank_1.csv")
target_training.to_csv("target_training_process_tank_1.csv")
target_valid.to_csv("target_valid_process_tank_1.csv")
