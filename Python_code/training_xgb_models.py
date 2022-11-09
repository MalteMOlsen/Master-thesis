
#Import packages
from pathlib import Path
import pandas as pd
from xgboost import XGBRegressor
from sklearn.metrics import _regression 


for j in [1,2]:
        #Load data
        #////////////////
        #Load training data
        data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

        df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
            parse_dates=['time_thirty_min'])

        #Load validation data
        df_valid = pd.read_csv(data_dir_path / "validation_data_for_influent.csv",
            parse_dates=['time_thirty_min'])

        step = j
        ammonium_lags_max = 10

        #Set target for the training and validation set
        target_training = df_training.loc[:, ["ammonium_load_AN_kg_h"]]
        target_valid = df_valid.loc[:, ["ammonium_load_AN_kg_h"]]


        # Defining the evaluation metrics table and model number counter 
        performance = {}
        model_n = 1

        #Set model experiment broaders (For step=1 add 1 to step in broaders)
        ammonium_lags_broaders = range(step,ammonium_lags_max,1)
        for i in ammonium_lags_broaders:
            #Print modelling progress
            print(f"AR terms: {lags}")
            
            target_training = target_training.loc[lags:]

            extra_valid_data = df_training[-lags:]
                
            df_valid= pd.concat([extra_valid_data, 
                                    df_valid])
                
            df_training.loc[:,"ammonium_load_AN_kg_h"+"_lag_"+str(lags)] = df_training["ammonium_load_AN_kg_h"].shift(lags)
            df_valid.loc[:,"ammonium_load_AN_kg_h"+"_lag_"+str(lags)] = df_valid["ammonium_load_AN_kg_h"].shift(lags)
            
            df_training.loc[:,"ammonium_load_AN_kg_h_lag_"+str(step)+"_AC_"+str(lags)] = df_training["ammonium_load_AN_kg_h_lag_"+str(step)].rolling(window=accumulator).sum()
            df_valid.loc[:,"ammonium_load_AN_kg_h_lag_"+str(step)+"_AC_"+str(lags)] = df_valid["ammonium_load_AN_kg_h_lag_"+str(step)].rolling(window=accumulator).sum()
                   
            

        for lags in ammonium_lags_broaders:
                #Print modelling progress
                print(f"AR terms: {lags}")
                
                target_training = target_training.loc[lags:]

                
                extra_valid_data = X_training[-lags:]
                
                X_valid= pd.concat([extra_valid_data, 
                                    X_valid])
                
                #Lagging or accumulating the chosen column
                X_training.loc[:,"rainfall_mm"+"_lag_"+str(lags)] = X_training["rainfall_mm"].shift(lags)
                X_valid.loc[:,"rainfall_mm"+"_lag_"+str(lags)] = X_valid["rainfall_mm"].shift(lags)
