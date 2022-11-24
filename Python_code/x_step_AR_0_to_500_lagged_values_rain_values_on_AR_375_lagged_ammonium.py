#Import packages
from pathlib import Path
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import _regression 

"""To does when training the models:
1. select the correct csv file for traing and validation data.
2. Rename section describing the model experiment.
3. Select the predictors.
4. Select the experiment broaders.
5. Select to investigate lagging or accumulation.
6. Replace the desired investigation function in the for loop.  
"""
#1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48
#1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48
for j in [1]:
    #Load data
    #////////////////
    #Load training data
    data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

    df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
        parse_dates=['time_thirty_min'])

    #Load validation data
    df_valid = pd.read_csv(data_dir_path / "validation_data_for_influent.csv",
        parse_dates=['time_thirty_min'])

    iteration_step = 1
    step = j
    iteration_length = 500
    print(step)
    #Set target for the training and validation set
    target_training = df_training.loc[:, ["ammonium_load_AN_kg_h"]]
    target_valid = df_valid.loc[:, ["ammonium_load_AN_kg_h"]]
    
    #Removing columns that are not desired in the modelling
    X_training = df_training.loc[:, ["ammonium_load_AN_kg_h","rainfall_mm"]]
    #Load validation data
    X_valid = df_valid.loc[:, ["ammonium_load_AN_kg_h","rainfall_mm"]]
    
    for ammonium_lag in range(step,376,1):
            #Print modelling progress
            print(f"AR terms: {ammonium_lag}")
            
            
            extra_valid_data2 = X_training[-ammonium_lag:]
            
            X_valid= pd.concat([extra_valid_data2, 
                                X_valid])
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,"ammonium_load_AN_kg_h"+"_lag_"+str(ammonium_lag)] = X_training["ammonium_load_AN_kg_h"].shift(ammonium_lag)
            X_valid.loc[:,"ammonium_load_AN_kg_h"+"_lag_"+str(ammonium_lag)] = X_valid["ammonium_load_AN_kg_h"].shift(ammonium_lag)
            


    X_training = X_training.drop("ammonium_load_AN_kg_h", axis=1)
    X_valid = X_valid.drop("ammonium_load_AN_kg_h", axis=1)



    #//////////////////////////////////////////////////////////////////////////////////////////////
    #Model experiment name
    #//////////////////////////////////////////////////////////////////////////////////////////////

    # Defining the evaluation metrics table and model number counter 
    performance = {}
    model_n = 1

    #Set model experiment broaders (For step=1 add 1 to step in broaders)
    broaders = range(step,iteration_length,iteration_step)


    for lags in broaders:
            #Print modelling progress
            print(f"AR rain terms: {lags}")
            
            if lags<375:
                target_training = target_training.loc[375:]
            else:
                target_training = target_training.loc[lags:]

            
            extra_valid_data = X_training[-lags:]
            
            X_valid= pd.concat([extra_valid_data, 
                                X_valid])
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,"rainfall_mm"+"_lag_"+str(lags)] = X_training["rainfall_mm"].shift(lags)
            X_valid.loc[:,"rainfall_mm"+"_lag_"+str(lags)] = X_valid["rainfall_mm"].shift(lags)
            
            
            #Removing NA created in the lagging or accumlation                    
            predictors_training = X_training.dropna()
            predictors_training = predictors_training.drop("rainfall_mm", axis=1)
            
            predictors_valid = X_valid.dropna()
            predictors_valid = predictors_valid.drop("rainfall_mm", axis=1)
            predictors_valid = predictors_valid.sort_index()
            predictors_valid = predictors_valid[0:4318]
            
            
            #Defining and fitting the linear regression
            linear = LinearRegression()
            linear.fit(predictors_training,
                    target_training) 
            
            #Prediction how well the model fits training and validation data
            training_pred    = linear.predict(predictors_training)
            valid_pred       = linear.predict(predictors_valid)
            
            
            #Calculating the evalutation metrics for the training set
            r2_training_set         = _regression.r2_score(target_training,
                                                        training_pred)
            rmse_training_set       = _regression.mean_squared_error(target_training,
                                                                    training_pred,
                                                                    squared=False)
            mae_training_set        = _regression.mean_absolute_error(target_training,
                                                                    training_pred)
            median_ae_training_set  = _regression.median_absolute_error(target_training,
                                                                        training_pred)
            max_ae_training_set     = _regression.max_error(target_training,
                                                            training_pred)
            
            #Calculating the evalutation metrics for the validation set
            r2_valid_set            = _regression.r2_score(target_valid,
                                                        valid_pred)
            rmse_valid_set          = _regression.mean_squared_error(target_valid,
                                                                    valid_pred,
                                                                    squared=False)
            mae_valid_set           = _regression.mean_absolute_error(target_valid,
                                                                    valid_pred)
            median_ae_valid_set     = _regression.median_absolute_error(target_valid,
                                                                        valid_pred)
            max_ae_valid_set        = _regression.max_error(target_valid,
                                                            valid_pred)
            
            
            
            #Saving the metrics in a preformance table
            performance[model_n] = {"feature_delay"         : lags, 
                                    "r2_training"           : r2_training_set,
                                    "rmse_training"         : rmse_training_set,
                                    "mae_training"          : mae_training_set,
                                    "median_ae_training"    : median_ae_training_set,
                                    "max_ae_training"       : max_ae_training_set,
                                #Evaluation metrics on the validation set
                                    "r2_validation"             : r2_valid_set,
                                    "rmse_validation"           : rmse_valid_set,
                                    "mae_validation"            : mae_valid_set,
                                    "median_ae_validation"      : median_ae_valid_set,
                                    "max_ae_validation"         : max_ae_valid_set,
                                    "forecasting_horizon"       : j}

            model_n += 1 
            print(mae_training_set)
        
            
            
    #Save evaluation metrics as csv
    preformace_table = pd.DataFrame.from_dict(performance).T      
    preformace_table.to_csv(str(step)+"_step_forecast_AR_0_to_"+str(iteration_length)+"_lagged_rainfall_with_AR_375_ammonium_lag.csv")




