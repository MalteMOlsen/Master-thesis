#Import packages
from pathlib import Path
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import _regression 

# Defining the evaluation metrics table and model number counter 
performance = {}
model_n = 1

#Column
#Target column
target_column = "ammonium_load_AN_kg_h"

#List of features  
list_of_features=["ammonium_load_AN_kg_h","rainfall_mm"]


#Feature one to be lagged 
lag_feature_one = "ammonium_load_AN_kg_h"
lag_broader_one = 375+1

#Feature one to be lagged
lag_feature_two = "rainfall_mm"
lag_broader_two = 350+1

for i in range(1,49,1):  
    #Forecasting horizon (target step)
    forecasting_horizon=i
    #Load the data
    #////////////////
    #Load training data
    data_dir_path = Path(__file__).parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

    df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
        parse_dates=['time_thirty_min'])

    #Load validation data
    df_valid = pd.read_csv(data_dir_path / "validation_data_for_influent.csv",
        parse_dates=['time_thirty_min'])

    #Set target
    #////////////////

    #for the training set 
    target_training = df_training.loc[:, [target_column]]
    target_training.loc[:,target_column+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = df_training[target_column].shift(-forecasting_horizon)
    target_training=target_training.drop(target_column, axis=1)
    target_training=target_training.dropna()

    #for the valid set 
    target_valid = df_valid.loc[:, [target_column]]
    target_valid.loc[:,target_column+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = df_valid[target_column].shift(-forecasting_horizon)
    target_valid=target_valid.drop(target_column, axis=1)
    target_valid=target_valid.dropna()




    #Removing columns that are not desired in the modelling
    X_training = df_training.loc[:, list_of_features]
    X_valid = df_valid.loc[:, list_of_features]
    #Load validation data



    if lag_broader_one>lag_broader_two:
        extra_valid_data = X_training[-lag_broader_one+1:]
    if lag_broader_one<lag_broader_two:
        extra_valid_data = X_training[-lag_broader_two+1:]
    
    X_valid= pd.concat([extra_valid_data, 
                        X_valid])
            

    for lag in range(1,lag_broader_one,1):
            #Print modelling progress
            print(f"AR terms: {lag}")
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,lag_feature_one+"_lag_"+str(lag)] = X_training[lag_feature_one].shift(lag)
            X_valid.loc[:,lag_feature_one+"_lag_"+str(lag)] = X_valid[lag_feature_one].shift(lag)
            

    for lag in range(1,lag_broader_two,1):
            #Print modelling progress
            print(f"AR terms: {lag}")
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,lag_feature_two+"_lag_"+str(lag)] = X_training[lag_feature_two].shift(lag)  
            X_valid.loc[:,lag_feature_two+"_lag_"+str(lag)] = X_valid[lag_feature_two].shift(lag)        

            
    X_training = X_training.dropna()
    X_training = X_training[:-forecasting_horizon]
    if lag_broader_one>lag_broader_two:
        target_training=target_training[lag_broader_one-1:]
    if lag_broader_one<lag_broader_two:
        target_training=target_training[lag_broader_two-1:]

    X_valid = X_valid.dropna()
    X_valid = X_valid[:-forecasting_horizon]
    
    
    #Defining and fitting the linear regression
    linear = LinearRegression()
    linear.fit(X_training,
            target_training) 
    
    #Prediction how well the model fits training and validation data
    training_pred    = linear.predict(X_training)
    valid_pred       = linear.predict(X_valid)
    
    
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
    performance[model_n] = {"r2_training"           : r2_training_set,
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
                            "forecasting_horizon"       : i}

    model_n += 1 
             sdf
   
#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv("AR_375_ammonium_and_AR_350_rain_linear_model_accuracy_all_forecasting_horizon.csv")
