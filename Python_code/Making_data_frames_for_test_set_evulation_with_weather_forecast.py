# import pandas for data wrangling
import pandas as pd

from pathlib import Path

for i in range(1,49,1):
    forecasting_horizon=i
    # Defining the evaluation metrics table and model number counter 

    #Column
    #Target column
    target_column_resid = "resid"
    target_column_load = "ammonium_load_AN_kg_h"

    #List of features  
    list_of_features=["ammonium_load_AN_kg_h","rainfall_mm","drought"]


    #Feature one to be lagged 
    lag_feature_one = "ammonium_load_AN_kg_h"
    lag_broader_one = 375+1
    

    #Feature one to be lagged
    lag_feature_two = "rainfall_mm"
    lag_broader_two = 350+1
    #Load the data
    #////////////////
    #Load training data

    #Set target
    #////////////////
    #Load the data
    #////////////////
    #Load training data
    data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" 
    
    not_adjusted_df_training = pd.read_csv(data_dir_path / "training_data_for_influent_test_set_runs.csv",
        parse_dates=['time_thirty_min'])
    for col in not_adjusted_df_training.columns:
        print(col)


    #Load validation data
    not_adjusted_df_valid = pd.read_csv(data_dir_path / "test_data.csv",
        parse_dates=['time_thirty_min'])


    seasonal_adjusted_df_training = pd.read_csv(data_dir_path / "seasonallity_adjusted_training_data_for_influent_with_validation_set_added.csv",
        parse_dates=['time_thirty_min'])

    #Load validation data
    seasonal_adjusted_df_valid = pd.read_csv(data_dir_path / "seasonallity_prediction_for_influent_of_the_test_set.csv",
        parse_dates=['time_thirty_min'])

        #Set target
    #////////////////

    #for the training set 

    #To remove time stamps which are NaN
    target_training_resid = seasonal_adjusted_df_training.loc[:, [target_column_resid]]
    target_training_resid.loc[:,target_column_resid+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_training[target_column_resid].shift(-forecasting_horizon)
    target_training_resid=target_training_resid.drop(target_column_resid, axis=1)
    target_training_resid=target_training_resid.dropna()


    #for the valid set 
    target_valid_load = not_adjusted_df_valid.loc[:, [target_column_load]]
    target_valid_load.loc[:,target_column_load+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = not_adjusted_df_valid[target_column_load].shift(-forecasting_horizon)
    target_valid_load=target_valid_load.drop(target_column_load, axis=1)
    target_valid_load=target_valid_load.dropna()


    #Removing columns that are not desired in the modelling
    X_training = not_adjusted_df_training.loc[:, list_of_features]
    X_valid = not_adjusted_df_valid.loc[:, list_of_features]
    
    for rain_forecast in range(1,forecasting_horizon+1,1):
        X_training.loc[:,"rainfall"+"_target_forecasting_horizon_of_"+str(rain_forecast)] = X_training["rainfall_mm"].shift(-rain_forecast)
    
    
    #Load validation data
    if lag_broader_one>lag_broader_two:
        extra_valid_data = X_training[-lag_broader_one+1:]
    if lag_broader_one<lag_broader_two:
        extra_valid_data = X_training[-lag_broader_two+1:]

    X_valid= pd.concat([extra_valid_data, 
                        X_valid])
      
    for rain_forecast in range(1,forecasting_horizon+1,1):
        X_valid.loc[:,"rainfall"+"_target_forecasting_horizon_of_"+str(rain_forecast)] = X_valid["rainfall_mm"].shift(-rain_forecast)
    
            

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
    #X_training = X_training[:-forecasting_horizon]

    if lag_broader_one>lag_broader_two:
        target_training_resid=target_training_resid[lag_broader_one-1:]
        seasonal_adjusted_df_training = seasonal_adjusted_df_training[lag_broader_one-1:]
        
    if lag_broader_one<lag_broader_two:
        target_training_resid=target_training_resid[lag_broader_two-1:]
        seasonal_adjusted_df_training = seasonal_adjusted_df_training[lag_broader_two-1:]

    X_valid = X_valid.dropna()
    #X_valid = X_valid[:-forecasting_horizon]

    
    seasonal_adjusted_df_valid.loc[:,"predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_valid["predicted"].shift(-forecasting_horizon)
    seasonal_adjusted_df_valid      = seasonal_adjusted_df_valid.loc[:, ["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]]
    seasonal_adjusted_df_valid=seasonal_adjusted_df_valid.dropna()
    
    
    print("1_fh"+str(forecasting_horizon))
    X_training.to_csv(str(forecasting_horizon)+"X_training_with_weather_forecast_with_test_set.csv")
    print("1_fh"+str(forecasting_horizon))
    X_valid.to_csv(str(forecasting_horizon)+"X_valid_with_weather_forecast_with_test_set.csv")
    print("2_fh"+str(forecasting_horizon))
    target_valid_load.to_csv(str(forecasting_horizon)+"target_valid_load_with_weather_forecast_with_test_set.csv")
    print("3_fh"+str(forecasting_horizon))
    seasonal_adjusted_df_valid.to_csv(str(forecasting_horizon)+"seasonal_adjusted_df_valid_with_weather_forecast_with_test_set.csv")
    print("4_fh"+str(forecasting_horizon))
    target_training_resid.to_csv(str(forecasting_horizon)+"target_training_resid_with_weather_forecast_with_test_set.csv")
    print("5_fh"+str(forecasting_horizon))
    