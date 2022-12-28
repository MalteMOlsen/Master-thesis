#Import packages
from pathlib import Path
import pandas as pd
from xgboost import XGBRegressor
from xgboost import plot_importance
from sklearn.metrics import _regression 

# Defining the evaluation metrics table and model number counter 
performance = {}
model_n = 1

for i in range(1,49,1):  
    #Forecasting horizon (target step)
    forecasting_horizon=i
    #Load the data
    #////////////////
    #Load training data
    data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB"  / "data_for_SMBO"
    
    X_training_path = str(forecasting_horizon)+"X_training_with_weather_forecast_with_test_set.csv"

    X_training = pd.read_csv(data_dir_path / X_training_path)

    X_training=X_training.drop("Unnamed: 0",axis=1)


    #Load validation data
    X_valid_path = str(forecasting_horizon)+"X_valid_with_weather_forecast_with_test_set.csv"

    X_valid = pd.read_csv(data_dir_path / X_valid_path)

    X_valid=X_valid.drop("Unnamed: 0",axis=1)


    target_valid_load_path = str(forecasting_horizon)+"target_valid_load_with_weather_forecast_with_test_set.csv"

    target_valid_load = pd.read_csv(data_dir_path / target_valid_load_path)

    target_valid_load=target_valid_load.drop("Unnamed: 0",axis=1)


    target_valid_seasonality_pred_path = str(forecasting_horizon)+"seasonal_adjusted_df_valid_with_weather_forecast__with_test_set.csv"

    target_valid_seasonality_pred = pd.read_csv(data_dir_path / target_valid_seasonality_pred_path)

    target_valid_seasonality_pred=target_valid_seasonality_pred.drop("Unnamed: 0",axis=1)
    target_valid_seasonality_pred = target_valid_seasonality_pred.dropna()
    


    target_training_resid_path = str(forecasting_horizon)+"target_training_resid_with_weather_forecast_with_test_set.csv"

    target_training_resid = pd.read_csv(data_dir_path / target_training_resid_path)

    target_training_resid=target_training_resid.drop("Unnamed: 0",axis=1)


    hyperparameter_path = str(forecasting_horizon)+"_XGB_ammonium_load_AN_Baysian_hyperparameter_tuning.csv"
    hyperparameter_df = pd.read_csv(data_dir_path / hyperparameter_path)
        #Set target
    #////////////////
    if i >1:
        print(i-1)
        print(r2_valid_set)
    
    

    gamma = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "gamma", 
                                  'Parameter_size']
    learning_rate = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "learning_rate", 
                                          'Parameter_size']
    max_depth = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "max_depth", 
                                          'Parameter_size']
    min_child_weight = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "min_child_weight", 
                                          'Parameter_size']
    n_estimators = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "n_estimators", 
                                          'Parameter_size']
    reg_lambda = hyperparameter_df.loc[hyperparameter_df['Parameter_name'] == "reg_lambda", 
                                          'Parameter_size']
    
    
    
    
    #Defining and fitting the XGB
    xgb = XGBRegressor( max_depth = int(max_depth), 
                        gamma = int(gamma), 
                        learning_rate = float(learning_rate),
                        min_child_weight=int(min_child_weight), 
                        n_estimators =int(n_estimators),
                        reg_lambda = int(reg_lambda),
                        booster = 'gbtree', 
                        tree_method = 'hist',
                        seed=int(0))

    
    xgb.fit(X_training,
            target_training_resid) 
    
    weight_plot = plot_importance(xgb,
                    importance_type='weight', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Times used in a split",
                    title ="Feature importance - weight - fh"+str(forecasting_horizon))
    
    
    weight_plot.figure.tight_layout()
    weight_plot.figure.savefig("Feature_importance_weight_forecasting_horizon_"+str(forecasting_horizon)+"_of_the_shelf_XGBoost_with_drought_and_seasonality.png")
    
    gain_plot = plot_importance(xgb,
                    importance_type='gain', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Gained obtained based on feature splitting",
                    title ="Feature importance - gain - forecasting horizon"+str(forecasting_horizon))
    
    
    
    gain_plot.figure.tight_layout()
    gain_plot.figure.savefig("Feature_importance_gain_forecasting_horizon_"+str(forecasting_horizon)+"_of_the_shelf_XGBoost_with_drought_and_seasonality.png")
    
    valid_pred_resid = xgb.predict(X_valid)
    valid_pred_resid                = pd.DataFrame(valid_pred_resid,columns = ['resid_pred'])
    valid_pred_resid                = pd.concat([valid_pred_resid,target_valid_seasonality_pred], 
                                            axis=1)
    valid_pred_resid                = valid_pred_resid.dropna()
    
    valid_pred_resid['ammonium_load_AN_kg_h'] = valid_pred_resid.apply(
    lambda x: x["resid_pred"]+ x["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)], axis=1)
    
    valid_pred_load = valid_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]

    #Calculating the evalutation metrics for the training set
    
    #Calculating the evalutation metrics for the validation set
    r2_valid_set            = _regression.r2_score(target_valid_load,
                                                valid_pred_load)
    rmse_valid_set          = _regression.mean_squared_error(target_valid_load,
                                                            valid_pred_load,
                                                            squared=False)
    mae_valid_set           = _regression.mean_absolute_error(target_valid_load,
                                                            valid_pred_load)
    median_ae_valid_set     = _regression.median_absolute_error(target_valid_load,
                                                                valid_pred_load)
    max_ae_valid_set        = _regression.max_error(target_valid_load,
                                                    valid_pred_load)
    
    
    
    #Saving the metrics in a preformance table
    performance[model_n] = {
                        #Evaluation metrics on the validation set
                            "r2_validation"             : r2_valid_set,
                            "rmse_validation"           : rmse_valid_set,
                            "mae_validation"            : mae_valid_set,
                            "median_ae_validation"      : median_ae_valid_set,
                            "max_ae_validation"         : max_ae_valid_set,
                            "forecasting_horizon"       : i}

    model_n += 1 
 
              
   
#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv("XGB_with_AN_tank_hyperparameters_optimasation_and_weather_forecast_on_test_set.csv")
