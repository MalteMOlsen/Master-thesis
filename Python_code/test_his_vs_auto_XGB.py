#Import packages
from pathlib import Path
import pandas as pd
from xgboost import XGBRegressor
from xgboost import plot_importance
from sklearn.metrics import _regression 
from matplotlib import pyplot as plt

# Defining the evaluation metrics table and model number counter 
performance = {}
performance_hist2={}
model_n = 1
performance_hist={}
def diff(a, b):
    return a - b

def add(a, b):
    return a + b

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

#for i in range(1,49,1):  
#Forecasting horizon (target step)
forecasting_horizon=10
#Load the data
#////////////////
#Load training data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

not_adjusted_df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
not_adjusted_df_valid = pd.read_csv(data_dir_path / "validation_data_for_influent.csv",
    parse_dates=['time_thirty_min'])


seasonal_adjusted_df_training = pd.read_csv(data_dir_path / "seasonallity_adjusted_training_data_for_influent.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
seasonal_adjusted_df_valid = pd.read_csv(data_dir_path / "seasonallity_prediction_for_validation_data_for_influent.csv",
    parse_dates=['time_thirty_min'])

    #Set target
#////////////////

#for the training set 

#To remove time stamps which are NaN
temp = not_adjusted_df_training.drop("ammonium_load_AN_kg_h",axis=1)
seasonal_adjusted_df_training = pd.merge(seasonal_adjusted_df_training,
                    temp)

#Rsahping the training data
target_training_resid = seasonal_adjusted_df_training.loc[:, [target_column_resid]]
target_training_resid.loc[:,target_column_resid+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_training[target_column_resid].shift(-forecasting_horizon)
target_training_resid=target_training_resid.drop(target_column_resid, axis=1)
target_training_resid=target_training_resid.dropna()


target_training_load = not_adjusted_df_training.loc[:, [target_column_load]]
target_training_load.loc[:,target_column_load+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = not_adjusted_df_training[target_column_load].shift(-forecasting_horizon)
target_training_load=target_training_load.drop(target_column_load, axis=1)
target_training_load=target_training_load.dropna()


#for the valid set 
target_valid_load = not_adjusted_df_valid.loc[:, [target_column_load]]
target_valid_load.loc[:,target_column_load+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = not_adjusted_df_valid[target_column_load].shift(-forecasting_horizon)
target_valid_load=target_valid_load.drop(target_column_load, axis=1)
target_valid_load=target_valid_load.dropna()


#Removing columns that are not desired in the modelling
X_training = not_adjusted_df_training.loc[:, list_of_features]
X_valid = not_adjusted_df_valid.loc[:, list_of_features]
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
    target_training_resid=target_training_resid[lag_broader_one-1:]
    target_training_load=target_training_load[lag_broader_one-1:]
    seasonal_adjusted_df_training = seasonal_adjusted_df_training[lag_broader_one-1:]
    
if lag_broader_one<lag_broader_two:
    target_training_resid=target_training_resid[lag_broader_two-1:]
    target_training_load=target_training_load[lag_broader_two-1:]
    seasonal_adjusted_df_training = seasonal_adjusted_df_training[lag_broader_two-1:]

X_valid = X_valid.dropna()
X_valid = X_valid[:-forecasting_horizon]



#Defining and fitting the XGB
xgb = XGBRegressor(seed=0,tree_method="hist")
xgb.fit(X_training,
        target_training_resid) 

weight_plot = plot_importance(xgb,
                importance_type='weight', 
                max_num_features=20,
                grid =False,
                xlabel  ="Times used in a split",
                title ="Feature importance - weight - fh"+str(forecasting_horizon))


#weight_plot.figure.tight_layout()
#weight_plot.figure.savefig("Feature_importance_weight_forecasting_horizon_"+str(forecasting_horizon)+"_of_the_shelf_XGBoost_with_drought_and_seasonality.png")

gain_plot = plot_importance(xgb,
                importance_type='gain', 
                max_num_features=20,
                grid =False,
                xlabel  ="Gained obtained based on feature splitting",
                title ="Feature importance - gain - forecasting horizon"+str(forecasting_horizon))



#gain_plot.figure.tight_layout()
#gain_plot.figure.savefig("Feature_importance_gain_forecasting_horizon_"+str(forecasting_horizon)+"_of_the_shelf_XGBoost_with_drought_and_seasonality.png")

    #Prediction how well the model fits training and validation data
xgb_pred_resid               = xgb.predict(X_training)
xgb_pred_resid               = pd.DataFrame(xgb_pred_resid,columns = ['resid_pred'])
seasonal_adjusted_df_training   = seasonal_adjusted_df_training.reset_index()
seasonal_adjusted_df_training   = seasonal_adjusted_df_training.loc[:, ["fitted"]]

seasonal_adjusted_df_training.loc[:,"fitted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_training["fitted"].shift(-forecasting_horizon)
seasonal_adjusted_df_training      = seasonal_adjusted_df_training.loc[:, ["fitted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]]
    
xgb_pred_resid               = pd.concat([xgb_pred_resid,seasonal_adjusted_df_training], axis=1)
xgb_pred_resid               = xgb_pred_resid.dropna()

xgb_pred_resid['ammonium_load_AN_kg_h'] = xgb_pred_resid.apply(
    lambda x: add(x["resid_pred"], x["fitted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]), axis=1)
training_pred_load = xgb_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]

valid_pred_resid                = xgb.predict(X_valid)
valid_pred_resid                = pd.DataFrame(valid_pred_resid,columns = ['resid_pred'])

seasonal_adjusted_df_valid.loc[:,"predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_valid["predicted"].shift(-forecasting_horizon)
seasonal_adjusted_df_valid      = seasonal_adjusted_df_valid.loc[:, ["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]]

valid_pred_resid                = pd.concat([valid_pred_resid,seasonal_adjusted_df_valid], axis=1)
valid_pred_resid                = valid_pred_resid.dropna()

valid_pred_resid['ammonium_load_AN_kg_h'] = valid_pred_resid.apply(
    lambda x: add(x["resid_pred"], x["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]), axis=1)
valid_pred_load = valid_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]

target_training_load = target_training_load.reset_index()
target_training_load = target_training_load.drop("index",axis=1)
#Calculating the evalutation metrics for the training set
r2_training_set         = _regression.r2_score(target_training_load,
                                            training_pred_load)
rmse_training_set       = _regression.mean_squared_error(target_training_load,
                                                        training_pred_load,
                                                        squared=False)
mae_training_set        = _regression.mean_absolute_error(target_training_load,
                                                        training_pred_load)
median_ae_training_set  = _regression.median_absolute_error(target_training_load,
                                                            training_pred_load)
max_ae_training_set     = _regression.max_error(target_training_load,
                                                training_pred_load)

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
performance_hist[model_n] = {"r2_training"           : r2_training_set,
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
                        "forecasting_horizon"       : 10}

model_n += 1 
    
 
              
   
#Save evaluation metrics as csv
#preformace_table = pd.DataFrame.from_dict(performance).T      
#preformace_table.to_csv("XGB_default_AR_375_ammonium_and_AR_350_rain_drought_and_seasonality_accuracy_all_forecasting_horizon.csv")
