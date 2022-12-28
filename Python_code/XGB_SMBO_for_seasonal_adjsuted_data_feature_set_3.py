# import pandas for data wrangling
import pandas as pd

from pathlib import Path

# import machine learning libraries
import xgboost as xgb
from sklearn.metrics import mean_squared_error

# import packages for hyperparameters tuning
from hyperopt import STATUS_OK, Trials, fmin, hp, tpe

forecasting_horizon=1
# Defining the evaluation metrics table and model number counter 
performance = {}
model_n = 1

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
#Load the data
#////////////////
#Load training data

#Set target
#////////////////
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


seasonal_adjusted_df_valid.loc[:,"predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = seasonal_adjusted_df_valid["predicted"].shift(-forecasting_horizon)
seasonal_adjusted_df_valid      = seasonal_adjusted_df_valid.loc[:, ["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]]

space={'max_depth'          : hp.quniform("max_depth", 3, 15, 1),
        'gamma'             : hp.quniform ('gamma', 0,50,1),
        'reg_lambda'        : hp.quniform('reg_lambda', 0,25,1),
        'min_child_weight'  : hp.quniform('min_child_weight', 1, 50, 1),
        'n_estimators'      : hp.quniform('n_estimators', 50, 1500, 1),
        'seed'              : 0,
        'booster'           : 'gbtree',
        'tree_method'       : 'hist' ,
        'learning_rate'     : hp.uniform('learning_rate', 0.05,0.4)
    }


def objective(space):
    xgbregres=xgb.XGBRegressor(
                    max_depth = int(space['max_depth']), 
                    gamma = int(space['gamma']), 
                    learning_rate = float(space['learning_rate']),
                    min_child_weight=int(space['min_child_weight']), 
                    n_estimators =int(space['n_estimators']), 
                    reg_lambda = int(space['reg_lambda']),
                    booster = space['booster'], 
                    tree_method = space['tree_method'],
                    seed=int(space['seed'])
                    )
    
    
    xgbregres.fit(X_training, 
                  target_training_resid)
    

    valid_pred_resid = xgbregres.predict(X_valid)
    valid_pred_resid                = pd.DataFrame(valid_pred_resid,columns = ['resid_pred'])
    valid_pred_resid                = pd.concat([valid_pred_resid,seasonal_adjusted_df_valid], 
                                            axis=1)
    valid_pred_resid                = valid_pred_resid.dropna()
    
    valid_pred_resid['ammonium_load_AN_kg_h'] = valid_pred_resid.apply(
    lambda x: add(x["resid_pred"], x["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)]), axis=1)
    
    valid_pred_load = valid_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]
    
    
    
    accuracy = mean_squared_error(target_valid_load, 
                                  valid_pred_load,
                                  squared=False)
    print ("SCORE:", accuracy)
    return {'loss': accuracy, 'status': STATUS_OK }

trials = Trials()

best_hyperparams = fmin(fn = objective,
                        space = space,
                        algo = tpe.suggest,
                        max_evals = 250,
                        trials = trials)

safety_best_hyperparams = best_hyperparams
best_hyperparams = pd.DataFrame.from_dict(best_hyperparams, orient='index')
best_hyperparams.columns =['Parameter_size']

best_hyperparams = best_hyperparams.reset_index()
best_hyperparams.rename(columns = {'index':'Parameter_name'}, inplace = True)
best_hyperparams.loc[:,"forecasting_horizon"] = forecasting_horizon

best_hyperparams.to_csv(str(forecasting_horizon)+"_XGB_ammonium_load_AN_Baysian_hyperparameter_tuning.csv")


