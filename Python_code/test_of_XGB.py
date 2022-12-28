# import pandas for data wrangling
import pandas as pd

# import numpy for Scientific computations
import numpy as np

from pathlib import Path

# import machine learning libraries
import xgboost as xgb
from sklearn.metrics import mean_squared_error

# import packages for hyperparameters tuning
from hyperopt import STATUS_OK, Trials, fmin, hp, tpe

#Forecasting horizon (target step)
forecasting_horizon=1
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

#for i in range(1,49,1):  

#Load the data
#////////////////
#Load training data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

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


X_training = X_training[0:200]
target_training = target_training[0:200]


space={'max_depth'          : hp.quniform("max_depth", 3, 15, 1),
        'gamma'             : hp.uniform ('gamma', 0,50),
        'reg_lambda'        : hp.uniform('reg_lambda', 0,10),
        'min_child_weight'  : hp.quniform('min_child_weight', 1, 50, 1),
        'n_estimators'      : hp.quniform('n_estimators', 50, 1000, 1),
        'seed'              : 0,
        'booster'           : 'gbtree',
        'tree_method'       : 'hist' ,
        'learning_rate'     : hp.uniform('learning_rate', 0.05,0.4)
    }

def objective(space):
    xgbregres=xgb.XGBRegressor(
                    max_depth = int(space['max_depth']), gamma = int(space['gamma']), learning_rate = float(space['learning_rate']),
                     min_child_weight=int(space['min_child_weight']), 
                    n_estimators =int(space['n_estimators']), booster = space['booster'], tree_method = space['tree_method']
                    )
    
    
    xgbregres.fit(X_training, target_training)
    

    pred = xgbregres.predict(X_valid)
    accuracy = mean_squared_error(target_valid, 
                                  pred,
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

best_hyperparams.to_csv(str(forecasting_horizon)+"XGB_ammonium_load_AN_Baysian_hyperparameter_tuning.csv")


