# import pandas for data wrangling
import pandas as pd

from pathlib import Path

# import machine learning libraries
import xgboost as xgb
from sklearn.metrics import mean_squared_error

# import packages for hyperparameters tuning
from hyperopt import STATUS_OK, Trials, fmin, hp, tpe

process_tank_number = 4 


#Set target
#////////////////
#Load the data
#////////////////
#Load training data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

X_training_path = "X_training_process_tank_"+str(process_tank_number)+".csv"

X_training = pd.read_csv(data_dir_path / X_training_path)

X_training=X_training.drop("Unnamed: 0",axis=1)


#Load validation data
X_valid_path = "X_valid_process_tank_"+str(process_tank_number)+".csv"

X_valid = pd.read_csv(data_dir_path / X_valid_path)

X_valid=X_valid.drop("Unnamed: 0",axis=1)


target_training_path = "target_training_process_tank_"+str(process_tank_number)+".csv"

target_training = pd.read_csv(data_dir_path / target_training_path)

target_training=target_training.drop("Unnamed: 0",axis=1)



target_valid_path = "target_valid_process_tank_"+str(process_tank_number)+".csv"

target_valid = pd.read_csv(data_dir_path / target_valid_path)

target_valid=target_valid.drop("Unnamed: 0",axis=1)






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
                target_training)
    

    valid_pred = xgbregres.predict(X_valid)
    
    
    
    accuracy = mean_squared_error(target_valid, 
                                valid_pred,
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
best_hyperparams.loc[:,"Process_tank_number"] = process_tank_number

best_hyperparams.to_csv("process_tank_"+str(process_tank_number)+"_XGB_ammonium_load_SMBO_hyperparameters.csv")


