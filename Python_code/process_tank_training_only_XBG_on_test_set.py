# import pandas for data wrangling
import pandas as pd

from pathlib import Path

# import machine learning libraries

from xgboost import XGBRegressor
from xgboost import plot_importance
from sklearn.metrics import _regression 

performance = {}
model_n=1
for i in [1,2,3,4]:
    process_tank_number = i


    #Set target
    #////////////////
    #Load the data
    #////////////////
    #Load training data
    data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" 

    X_training_path = "X_training_process_tank_"+str(process_tank_number)+"_with_test_set.csv"

    X_training = pd.read_csv(data_dir_path / X_training_path)

    X_training=X_training.drop("Unnamed: 0",axis=1)


    #Load validation data
    X_test_path = "X_test_process_tank_"+str(process_tank_number)+"_with_test_set.csv"

    X_test = pd.read_csv(data_dir_path / X_test_path)

    X_test=X_test.drop("Unnamed: 0",axis=1)


    target_training_path = "target_training_process_tank_"+str(process_tank_number)+"_with_test_set.csv"

    target_training = pd.read_csv(data_dir_path / target_training_path)

    target_training=target_training.drop("Unnamed: 0",axis=1)



    target_test_path = "target_valid_process_tank_"+str(process_tank_number)+"_with_test_set.csv"

    target_test = pd.read_csv(data_dir_path / target_test_path)

    target_test=target_test.drop("Unnamed: 0",axis=1)

    #data_dir_path = Path(__file__).parent / "hyperparameter_for_process_tank_modelling_only_XGB"

    hyperparameter_path = "process_tank_"+str(process_tank_number)+"_XGB_ammonium_load_SMBO_hyperparameters.csv"
    hyperparameter_df = pd.read_csv(data_dir_path / hyperparameter_path)
        #Set target
    #////////////////

    print(i)

    
    

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
    xgbregressor = XGBRegressor(max_depth = int(max_depth), 
                        gamma = int(gamma), 
                        learning_rate = float(learning_rate),
                        min_child_weight=int(min_child_weight), 
                        n_estimators =int(n_estimators),
                        reg_lambda = int(reg_lambda),
                        booster = 'gbtree', 
                        tree_method = 'hist',
                        seed=int(0))

    
    xgbregressor.fit(X_training,
            target_training) 
    
    weight_plot = plot_importance(xgbregressor,
                    importance_type='weight', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Times used in a split",
                    title ="Feature importance - weight - process tank "+str(process_tank_number))
    
    
    weight_plot.figure.tight_layout()
    weight_plot.figure.savefig("Feature_importance_weight__process_tank_"+str(process_tank_number)+"_only_XGB__on_test_set.png")
    
    gain_plot = plot_importance(xgbregressor,
                    importance_type='gain', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Gained obtained based on feature splitting",
                    title ="Feature importance - gain - process tank "+str(process_tank_number))
    
    
    
    gain_plot.figure.tight_layout()
    gain_plot.figure.savefig("Feature_importance_gain_process_tank_"+str(process_tank_number)+"_only_XGB__on_test_set.png")
    
    test_pred = xgbregressor.predict(X_test)
    #valid_pred_resid                = pd.DataFrame(valid_pred_resid,columns = ['resid_pred'])
    #valid_pred_resid                = pd.concat([valid_pred_resid,target_valid_seasonality_pred], 
    #                                        axis=1)
    #valid_pred_resid                = valid_pred_resid.dropna()
    
   #valid_pred_resid['ammonium_load_AN_kg_h'] = valid_pred_resid.apply(
    #lambda x: x["resid_pred"]+ x["predicted"+"_target_forecasting_horizon_of_"+str(forecasting_horizon)], axis=1)
    
    #valid_pred_load = valid_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]

    #Calculating the evalutation metrics for the training set
    
    #Calculating the evalutation metrics for the validation set
    r2_test_set            = _regression.r2_score(target_test,
                                                test_pred)
    rmse_test_set          = _regression.mean_squared_error(target_test,
                                                            test_pred,
                                                            squared=False)
    mae_test_set           = _regression.mean_absolute_error(target_test,
                                                            test_pred)
    median_ae_test_set     = _regression.median_absolute_error(target_test,
                                                                test_pred)
    max_ae_test_set        = _regression.max_error(target_test,
                                                    test_pred)
    
    
    
    #Saving the metrics in a preformance table
    performance[model_n] = {
                        #Evaluation metrics on the validation set
                            "r2_test"             : r2_test_set,
                            "rmse_test"           : rmse_test_set,
                            "mae_test"            : mae_test_set,
                            "median_ae_test"      : median_ae_test_set,
                            "max_ae_test"         : max_ae_test_set,
                            "Proccess_tank"       : i}
 
    model_n += 1 
 
              
   
#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv("XGB_in_the_process_tank_only_DDM_on_test_set.csv")

