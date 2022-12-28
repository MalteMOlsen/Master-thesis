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

    target_training_load = pd.read_csv(data_dir_path / target_training_path)

    target_training_load = target_training_load.drop("Unnamed: 0",axis=1)



    target_test_path = "target_valid_process_tank_"+str(process_tank_number)+"_with_test_set.csv"

    target_test_load = pd.read_csv(data_dir_path / target_test_path)

    target_test_load = target_test_load.drop("Unnamed: 0",axis=1)

    data_dir_path_2 = Path(__file__).parent / "hyperparameters_for_process_tanks_hybrid_model"

    hyperparameter_path = "process_tank_"+str(process_tank_number)+"mecanistic_model_and_XGB_ammonium_load_SMBO_hyperparameters.csv"
    hyperparameter_df = pd.read_csv(data_dir_path_2 / hyperparameter_path)
    
    data_dir_path_3 = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB"
  
    data_for_mecanistic_model_training = pd.read_csv(data_dir_path_3 / "training_data_for_process_tanks_test_set_runs.csv",
    parse_dates=['time_thirty_min'])
    
    
    data_for_mecanistic_model_training = data_for_mecanistic_model_training[350:]
    data_for_mecanistic_model_training = data_for_mecanistic_model_training.reset_index(drop=True)
    
    data_for_mecanistic_model_test = pd.read_csv(data_dir_path_3 / "test_data.csv",
        parse_dates=['time_thirty_min'])
   
    if i==4:
        ammonium_load_inlet = "ammonium_load_AN_kg_h"
    else:
        ammonium_load_inlet = "ammonium_load_PT"+str(process_tank_number+1)+"_kg_h"
    
    if i==1:
        SS_col = "SS_PT1_g_L"
    else:
        SS_col = "SS_PT4_g_L"
        
   
    tem_col = "T_PT"+str(process_tank_number)+"_C"
    DO_col = "DO_PT"+str(process_tank_number)+"_mg_L"
    NH4_col = "ammonium_PT"+str(process_tank_number)+"_mg_L"
    pred_col = "pred_ammonium_out_process_tank_"+str(process_tank_number)+"_kg_h"

    
    volume_of_process_tank = 5000
    kg_N_removed_pr_gSS_and_h = (0.09563/4.22)*60/1000/1000
    mecanistic_pred_training    =  target_training_load
    mecanistic_pred_test        =  target_test_load

    data_for_mecanistic_model_training['NH4_removed_kg_h'] = data_for_mecanistic_model_training.apply(
        lambda x: x[SS_col]*5000*1000*kg_N_removed_pr_gSS_and_h*(1.123**(x[tem_col]-20))*(x[DO_col]/(x[DO_col]+0.4))*(x[NH4_col]/(x[NH4_col]+1*(1.123**(x[tem_col]-20)))), axis=1)

    data_for_mecanistic_model_training[pred_col] = data_for_mecanistic_model_training.apply(
        lambda x: x[ammonium_load_inlet]-x["NH4_removed_kg_h"], axis=1)

    data_for_mecanistic_model_test['NH4_removed_kg_h'] = data_for_mecanistic_model_test.apply(
        lambda x: x[SS_col]*5000*1000*kg_N_removed_pr_gSS_and_h*(1.123**(x[tem_col]-20))*(x[DO_col]/(x[DO_col]+0.4))*(x[NH4_col]/(x[NH4_col]+1*(1.123**(x[tem_col]-20)))), axis=1)
    

    target_training_resid = data_for_mecanistic_model_training.loc[:, [pred_col]]
    pred_test_N_removed = data_for_mecanistic_model_test.loc[:, ["NH4_removed_kg_h"]]
   
    target_col = "ammonium_load_PT"+str(process_tank_number)+"_kg_h"
    target_test_load = target_test_load.loc[:, [target_col]]
    #Set target
    #////////////////
    #if i >1:
    #    print(i-1)
    #    print(r2_test_set)
    
    

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
    xgbregressor = XGBRegressor( max_depth = int(max_depth), 
                        gamma = int(gamma), 
                        learning_rate = float(learning_rate),
                        min_child_weight=int(min_child_weight), 
                        n_estimators =int(n_estimators),
                        reg_lambda = int(reg_lambda),
                        booster = 'gbtree', 
                        tree_method = 'hist',
                        seed=int(0))

    
    xgbregressor.fit(X_training,
            target_training_resid) 
    
    weight_plot = plot_importance(xgbregressor,
                    importance_type='weight', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Times used in a split",
                    title ="Feature importance - weight - process tank "+str(process_tank_number))
    
    
    weight_plot.figure.tight_layout()
    weight_plot.figure.savefig("Feature_importance_weight__process_tank_"+str(process_tank_number)+"_hybrid_model__on_test_set.png")
    
    gain_plot = plot_importance(xgbregressor,
                    importance_type='gain', 
                    max_num_features=20,
                    grid =False,
                    xlabel  ="Gained obtained based on feature splitting",
                    title ="Feature importance - gain - process tank "+str(process_tank_number))
    
    
    
    gain_plot.figure.tight_layout()
    gain_plot.figure.savefig("Feature_importance_gain_process_tank_"+str(process_tank_number)+"_hybrid_model__on_test_set.png")
    
    test_pred_resid = xgbregressor.predict(X_test)
    test_pred_resid                = pd.DataFrame(test_pred_resid,columns = ['resid_pred'])
    test_pred_resid                = pd.concat([test_pred_resid,pred_test_N_removed], 
                                            axis=1)
    
    test_pred_resid['ammonium_load_AN_kg_h'] = test_pred_resid.apply(
        lambda x: x["resid_pred"]+ x["NH4_removed_kg_h"], axis=1)
    
    test_pred_load = test_pred_resid.loc[:, ["ammonium_load_AN_kg_h"]]

    #Calculating the evalutation metrics for the training set
    
    #Calculating the evalutation metrics for the validation set
    r2_test_set            = _regression.r2_score(target_test_load,
                                                test_pred_load)
    rmse_test_set          = _regression.mean_squared_error(target_test_load,
                                                            test_pred_load,
                                                            squared=False)
    mae_test_set           = _regression.mean_absolute_error(target_test_load,
                                                            test_pred_load)
    median_ae_test_set     = _regression.median_absolute_error(target_test_load,
                                                                test_pred_load)
    max_ae_test_set        = _regression.max_error(target_test_load,
                                                    test_pred_load)
    
    
    
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
preformace_table.to_csv("Hybrid_model_in_the_process_tank_on_test_set.csv")

