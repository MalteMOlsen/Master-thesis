# import pandas for data wrangling
import pandas as pd
from pathlib import Path
from sklearn.metrics import _regression 

#First set-up 
#performance = {}

#Set up each run

process_tank="process_tank_1"

SS_col = "SS_PT1_g_L"

ammonium_load_inlet = "ammonium_load_PT2_kg_h"

target = "ammonium_load_PT1_kg_h"

model_n =4


pred_col = "pred_ammonium_out_"+str(process_tank)+"_kg_h"

#Load training data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB"

training_data = pd.read_csv(data_dir_path / "training_data_for_process_tanks.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
valid_data = pd.read_csv(data_dir_path / "validation_data_for_process_tanks.csv",
    parse_dates=['time_thirty_min'])


kg_N_removed_pr_gSS_and_h = (0.09563/4.22)*60/1000/1000

volume_of_process_tank = 5000


training_data['mass_of_ss_in_process_tank_g'] = training_data.apply(
    lambda x: x[SS_col]*5000*1000, axis=1)


training_data['NH4_removed_kg_h'] = training_data.apply(
    lambda x: x["mass_of_ss_in_process_tank_g"]*kg_N_removed_pr_gSS_and_h, axis=1)

training_data[pred_col] = training_data.apply(
    lambda x: x[ammonium_load_inlet]-x["NH4_removed_kg_h"], axis=1)


#Select target

valid_data['mass_of_ss_in_process_tank_g'] = valid_data.apply(
    lambda x: x[SS_col]*5000*1000, axis=1)


valid_data['NH4_removed_kg_h'] = valid_data.apply(
    lambda x: x["mass_of_ss_in_process_tank_g"]*kg_N_removed_pr_gSS_and_h, axis=1)

valid_data[pred_col] = valid_data.apply(
    lambda x: x[ammonium_load_inlet]-x["NH4_removed_kg_h"], axis=1)
#Select target

target_training = training_data.loc[:, [target]]
pred_training = training_data.loc[:, pred_col]

target_valid = valid_data.loc[:, [target]]
pred_valid = valid_data.loc[:, pred_col]



#Calculating the evalutation metrics for the training set
r2_training_set         = _regression.r2_score(target_training,
                                            pred_training)
rmse_training_set       = _regression.mean_squared_error(target_training,
                                                        pred_training,
                                                        squared=False)
mae_training_set        = _regression.mean_absolute_error(target_training,
                                                        pred_training)
median_ae_training_set  = _regression.median_absolute_error(target_training,
                                                            pred_training)
max_ae_training_set     = _regression.max_error(target_training,
                                                pred_training)

#Calculating the evalutation metrics for the validation set
r2_valid_set            = _regression.r2_score(target_valid,
                                            pred_valid)
rmse_valid_set          = _regression.mean_squared_error(target_valid,
                                                        pred_valid,
                                                        squared=False)
mae_valid_set           = _regression.mean_absolute_error(target_valid,
                                                        pred_valid)
median_ae_valid_set     = _regression.median_absolute_error(target_valid,
                                                            pred_valid)
max_ae_valid_set        = _regression.max_error(target_valid,
                                                pred_valid)



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
                        "Process_tank"              : process_tank}



#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv("Performance_of_only_the_lab_expression.csv")