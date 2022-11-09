#Import packages
from pathlib import Path
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import _regression 

"""To does when training the models:
1. select the correct csv file for traing and validation data.
2. Rename section describing the model experiment.
3. Select the predictors.
4. Select the experiment broaders.
5. Select to investigate lagging or accumulation.
6. Replace the desired investigation function in the for loop. 
"""

#Load data
#////////////////
#Load training data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
    parse_dates=['time_thirty_min'])

#Load validation data
df_valid = pd.read_csv(data_dir_path / "validation_data_for_influent.csv",
    parse_dates=['time_thirty_min'])

interation_step = 10 
step = 1


#Set target for the training and validation set
target_training = df_training.loc[:, ["ammonium_load_AN_kg_h"]]
target_training = target_training.loc[step:]
target_valid = df_valid.loc[:, ["ammonium_load_AN_kg_h"]]



#Define the column containing the last data we know
df_training.loc[:,"rainfall_mm"+"_lag_"+str(step)] = df_training["rainfall_mm"].shift(step)


extra_valid_data = df_training[-step:] 

df_valid= pd.concat([extra_valid_data, 
                        df_valid])

df_valid.loc[:,"rainfall_mm"+"_lag_"+str(step)] = df_valid["rainfall_mm"].shift(step)

df_valid = df_valid.dropna()

#Removing columns that are not desired in the modelling
X_training = df_training.loc[:, ["rainfall_mm"+"_lag_"+str(step)]]
#Load validation data
X_valid = df_valid.loc[:, ["rainfall_mm"+"_lag_"+str(step)]]


extra_X_valid_data_constant = X_training


#//////////////////////////////////////////////////////////////////////////////////////////////
#Model experiment name
#//////////////////////////////////////////////////////////////////////////////////////////////

# Defining the evaluation metrics table and model number counter 
performance = {}
model_n = 1

#Set model experiment broaders 
broaders = range(step,1000,interation_step)


for accumulator in broaders:
        #Print modelling progress
        print(f"AC terms: {accumulator}")
        
        target_training = target_training.loc[accumulator:]
        
 
        extra_valid_data = extra_X_valid_data_constant[-accumulator:]
        
        X_valid= pd.concat([extra_valid_data, 
                             X_valid])
        
        
        #Lagging or accumulating the chosen column
        X_training.loc[:,"rainfall_mm"+"_lag_"+str(step)+"_AC_"+str(accumulator)] = X_training["rainfall_mm"+"_lag_"+str(step)].rolling(window=accumulator).sum()
        X_valid.loc[:,"rainfall_mm"+"_lag_"+str(step)+"_AC_"+str(accumulator)] = X_valid["rainfall_mm"+"_lag_"+str(step)].rolling(window=accumulator).sum()
        #!!PUT THE CHOSEN INVESTIGATION METHOD HERE!!
        
        
        #Removing NA created in the lagging or accumlation
        predictors_training = X_training.dropna()
        
        predictors_valid = X_valid.dropna()
        predictors_valid = predictors_valid.sort_index()
        predictors_valid = predictors_valid[0:4318]
        
        
        #Defining and fitting the linear regression
        linear = LinearRegression()
        linear.fit(predictors_training,
                   target_training) 
        
        #Prediction how well the model fits training and validation data
        training_pred    = linear.predict(predictors_training)
        valid_pred       = linear.predict(predictors_valid)
        
        
        #Calculating the evalutation metrics for the training set
        r2_training_set         = _regression.r2_score(target_training,
                                                       training_pred)
        rmse_training_set       = _regression.mean_squared_error(target_training,
                                                                 training_pred,
                                                                 squared=False)
        mae_training_set        = _regression.mean_absolute_error(target_training,
                                                                  training_pred)
        mape_training_set       = _regression.mean_absolute_percentage_error(target_training,
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
        mape_valid_set          = _regression.mean_absolute_percentage_error(target_valid,
                                                                             valid_pred)
        median_ae_valid_set     = _regression.median_absolute_error(target_valid,
                                                                    valid_pred)
        max_ae_valid_set        = _regression.max_error(target_valid,
                                                        valid_pred)
        
        
        
        #Saving the metrics in a preformance table
        performance[model_n] = {"Model_terms"              : accumulator, 
                                "r2_training_set"          : r2_training_set,
                                "rmse_training_set"        : rmse_training_set,
                                "mae_training_set"         : mae_training_set,
                                "mape_training_set"        : mape_training_set,
                                "median_ae_training_set"   : median_ae_training_set,
                                "max_ae_training_set"      : max_ae_training_set,
                            #Evaluation metrics on the validation set
                                "r2_valid_set"             : r2_valid_set,
                                "rmse_valid_set"           : rmse_valid_set,
                                "mae_valid_set"            : mae_valid_set,
                                "mape_valid_set"           : mape_valid_set,
                                "median_ae_valid_set"      : median_ae_valid_set,
                                "max_ae_valid_set"         : max_ae_valid_set}

        model_n += 1 
        print(mae_training_set)
    
        
        
#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv(str(step)+"_step_forecast_AC_0_to_1000_accumulated_rainfall_iteration_step_"+str(interation_step)+".csv")




