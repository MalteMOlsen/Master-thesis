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



#//////////////////////////////////////////////////////////////////////////////////////////////
#Model experiment name
#//////////////////////////////////////////////////////////////////////////////////////////////

#Set target and predictors for the training set
target_training = df_training.loc[:, ["ammonium_load_AN_kg_h"]]

predictors_training = df_training.drop(["ammonium_load_AN_kg_h"], axis=1)


# Defining the evaluation metrics table and model number counter 
performance = {}
model_n = 1

#Set model experiment broaders
broaders = range(1,10,1)



#//////////////////////////////////////////////////////////////////////////////////////////////
"""
Chose the desired modelling investigation:
    Investigating the accumulated columns:
        df_training['cum_sum'] = df_training["ammonium_load_AN_kg_h"].rolling(window=2).sum()
        
    Investigating the lagged columns:
        df_training.loc[:,"ammonium_load_AN_kg_h"+"_"+str(lags)] = df_training["ammonium_load_AN_kg_h"].shift(lags)
        df_valid.loc[:,"ammonium_load_AN_kg_h"+"_"+str(lags)] = df_valid["ammonium_load_AN_kg_h"].shift(lags)
"""
#//////////////////////////////////////////////////////////////////////////////////////////////



for lags in broaders:
        #Print modelling progress
        print(f"AR terms: {lags}")
        
        #Lagging or accumulating the chosen column
        df_training.loc[:,"ammonium_load_AN_kg_h"+"_"+str(lags)] = df_training["ammonium_load_AN_kg_h"].shift(lags)
        df_valid.loc[:,"ammonium_load_AN_kg_h"+"_"+str(lags)] = df_valid["ammonium_load_AN_kg_h"].shift(lags)
        #!!PUT THE CHOSEN INVESTIGATION METHOD HERE!!
        
        #Removing NA created in the lagging or accumlation
        df_training.dropna()
        df_valid.dropna()
        
        
        #Redefining the target to obtain same length as predictors, after NA is removed
        target_training = df_training.loc[:, ["ammonium_load_AN_kg_h"]]
        #Redefining the predictors, as new columns has been created and some row with NA has been removed
        predictors_training = df_training.drop(["ammonium_load_AN_kg_h"], axis=1)
        
        #Redefining the target to obtain same length as predictors, after NA is removed
        target_valid = df_valid.loc[:, ["ammonium_load_AN_kg_h"]]
        #Redefining the predictors, as new columns has been created and some row with NA has been removed
        predictors_valid = df_valid.drop(["ammonium_load_AN_kg_h"], axis=1)
        
        
        
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
        median_ae_valid_set     = _regression.median_absolute_error(target,
                                                                    valid_pred)
        max_ae_valid_set        = _regression.max_error(target_valid,
                                                        valid_pred)
        
        
        
        #Saving the metrics in a preformance table
        performance[model_n] = {"Model terms:"             : lags, 
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
        
        
        
#Save evaluation metrics as csv
preformace_table = pd.DataFrame.from_dict(performance).T      
preformace_table.to_csv("SOMETHING.csv")        
