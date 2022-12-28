from pathlib import Path
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.feature_selection import SequentialFeatureSelector

#User defined parameters
#///////////////
# Defining the evaluation metrics table and model number counter 
#Forecasting horizon (target step)


sfs_feature=50


sfs_direction = 'forward'
sfs_cv = 5
sfs_scoring = 'r2'

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

#1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48#
for i in [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48]:
    
    forecasting_horizon=i
    
    #Load the data
    #////////////////
    #Load training data
    #data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"
    data_dir_path = Path(__file__).parent

    df_training = pd.read_csv(data_dir_path / "training_data_for_influent.csv",
        parse_dates=['time_thirty_min'])

    #Set target
    #////////////////

    #for the training set 
    target_training = df_training.loc[:, [target_column]]
    target_training.loc[:,target_column+"_target_forecasting_horizon_of_"+str(forecasting_horizon)] = df_training[target_column].shift(-forecasting_horizon)
    target_training=target_training.drop(target_column, axis=1)
    target_training=target_training.dropna()


    #Removing columns that are not desired in the modelling
    X_training = df_training.loc[:, list_of_features]
    #Load validation data

    for lag in range(1,lag_broader_one,1):
            #Print modelling progress
            print(f"AR terms: {lag}")
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,lag_feature_one+"_lag_"+str(lag)] = X_training[lag_feature_one].shift(lag)
            
    
    for lag in range(1,lag_broader_two,1):
            #Print modelling progress
            print(f"AR terms: {lag}")
            
            #Lagging or accumulating the chosen column
            X_training.loc[:,lag_feature_two+"_lag_"+str(lag)] = X_training[lag_feature_two].shift(lag)        
    
            
    X_training = X_training.dropna()
    X_training = X_training[:-forecasting_horizon]
    
    if lag_broader_one>lag_broader_two:
        target_training=target_training[lag_broader_one-1:]
        
    if lag_broader_one<lag_broader_two:
        target_training=target_training[lag_broader_two-1:]



    ################################ Functions #############################################################
    #Inputs
    # data - Input feature data 
    # train_target - Target variable training data
    # sfs_feature - no. of features to select
    # sfs_direction -  forward and backward selection
    # sfs_cv - cross-validation splitting strategy
    # sfs_scoring - CV performance scoring metric    
    regressor = LinearRegression()    
    sfs=SequentialFeatureSelector(estimator = regressor,
                                    n_features_to_select=sfs_feature,   
                                    direction = sfs_direction,
                                    cv = sfs_cv,
                                    scoring = sfs_scoring)
    sfs.fit(X_training, target_training)

    sfs.get_support()    
    sfs_df = pd.DataFrame(columns = ['Feature', 'SFS_filter'])
    sfs_df['Feature'] = X_training.columns
    sfs_df['SFS_filter'] = sfs.get_support().tolist()    
    sfs_df_v2 = sfs_df[sfs_df['SFS_filter']==True]
    sfs_top_features = sfs_df_v2['Feature'].tolist()

    sfs_top_features_df=pd.DataFrame(sfs_top_features\
                                        ,columns = ['Feature'])
    sfs_top_features_df['Forecasting_horizon']=forecasting_horizon
    sfs_top_features_df['Numbers_of_parameters']=sfs_feature

    # saving the dataframe 
    sfs_top_features_df.to_csv("Sequential_feature_selection_forecasting_horizon_"+str(forecasting_horizon)+"_with_"+str(sfs_feature)+"_numbers_of_parameters.csv")


