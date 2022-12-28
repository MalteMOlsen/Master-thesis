#Outline:
"""Importer pakker.
load data
fjern tids kolonne
Set target, og predictors
"""

#Import packages
from pathlib import Path
import pandas as pd
from xgboost import XGBRegressor
from sklearn.metrics._regression import mean_absolute_error
import xgboost
#Load data
data_dir_path = Path(__file__).parent.parent / "Master-thesis-R-project" / "data_for_XGB" / "Influent_modelling"

df = pd.read_csv(data_dir_path / "test.csv")


#Remove time column
df = df.drop('time_thirty_min', axis=1)

#Select target
target = df["ammonium_load_AN_kg_h"]

predictors = df.drop(["ammonium_load_AN_kg_h","weekday"], axis=1)

xgb = XGBRegressor()
xgb.fit(predictors,target)

xgb.max_depth

training_pred = xgb.predict(predictors)

mean_absolute_error(target,training_pred)


performance = {}

n_trees_grid = range(1,100,10)
learning_rate_grid = [0.1, 0.5, 0.6]

model_n = 0
for n_trees in n_trees_grid:
    for learning_rate in learning_rate_grid:
        print(f"N trees: {n_trees}, learning_rate: {learning_rate}")
        xgb = XGBRegressor(n_estimators=n_trees, learning_rate=learning_rate)
        xgb.fit(predictors,target) 
        training_pred = xgb.predict(predictors)
        mae = mean_absolute_error(target,training_pred)
        performance[model_n] = {"n_trees" : n_trees, "learning_rate" : learning_rate, "mae" : mae}

        model_n += 1


df = pd.DataFrame.from_dict(performance).T
df.to_csv("results.csv", sep=";")
#Måse søt til gain i stedet for weight


# for n_trees in range(0, 100, 10):
#     xgb = XGBRegressor(n_estimators=n_trees)
#     xgb.fit(predictors,target) 
#     training_pred = xgb.predict(predictors)
#     mae = mean_absolute_error(target,training_pred)
     
#     performance[n_trees] = mae

# "https://scikit-learn.org/stable/modules/model_evaluation.html"
    
# performance_df = pd.DataFrame.from_dict([performance]).T.reset_index()
# performance_df.columns = ["n_trees", "mae"]


# performance_df.to_latex()

# class Menneske:

#     def __init__(self, height):
#         self.height = height
        
#     def height_lol(self):
#         self.height = self.height * 2

