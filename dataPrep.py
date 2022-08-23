import os
import numpy as np
import pandas as pd
pd.options.display.precision = 4  # display option settings
pd.options.mode.chained_assignment = None  # mode settings, to ignore string errors

from sklearn.model_selection import train_test_split
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import OneHotEncoder, MinMaxScaler
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn import set_config
set_config(display="diagram")

# This python file is created for data preparation
# and data manipulation. Please also keep in mind
# that the corresponding conda virtual env is called 
# "maenv".

# set the working directory
current_path <- os.getcwd()
os.chdir(current_path)

# Now, we need to load the data.
syn_dataset <- pd.read_csv("./syn_2020-08-02_2020-08-08.csv")





