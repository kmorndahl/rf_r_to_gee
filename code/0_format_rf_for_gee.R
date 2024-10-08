# SCRIPT METADATA ==========================================================================================
# Description: R script to format a random forest model fit in 'ranger' or 'randomForest' for upload to Google Earth Engine
# Script Author: Kathleen Orndahl & Patrick Burns
# Script Date: 2024-08-30

# NOTES:

# This code has not been robustly tested on all use cases and may require some troubleshooting depending on the parameters used in creating the models.
# The script can correctly encode categorical variables, but note that categorical variables are not currently supported by GEE models (unless they are one hot encoded, see below).
# For use in GEE, categorical predictors can be one hot encoded. In this instance, each categorical level should be treated as a separate, numerical predictor during model building.

# SET UP ====================================================================================================

# Libraries
library(dplyr)
library(tidyr)
library(ranger)
library(randomForest)
library(caret)
library(tidymodels)
source('code/reprtree_update.R')
options(scipen=999) # Ensure numbers are not represented in scientific notation

# Parameters
model_generation_method = 'fit' # Choose 'fit' to fit an example model, choose 'load' to load existing model
model_type = 'randomForest' # Specify R package used to create model - 'randomForest' or 'ranger'
response_type = 'regression' # Choose 'classification' or 'probability' or 'regression'
model_fit_package = 'base' # Choose 'base' for models fit directly with 'randomForest' or 'ranger', choose 'caret' for models fit using 'caret', choose 'tidymodels' for models fit using tidymodels
out_mod_prefix = 'mtcars_' # Name prefix to use for output model
out_path = 'example_output/' # Where to save the formatted forest

# 'load' parameters
# Note: if loading a model, still provide the parameters above to ensure model is processed properly
in_path = 'data/mtcars_ranger_regression.rds'

# FIT OR LOAD MODEL ====================================================================================================

# Use example models, or load existing model
if(model_generation_method == 'fit'){
  
  rf = fit.test.mod(model_type, model_fit_package, response_type, seed = 12)
  
}else if(model_generation_method == 'load'){
  
  rf = readRDS(in_path)
  
}else(stop("Model generation method not recognized, choose 'fit' or 'load'"))

# CONVERT FOREST ====================================================================================================

# Prep model
rf = prep.mod(rf, model_type, response_type, model_fit_package)

# Convert forest
convert.forest(rf, paste0(out_path, out_mod_prefix, model_type, '_', response_type, '_', model_fit_package, '.txt'))
