[![DOI](https://zenodo.org/badge/840049730.svg)](https://zenodo.org/doi/10.5281/zenodo.13774447)

# Format Ranger Random Forest Models for Google Earth Engine

## Project summary

Google Earth Engine (GEE) is a cloud-based platform that allows researchers access to an unprescedented archive of remotely sensed data, as well as the computing power and infrastructure to perform large scale geospatial analysis. As such, GEE has become indispensible for ecological research. However, GEE is not optmized to work with vector data and has limited (albeit growing) options for training and validating machine learning models.

In contrast, R has been used by ecological researchers for decades and has a robust ecosystem of packages and resources for training, tuning and applying machine learning models, including the 'tidymodels' and 'caret' frameworks.

Thus, researchers often find themselves needing to move between R and GEE over the course of a modeling workflow. For example, a researcher might capitalize on GEE's raster processing power to extract predictor variables across a set of training data, then bring the extracted data into R to train and validate a series of models. Ideally, these models could then be applied in GEE across the full raster domain. However, a common sticking point is formatting R-based models in a way that is readable by GEE's javascript based interface. 

This repository includes scripts and sample data to take random forest models fitted via the ranger or randomForest package in R and convert them into the 'tree' based string format required for import into GEE. The code presented here is modified from the 'reprtree' package available here: https://github.com/araastat/reprtree/tree/master

## How to use

### 1. Gather files

The script **0_format_ranger_for_gee.R** performs the random forest conversion. To run this script, users will need a random forest model fitted using either the 'ranger' or 'randomForest' package in R.

The script provides functionality to fit a test model, or users can load an existing model saved as an .RDS file.

### 2. Set parameters

In the script, set parameters to your specification:

- *model_generation_method*: Choose whether to fit an example model ('fit') or load an existing model ('load').
- *model_type*: Choose which package was used or will be used to fit the model -- 'ranger' or 'randomForest.'
- *response_type*: Choose 'classification', 'regression' or 'probability' -- note that 'probability' forests are only available via ranger.
- *model_fit_package*: Specify which R package was used to built the model. Choose 'base' for models fit directly with ranger or randomForest, choose 'caret' for models fit using the caret framework, choose 'tidymodels' for models fit using the tidymodels framework.
- *out_mod_prefix*: Specify a prefix to use when saving the formatted model. This name will be combined with information on model and response type to create a final model name.
- *out_path*: Provide an output path where formatted forest will be saved.

If loading an existing model:

- *in_path*: Provide the input path where the fitted model is saved as a .rds file.

NOTE: when providing an existing model, still populate all other parameters to ensure the model is processed properly.

### 3. Run script 

The output will be a .txt file with the random forest model formatted for input into GEE.

### 4. Import into Google Earth Engine

The script **1_apply_model_in_gee.R** contains javascript pseduo-code for importing the model into GEE and applying it across a stack of predictor variables.
