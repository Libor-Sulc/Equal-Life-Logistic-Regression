#######################################
# Set the working directory
#######################################
# Instead of "C:/Users/libsul/R/EQL/Logit_code" write pathway to
# local directory you are working in
# NOTE: R may set working directory automatically
setwd("C:/Users/...")

#######################################
# Install/Load packages
#######################################
source("Scripts/0_library.R")

#######################################
# Set data type
#######################################
# Create "variable_type.xlsx" in the root directory
source("Scripts/1_data.R")
# Inspect the "variable_type.xlsx" file

# "variable_type" sheet:
# Change change variable type as needed (column "Type")
# "C" = continuous variable (e.g., length of streets, population density, NO2 levels)
# "O" = ordered categorical/binary variable (e.g., smoking, sex, education)
# By writing "1" into appropriate cell you can
# select dependent variable = ADHD/Autism (column "Dependent_Variable")
# select independent variable(s) = physical, social exposome (column "Independent_Variables")
# select adjusting variable(s)  ("Adjusting_Variables")

# "desc_stat" sheet:
# In sheet "variable_type", you can find all variable names,
# copy-paste variable name into appropriate cell in "desc_stat" sheet 
# to run descriptive statistics.

# "sex_differences" sheet:
# In sheet "variable_type", you can find all variable names,
# copy-paste variable name into appropriate cell in "sex_differences" sheet
# to run sex differences analyses.

#######################################
# Impute data set
#######################################
# set maximum fraction of missing data (0.25 = 25%)
imp_max <- 0.25 # no variable used in RF should have > 25% of missing data
# set seed for imputation reproducibility (can be random number)
seed <- 2024
# generate correlation between imputed data
correlations_imputed <- "ON" # default is "ON", change to "OFF" to disable and speed up imputation step

source("Scripts/2_impute.R") # impute the data set
# imputed data set saved: Data/imputed_data.Rds



# code bellow can be re-run according to your need without re-running the imputation step 
#######################################
# Descriptive statistics
#######################################
# this line will calculate descriptive statistics (or do nothing if not assigned before)

analysis_run <- "test_II" # write names of your analysis

correlations <- "ON" # default is "OFF", to generate correlations separately per analysis chnage to "ON"

source("Scripts/3_descriptive.R") # generate descriptive statistics and correlation matrix

#######################################
# Run logistic regression
#######################################
linearity_plots <- "ON" # default is "ON", you can "OFF" if you want to speed up the process

source("Scripts/4_logit.R") # run logistic regression models
# analysis results are saved in: your local disc/Logit_Code/Results


# Results can be inspected in console
# If you want to change dependent/independent/adjusting variables, 
# do so in "variable_type.xlsx" and re-run lines 67

#######################################
# Run sex differences models (logistic and linear regression)
#######################################
source("Scripts/5_sex_differences.R") # run sex differences models
# analysis results are saved in: your local disc/Logit_Code/Results
