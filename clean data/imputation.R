








# Imputation  -------------------------------------------------------------
# Variables to impute 
#ED_HIGH_COM   WEA_SVNGSVL_MC Qpm25_1yr_com       no2_1yr_com        o3_1yr_com    GRLANYY_26_COM      temp_1yr_com  
#LGTNLTYY_01_COM ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ     PA2_DSCR2_MCQ  ICQ_SMOKE_COM 


# Test 9: Create 20 datasets  ---------------------------------------------

library(mice)

# Subset data
imp_data <- subset_data[, c(
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]

# Dry run to get defaults
init <- mice(imp_data, maxit = 0)
my_methods <- init$method

# Variables NOT to impute
no_impute <- c(
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)
my_methods[no_impute] <- ""

# Specify methods for variables to impute
impute_vars <- c(
  "SEX_ASK_COM" = "logreg",
  "AGE_NMBR_COM" = "pmm",
  "Race7" = "polyreg",
  "WGHTS_PROV_COM" = "polyreg",
  "ALC_FREQ_COM" = "polyreg",
  "ICQ_SMOKE_COM" = "polyreg",
  "PA2_DSCR2_MCQ" = "pmm",
  "no2_1yr_com" = "pmm",
  "pm25_1yr_com" = "pmm",
  "GRLANYY_26_COM" = "pmm",
  "o3_1yr_com" = "pmm",
  "temp_1yr_com" = "pmm",
  "LGTNLTYY_01_COM" = "pmm",
  "ENV_HMPRB_CON_MCQ" = "logreg",
  "ENV_HMPRB_NOI_MCQ" = "logreg"
)
my_methods[names(impute_vars)] <- impute_vars

# Run MICE with 20 imputed datasets
imp_data <- mice(
  imp_data,
  method = my_methods,
  m = 20,          # <- changed to 20
  seed = 123
)

# Long format with all imputations
imp_long <- complete(imp_data, action = "long", include = TRUE)

# List of individual completed datasets
completed_list <- lapply(1:20, function(i) complete(imp_data, i))

# Check remaining NAs in the first imputed dataset
colSums(sapply(completed_list[[1]], is.na))

      
      
      
  
































#didn't work 

# Test 1  (this works) --------------------------------------------------------


# Load package
library(mice)

# Select the variables you care about
imp_data <- subset_data[, c("AGE_NMBR_COM", "WEA_SVNGSVL_MCQ", "pm25_1yr_com")]

# Run a dry run first (to see defaults)
ini <- mice(imp_data, maxit = 0)

# Check the default methods
ini$method
# You'll likely see "" (no imputation) for AGE_NMBR_COM (numeric, no missing)
# and "pmm" (predictive mean matching) for WEA_SVNGSVL_MCQ if it's numeric

# Tell mice to impute WEA_SVNGSVL_MCQ using AGE_NMBR_COM
meth <- ini$method
meth["WEA_SVNGSVL_MCQ"] <- "polyreg"  # predictive mean matching
meth["pm25_1yr_com"] <- "pmm" 


# Set predictor matrix so that only AGE_NMBR_COM is used
pred <- ini$predictorMatrix
pred[,] <- 0                            # turn off all predictors
pred["WEA_SVNGSVL_MCQ", "AGE_NMBR_COM"] <- 1
pred["pm25_1yr_com", "AGE_NMBR_COM"]   <- 1


# Run the imputation (do 5 datasets for example)
imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123)

# Get a completed dataset (first one)
completed_data <- complete(imp, 1)

sum(is.na(imp_data$completed_data)) #there are 0 NAs 




# Test 2 - try adding a variable that you don't impute (Income)  ----------


# Load package
library(mice)

# Select the variables you care about, including income (will be a predictor only)
imp_data <- subset_data[, c("AGE_NMBR_COM", "WEA_SVNGSVL_MCQ", "pm25_1yr_com", "INC_TOT_COM")]

# Dry run to see defaults
ini <- mice(imp_data, maxit = 0)

# Set imputation methods
meth <- ini$method
meth["WEA_SVNGSVL_MCQ"] <- "polyreg"  # categorical variable
meth["pm25_1yr_com"] <- "pmm"          # continuous variable
meth["INC_TOT_COM"] <- ""               # "" means do NOT impute

# Set predictor matrix so that AGE_NMBR_COM and INC_TOT_COM are used as predictors
pred <- ini$predictorMatrix
pred[,] <- 0                            # turn off all predictors
pred["WEA_SVNGSVL_MCQ", c("AGE_NMBR_COM","INC_TOT_COM")] <- 1
pred["pm25_1yr_com",   c("AGE_NMBR_COM","INC_TOT_COM")] <- 1

# Run the imputation (5 datasets)
imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123)

# Get a completed dataset (first imputed version)
completed_data <- complete(imp, 1)

# Check for remaining NAs
sum(is.na(completed_data))




# Test 3 - try adding a variable that you don't impute (Income) - did not work  ----------

imp_data <- subset_data[, c(
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM", 
  "temp_1yr_com", "LGTNLTYY_01_COM", "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]


ini <- mice(imp_data, maxit = 0)
meth <- ini$method


# Do NOT impute main exposures or outcomes
meth[c("INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
       "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC")] <- ""

#impute environmental exposures and lifestyle variables
meth[c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM",
       "temp_1yr_com", "LGTNLTYY_01_COM", "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
       "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM")] <- c(
         "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "logreg", "logreg",
         "pmm", "polyreg", "polyreg"
       )


pred <- ini$predictorMatrix
pred[,] <- 0  # turn everything off

# For example, pm25_1yr_com is imputed using age, sex, province, race, income, education, savings, outcomes, other exposures
pred["pm25_1yr_com", c(
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "no2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM", 
  "temp_1yr_com", "LGTNLTYY_01_COM", "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)] <- 1

imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123)

# Get one completed dataset
completed_data <- complete(imp, 1)

# Check for remaining NAs
sum(is.na(completed_data))




# Test 4 -  ----------




# Load mice
library(mice)

# Step 1: Select variables
imp_data <- subset_data[, c(
  # Demographics / exposures
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  # Environmental exposures to impute
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  # Lifestyle / behavioral variables to impute
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  # Outcomes (not imputed)
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]


# Step 3: Dry run to get defaults
ini <- mice(imp_data, maxit = 0)
meth <- ini$method

# Step 4: Set methods
# "" = do NOT impute
# Do NOT impute exposures or outcomes
meth[c("INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
       "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC")] <- ""

# Impute environmental exposures and lifestyle variables
meth[c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
       "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
       "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
       "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM")] <- c(
         "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
         "logreg", "logreg", "pmm", "polyreg", "polyreg"
       )

# Step 5: Create predictor matrix
pred <- ini$predictorMatrix
pred[,] <- 0  # turn everything off

# Only use predictors with no missing values
predictors_no_NA <- colnames(imp_data)[colSums(is.na(imp_data)) == 0]

# Assign predictors for each variable to be imputed
vars_to_impute <- names(meth[meth != ""])
for (v in vars_to_impute) {
  pred[v, predictors_no_NA[predictors_no_NA != v]] <- 1
}


# Step 6: Run multiple imputation

imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123)


# Step 7: Get completed datasets

completed_data_list <- list()
for (i in 1:5) {
  completed_data_list[[i]] <- complete(imp, action = i)
}

# Optional: get long-format dataset
imp_long <- complete(imp, action = "long", include = TRUE)


# Step 8: Check for remaining NAs

colSums(sapply(completed_data_list[[1]], is.na))





# Test 5  -----------------------------------------------------------------



# Load mice
library(mice)

# -------------------------------
# Step 1: Select variables

imp_data <- subset_data[, c(
  # Demographics / exposures
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  # Environmental exposures to impute
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  # Lifestyle / behavioral variables to impute
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  # Outcomes (not imputed)
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]



# -------------------------------
# Step 3: Dry run to get defaults
# -------------------------------
ini <- mice(imp_data, maxit = 0)
meth <- ini$method

# -------------------------------
# Step 4: Set methods
# "" = do NOT impute
# -------------------------------
# Do NOT impute exposures or outcomes
meth[c("INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
       "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC")] <- ""

# Impute environmental exposures and lifestyle variables
meth[c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
       "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
       "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
       "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM")] <- c(
         "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
         "logreg", "logreg", "pmm", "polyreg", "polyreg"
       )

# -------------------------------
# Step 5: Create predictor matrix
# -------------------------------
pred <- ini$predictorMatrix
pred[,] <- 0  # turn everything off

# Only use predictors with no missing values
predictors_no_NA <- colnames(imp_data)[colSums(is.na(imp_data)) == 0]

# Assign predictors for each variable to be imputed
vars_to_impute <- names(meth[meth != ""])
for (v in vars_to_impute) {
  pred[v, predictors_no_NA[predictors_no_NA != v]] <- 1
}

# -------------------------------
# Step 6: Run multiple imputation
# -------------------------------
imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123)

# -------------------------------
# Step 7: Get completed datasets
# -------------------------------
completed_data_list <- list()
for (i in 1:5) {
  completed_data_list[[i]] <- complete(imp, action = i)
}

# Optional: get long-format dataset
imp_long <- complete(imp, action = "long", include = TRUE)

# -------------------------------
# Step 8: Check for remaining NAs
# -------------------------------
colSums(sapply(completed_data_list[[1]], is.na))


#check distributions of newly imputed data
summary(completed_data_list[[1]]$pm25_1yr_com)
summary(completed_data_list[[1]]$ALC_FREQ_COM)



# Test 6  - this worked. but it's only letting fully observed variables (no NAs) be predictors. ------------------------------------------------------------------
# did not work - still getting lots of NAs 

library(mice)

# -------------------------------
# Step 1: Select variables
# -------------------------------
imp_data <- subset_data[, c(
  # Demographics / exposures
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  # Environmental exposures to impute
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  # Lifestyle / behavioral variables to impute
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  # Outcomes (not imputed)
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]


# -------------------------------
# Step 3: Dry run to get defaults
# -------------------------------
ini <- mice(imp_data, maxit = 0)
meth <- ini$method

# -------------------------------
# Step 4: Set methods
# -------------------------------
# Do NOT impute exposures or outcomes
meth[c("INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
       "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC")] <- ""

# Impute environmental exposures and lifestyle variables
meth[c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
       "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
       "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
       "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM")] <- c(
         "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
         "logreg", "logreg", "pmm", "polyreg", "polyreg"
       )

# -------------------------------
# Step 5: Predictor matrix
# -------------------------------
pred <- ini$predictorMatrix
pred[,] <- 0  # turn everything off

# Identify fully observed predictors
predictors_no_NA <- colnames(imp_data)[colSums(is.na(imp_data)) == 0]

# Assign predictors only for variables being imputed
vars_to_impute <- names(meth[meth != ""])
for (v in vars_to_impute) {
  pred[v, predictors_no_NA[predictors_no_NA != v]] <- 1
}

# -------------------------------
# Step 6: Run multiple imputation
# -------------------------------
imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123, maxit = 20)

# -------------------------------
# Step 7: Get completed datasets
# -------------------------------
completed_data_list <- list()
for (i in 1:5) {
  completed_data_list[[i]] <- complete(imp, action = i)
}

# Optional: long-format dataset
imp_long <- complete(imp, action = "long", include = TRUE)

# -------------------------------
# Step 8: Check remaining NAs
# -------------------------------
colSums(sapply(completed_data_list[[1]], is.na))




# Test 7 - let anything (including with NAs) be predictors. ------------------------------------------------------------------
#did not work (lots of NAs) 

library(mice)

# -------------------------------
# Step 1: Select variables
# -------------------------------
imp_data <- subset_data[, c(
  # Demographics / exposures
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  # Environmental exposures to impute
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  # Lifestyle / behavioral variables to impute
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  # Outcomes (not imputed)
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]

# -------------------------------
# Step 2: Dry run to get defaults
# -------------------------------
ini <- mice(imp_data, maxit = 0)
meth <- ini$method

# -------------------------------
# Step 3: Set methods
# -------------------------------
# Do NOT impute exposures or outcomes
meth[c("INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
       "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC")] <- ""

# Impute environmental exposures and lifestyle variables
meth[c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
       "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
       "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
       "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM")] <- c(
         "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
         "logreg", "logreg", "pmm", "polyreg", "polyreg"
       )

# -------------------------------
# Step 4: Create predictor matrix
# -------------------------------
pred <- ini$predictorMatrix
pred[,] <- 0  # turn everything off

vars_to_impute <- names(meth[meth != ""])
all_vars <- colnames(imp_data)

# For each variable to be imputed, use all other variables as predictors
for (v in vars_to_impute) {
  pred[v, all_vars[all_vars != v]] <- 1
}

# -------------------------------
# Step 5: Run multiple imputation
# -------------------------------
imp <- mice(imp_data, method = meth, predictorMatrix = pred, m = 5, seed = 123, maxit = 20)

# -------------------------------
# Step 6: Get completed datasets
# -------------------------------
completed_data_list <- list()
for (i in 1:5) {
  completed_data_list[[i]] <- complete(imp, action = i)
}

# Optional: long-format dataset
imp_long <- complete(imp, action = "long", include = TRUE)

# -------------------------------
# Step 7: Check remaining NAs
# -------------------------------
colSums(sapply(completed_data_list[[1]], is.na))












# Test 8  ---------------------------------

#I used the MICE (Multivariate Imputation by Chained Equations) method to handle missing data, specifying different imputation models depending on the variable type: 
#pmm for continuous variables, logreg for binary variables, and polyreg for categorical variables with more than two levels.
#Exposures and outcomes (INC_TOT_COM, ED_HIGH_COM, WEA_SVNGSVL_MCQ, GrimAge_EAA, PhenoAge_EAA, DunedinPACE, DNAmIC) were not imputed. 
#All other variables in the dataset, such as demographics, environmental exposures, and lifestyle measures, were imputed. 
#By default, MICE used all other variables as predictors for each variable being imputed, so each imputed variable was modeled conditional on the rest of the dataset. 
#The process generated five completed datasets for downstream analyses.


# Load package
library(mice)

#Subset data to only the variables i want to work with
imp_data <- subset_data[, c(
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com",
  "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM",
  "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ",
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)]

#Run a dry run to extract defaults
init <- mice(imp_data, maxit = 0)
my_methods <- init$method

#Identify variables NOT to impute (exposures/outcomes)
no_impute <- c(
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
)
my_methods[no_impute] <- ""

#Specify imputation methods for variables you DO want to impute
impute_vars <- c(
  "SEX_ASK_COM" = "logreg",      # binary
  "AGE_NMBR_COM" = "pmm",        # continuous
  "Race7" = "polyreg",           # unordered categorical
  "WGHTS_PROV_COM" = "polyreg",  # unordered categorical
  "ALC_FREQ_COM" = "polyreg",    # unordered categorical
  "ICQ_SMOKE_COM" = "polyreg",   # unordered categorical
  "PA2_DSCR2_MCQ" = "pmm",       # continuous-ish
  "no2_1yr_com" = "pmm",         # continuous
  "pm25_1yr_com" = "pmm",        # continuous
  "GRLANYY_26_COM" = "pmm",      # continuous
  "o3_1yr_com" = "pmm",          # continuous
  "temp_1yr_com" = "pmm",        # continuous
  "LGTNLTYY_01_COM" = "pmm",     # continuous
  "ENV_HMPRB_CON_MCQ" = "logreg",# binary
  "ENV_HMPRB_NOI_MCQ" = "logreg" # binary
)

# Overwrite defaults with your specified methods
my_methods[names(impute_vars)] <- impute_vars

#Run mice with your method setup
imp_data <- mice(
  imp_data,
  method = my_methods,
  m = 5,
  seed = 123
)

#Extract completed datasets
imp_long <- complete(imp_data, action = "long", include = TRUE)# Long format with all imputations
completed_list <- lapply(1:5, function(i) complete(imp_data, i))# Individual completed datasets
colSums(sapply(completed_list[[1]], is.na)) #check remaining NAs 


      
saveRDS(imp_data, "imputed_data.rds")

# reload later
imp_data <- readRDS("imputed_data.rds")
      
      