
# Cramers V function
# https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
cramer <- function(x, y) {
  tbl <- categorical_columns %>% select(x, y) %>% table()
  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    message("Skipping pair: ", x, ", ", y)
    return(data.frame(x = x, y = y, chisq_pval = NA, cramV = NA))
  }
  chisq_pval <- round(chisq.test(tbl)$p.value, 4)
  cramV <- round(cramersV(tbl), 4) 
  data.frame(x = x, y = y, chisq_pval = chisq_pval, cramV = cramV)
}

# Get variables
dvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[ c(1,5)])
dep_var <- as.vector(na.omit(dvar$Variable_Name[dvar$Dependent_Variable == 1]))
ivar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[ c(1,6)])
ind_var <- as.vector(na.omit(ivar$Variable_Name[ivar$Independent_Variables == 1]))
cvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[ c(1,7)])
co_var <- as.vector(na.omit(cvar$Variable_Name[cvar$Covariates == 1]))
sIvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[ c(1,8)])
sI_var <- as.vector(na.omit(sIvar$Variable_Name[sIvar$SES_mother == 1]))
sIIvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[ c(1,9)])
sII_var <- as.vector(na.omit(sIIvar$Variable_Name[sIIvar$SES_father_household == 1]))

if (length(dep_var) == 1) {
  # Combine project name with timestamp for folder name
  #f_name <- paste0(analysis_run, "_",  c(format(Sys.time(), "%d_%m_%Y")))
  f_name <- paste0(analysis_run, "_",  c( strftime(Sys.time(), "%d_%m_%Y") ))
  
  # get pathway
  drktry <- paste0(c(getwd()), c("/Results"))
  # combine pathway and filder name
  folder_outcome <- file.path(drktry, f_name)
  # Check if the folder already exists
  if (dir.exists(folder_outcome)) {
    # Remove the existing folder and its contents
    unlink(folder_outcome, recursive = TRUE)
  }
  # Create
  dir.create(folder_outcome) # separate folder for outcome
  dir.create(paste0(folder_outcome, c("/Logistic"))) # additional folder for logistic regression
  dir.create(paste0(folder_outcome, c("/Logistic/Linearity"))) # additional folder for linearity assumptions
  dir.create(paste0(folder_outcome, c("/Logistic/ROC_AUC"))) # additional folder for ROC plots
  dir.create(paste0(folder_outcome, c("/Sex_Differences"))) # additional folder for linearity assumptions
  dir.create(paste0(folder_outcome, c("/Sex_Differences/Linearity"))) # additional folder for linearity assumptions
  dir.create(paste0(folder_outcome, c("/Sex_Differences/LM_assump"))) # additional folder for linearity assumptions
  dir.create(paste0(folder_outcome, c("/Sex_Differences/ROC_AUC"))) # additional folder for linearity assumptions
  rm(folder_outcome, drktry)
  
  
  # Descriptive statistics
  desc_stat <- data.frame(read_xlsx("variable_type.xlsx", sheet = "desc_stat"))
  
  if (sum(is.na(desc_stat[2])) != nrow(desc_stat)) {
    variable_type <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type"))
    #DF <- read_csv("Data/master_data.csv")
    
    AS_N <- as.vector(variable_type$Variable_Name[variable_type$Type == "C"])
    AS_U <- as.vector(variable_type$Variable_Name[variable_type$Type == "U"])
    AS_O <- as.vector(variable_type$Variable_Name[variable_type$Type == "O"])
    
    # variables to numeric type
    for (var in AS_N) {
      DF[[var]] <- as.numeric(as.character(DF[[var]]))   
    }
    # variables to factor type - unordered
    for (var in AS_U) {
      DF[[var]] <- as.factor(DF[[var]])
    }
    # variables to factor type - ordered
    for (var in AS_O) {
      DF[[var]] <- as.ordered(DF[[var]])
    }
    
    stat_frame <- data.frame(matrix(nrow = nrow(DF), ncol = nrow(desc_stat)))
    
    for (i in 1:nrow(desc_stat)) {
      if (!is.na(desc_stat[i, 2])) {
        stat_frame[i] <- DF[paste0(desc_stat[i, 2])]
      } else {
        stat_frame[i] <- NA
      }
    }
    
    # create names for the descriptive statistics
    Parameter <- c("Study size (n)", 
                   "Outcome positive (%)",
                   "  of which positive boys (%)",
                   "  of which positive girls (%)",
                   "Child diag. age median",
                   "Child diag. age 5th percentile",
                   "Child diag. age 95th percentile",
                   "Child diag. age NA (%)",
                   "Mothers age at birth median",
                   "Mothers age at birth 5th percentile",
                   "Mothers age at birth 95th percentile",
                   "Mothers age at birth NA (%)",
                   "Fathers age at birth median",
                   "Fathers age at birth 5th percentile",
                   "Fathers age at birth 95th percentile",
                   "Fathers age at birth NA (%)",
                   "Mothers BMI median",
                   "Mothers BMI 5th percentile",
                   "Mothers BMI 95th percentile",
                   "Mothers BMI NA (%)",
                   "Smoking (%)",
                   "Smoking NA (%)",
                   "Income Q1 (%)",
                   "Income Q2 (%)",
                   "Income Q3 (%)",
                   "Income Q4 (%)",
                   "Income NA (%)",
                   "Mothers education Low (%)",
                   "Mothers education Mid (%)",
                   "Mothers education High (%)",
                   "Mothers education NA (%)",
                   "Mothers COB (%)",
                   "Mothers COB NA (%)",
                   "Fathers education Low (%)",
                   "Fathers education Mid (%)",
                   "Fathers education High (%)",
                   "Fathers education NA (%)",
                   "Fathers COB (%)",
                   "Fathers COB NA (%)",
                   "Positive boys (n)",
                   "Negative boys (n)",
                   "Positive girls (n)",
                   "Negative girls (n)",
                   "BMI underweight (<18.5, n)",
                   "BMI normal (>=18.5<25, n)",
                   "BMI overweight (>=25, n)")
    
    D_stat <- data.frame(Parameter = Parameter,
                         Value = NA)
    # Size of study
    D_stat[1,2] <- nrow(stat_frame)
    
    # % cases
    if (!is.na(desc_stat[1,2])) {
      D_stat[2,2] <- (sum(DF[paste0(desc_stat[1,2])] == 1) / nrow(DF)) * 100
    } else {
      D_stat[2,2] <- "Not provided or not available for this study"
    }
    
    # % cases boys
    if (!is.na(desc_stat[2,2]) & !is.na(desc_stat[1,2])) {
      D_stat[3,2] <- (sum(stat_frame[1] == 1 & stat_frame[2] == 1) / sum(stat_frame[1] == 1)) * 100
    } else {
      D_stat[3,2] <- "Not provided or not available for this study"
    }
    
    # % cases girls
    if (!is.na(desc_stat[2,2]) & !is.na(desc_stat[1,2])) {
      D_stat[4,2] <- (sum(stat_frame[1] == 1 & stat_frame[2] == 2) / sum(stat_frame[1] == 1)) * 100
    } else {
      D_stat[4,2] <- "Not provided or not available for this study"
    }
    
    stat_frame[,3] <- as.numeric(stat_frame[,3])  
    
    # Child age at diagnosis
    if (!is.na(desc_stat[3,2])) {
      D_stat[5,2] <- median(stat_frame[,3], na.rm = T)
      D_stat[6,2] <- quantile(stat_frame[,3], na.rm = T, 0.05)
      D_stat[7,2] <- quantile(stat_frame[,3], na.rm = T, 0.95)
      # D_stat[8,2] <- (sum(is.na(stat_frame[,3])) / nrow(stat_frame)) * 100
      D_stat[8,2] <- (sum(is.na(stat_frame[,3][stat_frame[1] == 1]  )) / sum(stat_frame[1] == 1)) * 100
    } else {
      D_stat[5,2] <- "Not provided or not available for this study"
      D_stat[6,2] <- "Not provided or not available for this study"
      D_stat[7,2] <- "Not provided or not available for this study"
      D_stat[8,2] <- "Not provided or not available for this study"
    }
    
    # Mothers age at birth
    if (!is.na(desc_stat[4,2])) {
      D_stat[9,2] <- median(stat_frame[,4], na.rm = T)
      D_stat[10,2] <- quantile(stat_frame[,4], na.rm = T, 0.05)
      D_stat[11,2] <- quantile(stat_frame[,4], na.rm = T, 0.95)
      D_stat[12,2] <- (sum(is.na(stat_frame[,4])) / nrow(stat_frame)) * 100
    } else {
      D_stat[9,2] <- "Not provided or not available for this study"
      D_stat[10,2] <- "Not provided or not available for this study"
      D_stat[11,2] <- "Not provided or not available for this study"
      D_stat[12,2] <- "Not provided or not available for this study"
    }
    
    # Fathers age at birth
    if (!is.na(desc_stat[5,2])) {
      D_stat[13,2] <- median(stat_frame[,5], na.rm = T)
      D_stat[14,2] <- quantile(stat_frame[,5], na.rm = T, 0.05)
      D_stat[15,2] <- quantile(stat_frame[,5], na.rm = T, 0.95)
      D_stat[16,2] <- (sum(is.na(stat_frame[,5])) / nrow(stat_frame)) * 100
    } else {
      D_stat[13,2] <- "Not provided or not available for this study"
      D_stat[14,2] <- "Not provided or not available for this study"
      D_stat[15,2] <- "Not provided or not available for this study"
      D_stat[16,2] <- "Not provided or not available for this study"
    }
    
    # Mothers BMI
    if (!is.na(desc_stat[6,2])) {
      D_stat[17,2] <- median(stat_frame[,6], na.rm = T)
      D_stat[18,2] <- quantile(stat_frame[,6], na.rm = T, 0.05)
      D_stat[19,2] <- quantile(stat_frame[,6], na.rm = T, 0.95)
      D_stat[20,2] <- (sum(is.na(stat_frame[,6])) / nrow(stat_frame)) * 100
    } else {
      D_stat[17,2] <- "Not provided or not available for this study"
      D_stat[18,2] <- "Not provided or not available for this study"
      D_stat[19,2] <- "Not provided or not available for this study"
      D_stat[20,2] <- "Not provided or not available for this study"
    }
    
    # Mothers smoking during pregnancy
    if (!is.na(desc_stat[7,2])) {
      D_stat[21,2] <- (sum(stat_frame[,7] == 1, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[22,2] <- (sum(is.na(stat_frame[,7])) / nrow(stat_frame)) * 100
    } else {
      D_stat[21,2] <- "Not provided or not available for this study"
      D_stat[22,2] <- "Not provided or not available for this study"
    }
    
    # Household income
    if (!is.na(desc_stat[8,2])) {
      D_stat[23,2] <- (sum(stat_frame[,8] == 4, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[24,2] <- (sum(stat_frame[,8] == 3, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[25,2] <- (sum(stat_frame[,8] == 2, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[26,2] <- (sum(stat_frame[,8] == 1, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[27,2] <- (sum(is.na(stat_frame[,8])) / nrow(stat_frame)) * 100
    } else {
      D_stat[23,2] <- "Not provided or not available for this study"
      D_stat[24,2] <- "Not provided or not available for this study"
      D_stat[25,2] <- "Not provided or not available for this study"
      D_stat[26,2] <- "Not provided or not available for this study"
      D_stat[27,2] <- "Not provided or not available for this study"
    }
    
    # Mothers education
    if (!is.na(desc_stat[9,2])) {
      D_stat[28,2] <- (sum(stat_frame[,9] == 3, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[29,2] <- (sum(stat_frame[,9] == 2, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[30,2] <- (sum(stat_frame[,9] == 1, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[31,2] <- (sum(is.na(stat_frame[,9])) / nrow(stat_frame)) * 100
    } else {
      D_stat[28,2] <- "Not provided or not available for this study"
      D_stat[29,2] <- "Not provided or not available for this study"
      D_stat[30,2] <- "Not provided or not available for this study"
      D_stat[31,2] <- "Not provided or not available for this study"
    }
    
    # Mothers country of birth
    if (!is.na(desc_stat[10,2])) {
      D_stat[32,2] <- (sum(stat_frame[,10] == 0, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[33,2] <- (sum(is.na(stat_frame[,10])) / nrow(stat_frame)) * 100
    } else {
      D_stat[32,2] <- "Not provided or not available for this study"
      D_stat[33,2] <- "Not provided or not available for this study"
    }
    
    # Fathers education
    if (!is.na(desc_stat[11,2])) {
      D_stat[34,2] <- (sum(stat_frame[,11] == 3, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[35,2] <- (sum(stat_frame[,11] == 2, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[36,2] <- (sum(stat_frame[,11] == 1, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[37,2] <- (sum(is.na(stat_frame[,11])) / nrow(stat_frame)) * 100
    } else {
      D_stat[34,2] <- "Not provided or not available for this study"
      D_stat[35,2] <- "Not provided or not available for this study"
      D_stat[36,2] <- "Not provided or not available for this study"
      D_stat[37,2] <- "Not provided or not available for this study"
    }
    
    # Fathers country of birth
    if (!is.na(desc_stat[12,2])) {
      D_stat[38,2] <- (sum(stat_frame[,12] == 0, na.rm = T) / nrow(stat_frame)) * 100
      D_stat[39,2] <- (sum(is.na(stat_frame[,12])) / nrow(stat_frame)) * 100
    } else {
      D_stat[38,2] <- "Not provided or not available for this study"
      D_stat[39,2] <- "Not provided or not available for this study"
    }
    
    # N of negative and positive boys nad girls
    if (!is.na(desc_stat[2,2]) & !is.na(desc_stat[1,2])) {
      D_stat[40,2] <- sum(stat_frame[,2] == 1 & stat_frame[,1] == 1)
      D_stat[41,2] <- sum(stat_frame[,2] == 1 & stat_frame[,1] == 0)
      D_stat[42,2] <- sum(stat_frame[,2] == 2 & stat_frame[,1] == 1)
      D_stat[43,2] <- sum(stat_frame[,2] == 2 & stat_frame[,1] == 0)
    } else {
      D_stat[40,2] <- "Not provided or not available for this study"
      D_stat[41,2] <- "Not provided or not available for this study"
      D_stat[42,2] <- "Not provided or not available for this study"
      D_stat[43,2] <- "Not provided or not available for this study"
    }
    
    # BMI categories
    if (!is.na(desc_stat[6,2])) {
      D_stat[44,2] <- nrow(subset(stat_frame, X6 < 18.5))
      D_stat[45,2] <- nrow(subset(stat_frame, X6 >= 18.5 & X6 < 25))
      D_stat[46,2] <- nrow(subset(stat_frame, X6 >= 25))
    } else {
      D_stat[44,2] <- "Not provided or not available for this study"
      D_stat[45,2] <- "Not provided or not available for this study"
      D_stat[46,2] <- "Not provided or not available for this study"
    }
    # birth year
    
    if (!is.na(desc_stat[13,2])) {
      c <- data.frame(table(stat_frame[13], useNA = "ifany"))
      colnames(c) <- c("Parameter", "Value")
      D_stat <- rbind(D_stat, c)
    } else {
      D_stat[47,1] <- "Birth year"
      D_stat[47,2] <- "Not provided or not available for this study"
    }
 
    if (correlations == "ON") {
      # CORRELATION MATRIX
      num_columns <- DF %>%
        select(all_of(c(co_var, ind_var, dep_var, sI_var, sII_var))) %>%
        select_if(is.numeric) %>% 
        select_if(~ any(!is.na(.)))
      if (ncol(num_columns) > 1) {
        c_mat <- cor_test(num_columns, conf.level = 0.95, method = "spearman")
      } else {
        print("Not enough continuous variables present in the data set, correlation matrix not created.")
      }
      # Cramers V
      # https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
      # 0 = no association; 1 = 100% association between variables
      tryCatch({
        # Select the columns from DF based on the provided vector
        categorical_columns <- DF %>% 
          select(all_of(c(co_var, ind_var, dep_var, sI_var, sII_var))) %>% 
          select_if(is.factor) %>% 
          select_if(~ n_distinct(.) > 1) # Keep only meaningful variables
        if (ncol(categorical_columns) > 1) {
          # Generate combinations of column pairs
          df_comb <- data.frame(t(combn(sort(names(categorical_columns)), 2)), stringsAsFactors = FALSE)
          # Apply the cramer function to each variable combination
          cat_corr <- map2_df(df_comb$X1, df_comb$X2, cramer)
        } else {
          print("Not enough categorical variables present in the data set, Cramers association not generated.")
        }
      }, silent = FALSE)
    }
    # put results in a list
    stat_sheets <- list(descriptives = data.frame(),
                        spearman = data.frame(),
                        cramerV = data.frame())
    if (exists("D_stat")) {
      stat_sheets$descriptives <- D_stat
    }
    if (exists("c_mat")) {
      stat_sheets$spearman <- c_mat
    }
    if (exists("cat_corr")) {
      stat_sheets$cramerV <- cat_corr
    }
    # exposrt results
    write_xlsx(stat_sheets, paste0(c("Results/"), c(f_name), c("/Logistic/descriptives.xlsx")))
    
  } else {
    print("Descriptive statistics not assigned, please assign descriptive statistic variables.")
  }
} else if (length(dep_var) > 1) {
  print("More than one dependent variables selected, please select only one.")
} else if (length(dep_var) < 1) {
  print("No dependent variable selected, please select one")
}
