
# Load selected variables for regression
#####

# save variable_type excel file to tthe results (to check)
vt <- list(variable_type = read_xlsx("variable_type.xlsx", sheet = 1),
           desc_stat = read_xlsx("variable_type.xlsx", sheet = 2),
           sex_differences = read_xlsx("variable_type.xlsx", sheet = 3))

write_xlsx(vt, paste0(c("Results/"), paste0(f_name), c("/Logistic/variable_type.xlsx")))

dvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,5)])
dep_var <- as.vector(na.omit(dvar$Variable_Name[dvar$Dependent_Variable == 1]))
ivar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,6)])
ind_var <- as.vector(na.omit(ivar$Variable_Name[ivar$Independent_Variables == 1]))
cvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,7)])
co_var <- as.vector(na.omit(cvar$Variable_Name[cvar$Covariates == 1]))
sIvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,8)])
sI_var <- as.vector(na.omit(sIvar$Variable_Name[sIvar$SES_mother == 1]))
sIIvar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,9)])
sII_var <- as.vector(na.omit(sIIvar$Variable_Name[sIIvar$SES_father_household == 1]))
# add other for univariate ONLY
othervar <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type")[c(1,10)])
other_var <- as.vector(na.omit(othervar$Variable_Name[othervar$other_variables == 1]))

#####

# Function to create pooled covariance-varaince matrix
#####

pool_vcov <- function(mid) {
  vcovs <- lapply(mid$analyses, vcov)
  coefs <- lapply(mid$analyses, coef)
  m <- length(coefs) 
  ##  rubins rule T = U + (1+1/m) * B
  ## U= mean covariance matrix
  ## B between imputation covariance 1/(m-1) Sum (Q - Qgem)(Q-Qgem)'
  U <- vcovs[[1]]
  for (i in 2:m) U <- U + vcovs[[i]]
  U <- U/m
  Q <- coefs[[1]] 
  for (i in 2:m) Q <- Q + coefs[[i]]
  Q <- Q/m
  B <- as.vector(coefs[[1]] - Q) %*% t(as.vector(coefs[[1]] - Q))
  for (i in 2:m) B <- B + as.vector(coefs[[i]] - Q) %*% t(as.vector(coefs[[i]] - Q))
  B <- B/(m - 1) 
  ##V <- U * tr(B %*% inv(as.matrix(U))) 
  T <- U + (1 + 1/m) * B
  ## this is just a check
  if (sum(abs(pool(mid)$pooled$t - diag(T))) > 0.0000001) print("Something went wrong, issue in results pooling.")
  return(T)
}
#####

# Function for logistic regression with only one variable
#####

univar <- function(dep_var, ind_var, co_var, sI_var, sII_var, imputed_data) {
  mulas <- c()
    # UNIVARIATE ONLY
  # Create an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    Formula = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    p_value = numeric(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    n_fitted = numeric())
  covar_mat <- data.frame(NA)
  
  # Pool all variables
  pool_var <- c(ind_var, co_var, sI_var, sII_var, other_var)
  
  # Loop through each variable (univariable logit model)
  for (i in 1:length(pool_var)) {
    # Fit logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, " ~ ", pool_var[i] ) ),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, " ~ ", pool_var[i]), times = length(term))
    mulas <- append(mulas, paste0(dep_var, " ~ ", pool_var[i]))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    covar_mat <- bind_rows(covar_mat, cov_matrix)
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
  }
  covar_mat <- covar_mat[2:ncol(covar_mat)]
  covar_mat <- covar_mat[2:nrow(covar_mat),]
  sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                       ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                              ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                     ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
  mpty <- c("->")
  results1 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, covar_mat)
  colnames(results1)[11] <- "Variance-Covariance Matrix"
  
  # UNIVARIATE + COVARIATES ONLY
  # Create an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    Formula = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    p_value = numeric(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    n_fitted = numeric())
  covar_mat <- data.frame(NA)
  
  # Pool all variables
  pool_var <- c(ind_var)
  
  # Loop through each variable (univariable logit model)
  for (i in 1:length(pool_var)) {
    # Fit logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var), collapse = "+"))),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var), collapse = "+")), times = length(term))  
    mulas <- append(mulas, paste0(dep_var, "~",  paste0(c(pool_var[i], co_var), collapse = "+")))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    covar_mat <- bind_rows(covar_mat, cov_matrix)
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
  }
  covar_mat <- covar_mat[2:ncol(covar_mat)]
  covar_mat <- covar_mat[2:nrow(covar_mat),]
  sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                       ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                              ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                     ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
  mpty <- c("->")
  results2 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, covar_mat)
  colnames(results2)[11] <- "Variance-Covariance Matrix"  
  
  # UNIVARIATE + COVARIATE + MOTHERS SES ONLY
  # Create an empty data frame to store results
  if (length(sI_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Pool all variables
    pool_var <- c(ind_var)
    
    # Loop through each variable (univariable logit model)
    for (i in 1:length(pool_var)) {
      # Fit logistic regression model
      fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var), collapse = "+"))),
                                    family = binomial(link = "logit")))
      # Get model summary
      summary_fit <- summary(pool(fit))
      # Get model results
      term <- as.character(summary_fit[,1])
      formula <- rep(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var), collapse = "+")), times = length(term))  
      mulas <- append(mulas, paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var), collapse = "+")))
      estimate <- summary_fit[,2]
      std_error <- summary_fit[,3]
      p_value <- summary_fit[,6]
      # Get OR + 95%CI
      odds_ratio <- exp(estimate)
      ci_low <- exp(estimate - 1.96 * std_error)
      ci_high <- exp(estimate + 1.96 * std_error)
      n_fitted <- nobs(fit$analyses[[1]])
      # Get the covariance matrix
      cov_matrix <- data.frame(pool_vcov(fit))
      covar_mat <- bind_rows(covar_mat, cov_matrix)
      # Append results to the data frame
      for (j in 1:length(term)) {
        results[nrow(results) + 1,] <- list(
          term[j],
          formula[j],
          estimate[j],
          std_error[j],
          p_value[j],
          odds_ratio[j],
          ci_low[j],
          ci_high[j],
          n_fitted[j])
      }
    }
    covar_mat <- covar_mat[2:ncol(covar_mat)]
    covar_mat <- covar_mat[2:nrow(covar_mat),]
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results3 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, covar_mat)
    colnames(results3)[11] <- "Variance-Covariance Matrix"  
  }
  
  # UNIVARIATE + COVARIATES + MOTHERS SES + FATHERS and HOUSEHOLD SES
  # Create an empty data frame to store results
  if (length(sII_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Pool all variables
    pool_var <- c(ind_var)
    
    # Loop through each variable (univariable logit model)
    for (i in 1:length(pool_var)) {
      # Fit logistic regression model
      fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var, sII_var), collapse = "+"))),
                                    family = binomial(link = "logit")))
      # Get model summary
      summary_fit <- summary(pool(fit))
      # Get model results
      term <- as.character(summary_fit[,1])
      formula <- rep(paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var, sII_var), collapse = "+")), times = length(term))  
      mulas <- append(mulas, paste0(dep_var, "~",  paste0(c(pool_var[i], co_var, sI_var, sII_var), collapse = "+")))
      estimate <- summary_fit[,2]
      std_error <- summary_fit[,3]
      p_value <- summary_fit[,6]
      # Get OR + 95%CI
      odds_ratio <- exp(estimate)
      ci_low <- exp(estimate - 1.96 * std_error)
      ci_high <- exp(estimate + 1.96 * std_error)
      n_fitted <- nobs(fit$analyses[[1]])
      # Get the covariance matrix
      cov_matrix <- data.frame(pool_vcov(fit))
      covar_mat <- bind_rows(covar_mat, cov_matrix)
      # Append results to the data frame
      for (j in 1:length(term)) {
        results[nrow(results) + 1,] <- list(
          term[j],
          formula[j],
          estimate[j],
          std_error[j],
          p_value[j],
          odds_ratio[j],
          ci_low[j],
          ci_high[j],
          n_fitted[j])
      }
    }
    covar_mat <- covar_mat[2:ncol(covar_mat)]
    covar_mat <- covar_mat[2:nrow(covar_mat),]
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results4 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, covar_mat)
    colnames(results4)[11] <- "Variance-Covariance Matrix"
  }
  
  # create an empty list with results names
  sheets <- list(univar = data.frame(),
                 univar_covar = data.frame(),
                 univar_covar_adjI = data.frame(),
                 univar_covar_adjI_adjII = data.frame(),
                 formulas = vector())
  # add the results to the list
  sheets[[1]] <- results1
  sheets[[2]] <- results2
  if (exists("results3")) {
    sheets[[3]] <- results3
  }
  if (exists("results4")) {
    sheets[[4]] <- results4
  }
  sheets[[5]] <- mulas
  # Return the results data frame
  return(sheets)
}
#####

# Function for logistic regression with all independent variables = crude,
#####

multivar <- function(dep_var, ind_var, co_var, sI_var, sII_var, imputed_data) {
  mulas <- c()
  # CRUDE MODEL ONLY
  # Create an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    Formula = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    p_value = numeric(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    n_fitted = numeric())
  covar_mat <- data.frame(NA)
  
  # Fit "crude" logistic regression model
  fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(ind_var), collapse = "+"))),
                                family = binomial(link = "logit")))
  # Get model summary
  summary_fit <- summary(pool(fit))
  # Get model results
  term <- as.character(summary_fit[,1])
  formula <- rep(paste0(dep_var, "~", paste0(c(ind_var), collapse = "+")), times = length(term))  
  mulas <- append(mulas, paste0(dep_var, "~", paste0(c(ind_var), collapse = "+")))
  estimate <- summary_fit[,2]
  std_error <- summary_fit[,3]
  p_value <- summary_fit[,6]
  # Get OR + 95%CI
  odds_ratio <- exp(estimate)
  ci_low <- exp(estimate - 1.96 * std_error)
  ci_high <- exp(estimate + 1.96 * std_error)
  n_fitted <- nobs(fit$analyses[[1]])
  # Get the covariance matrix
  cov_matrix <- data.frame(pool_vcov(fit))
  # Append results to the data frame
  for (j in 1:length(term)) {
    results[nrow(results) + 1,] <- list(
      term[j],
      formula[j],
      estimate[j],
      std_error[j],
      p_value[j],
      odds_ratio[j],
      ci_low[j],
      ci_high[j],
      n_fitted[j])
  }
  sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                       ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                              ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                     ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
  mpty <- c("->")
  results1 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
  colnames(results1)[11] <- "Variance-Covariance Matrix"
  
  # COVARIATES ONLY
  # Create an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    Formula = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    p_value = numeric(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    n_fitted = numeric())
  covar_mat <- data.frame(NA)
  
  # Fit "crude" logistic regression model
  fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(co_var), collapse = "+"))),
                                family = binomial(link = "logit")))
  # Get model summary
  summary_fit <- summary(pool(fit))
  # Get model results
  term <- as.character(summary_fit[,1])
  formula <- rep(paste0(dep_var, "~", paste0(c(co_var), collapse = "+")), times = length(term))  
  mulas <- append(mulas, paste0(dep_var, "~", paste0(c(co_var), collapse = "+")))
  estimate <- summary_fit[,2]
  std_error <- summary_fit[,3]
  p_value <- summary_fit[,6]
  # Get OR + 95%CI
  odds_ratio <- exp(estimate)
  ci_low <- exp(estimate - 1.96 * std_error)
  ci_high <- exp(estimate + 1.96 * std_error)
  n_fitted <- nobs(fit$analyses[[1]])
  # Get the covariance matrix
  cov_matrix <- data.frame(pool_vcov(fit))
  # Append results to the data frame
  for (j in 1:length(term)) {
    results[nrow(results) + 1,] <- list(
      term[j],
      formula[j],
      estimate[j],
      std_error[j],
      p_value[j],
      odds_ratio[j],
      ci_low[j],
      ci_high[j],
      n_fitted[j])
  }
  sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                       ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                              ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                     ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
  mpty <- c("->")
  results2 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
  colnames(results2)[11] <- "Variance-Covariance Matrix"
  
  # MOTHERS SES ONLY
  # Create an empty data frame to store results
  if (length(sI_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Fit "crude" logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(sI_var), collapse = "+"))),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, "~", paste0(c(sI_var), collapse = "+")), times = length(term))  
    mulas <- append(mulas, paste0(dep_var, "~", paste0(c(sI_var), collapse = "+")))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results3 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
    colnames(results3)[11] <- "Variance-Covariance Matrix"
  }
  
  # FATHERS and HOUSEHOLD SES ONLY
  # Create an empty data frame to store results
  if (length(sII_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Fit "crude" logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(sII_var), collapse = "+"))),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, "~", paste0(c(sII_var), collapse = "+")), times = length(term))  
    mulas <- append(mulas, paste0(dep_var, "~", paste0(c(sII_var), collapse = "+")))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results4 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
    colnames(results4)[11] <- "Variance-Covariance Matrix"
  }
  
  # CRUDE + COVARIATES
  # Create an empty data frame to store results
  results <- data.frame(
    Variable = character(),
    Formula = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    p_value = numeric(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    n_fitted = numeric())
  covar_mat <- data.frame(NA)
  
  # Fit "crude" logistic regression model
  fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(ind_var, co_var), collapse = "+"))),
                                family = binomial(link = "logit")))
  
  # Get model summary
  summary_fit <- summary(pool(fit))
  # Get model results
  term <- as.character(summary_fit[,1])
  formula <- rep(paste0(dep_var, "~", paste0(c(ind_var, co_var), collapse = "+")), times = length(term))  
  mulas <- append(mulas, paste0(dep_var, "~", paste0(c(ind_var, co_var), collapse = "+")))
  estimate <- summary_fit[,2]
  std_error <- summary_fit[,3]
  p_value <- summary_fit[,6]
  # Get OR + 95%CI
  odds_ratio <- exp(estimate)
  ci_low <- exp(estimate - 1.96 * std_error)
  ci_high <- exp(estimate + 1.96 * std_error)
  n_fitted <- nobs(fit$analyses[[1]])
  # Get the covariance matrix
  cov_matrix <- data.frame(pool_vcov(fit))
  # Append results to the data frame
  for (j in 1:length(term)) {
    results[nrow(results) + 1,] <- list(
      term[j],
      formula[j],
      estimate[j],
      std_error[j],
      p_value[j],
      odds_ratio[j],
      ci_low[j],
      ci_high[j],
      n_fitted[j])
  }
  sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                       ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                              ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                     ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
  mpty <- c("->")
  results5 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
  colnames(results5)[11] <- "Variance-Covariance Matrix"
  
  # CRUDE + COVARIATES + MOTHERS SES
  # Create an empty data frame to store results
  if (length(sI_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Fit "crude" logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var), collapse = "+"))),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var), collapse = "+")), times = length(term))  
    mulas <- append(mulas, paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var), collapse = "+")))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results6 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
    colnames(results6)[11] <- "Variance-Covariance Matrix"
  }
  
  # CRUDE + COVARIATES + MOTHERS SES + FATHERS and HOUSEHOLD SES
  # Create an empty data frame to store results
  if (length(sI_var) > 0 & length(sII_var) > 0) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    
    # Fit "crude" logistic regression model
    fit <- with(imputed_data, glm(as.formula(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var, sII_var), collapse = "+"))),
                                  family = binomial(link = "logit")))
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    formula <- rep(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var, sII_var), collapse = "+")), times = length(term))  
    mulas <- append(mulas, paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var, sII_var), collapse = "+")))
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    n_fitted <- nobs(fit$analyses[[1]])
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    # Append results to the data frame
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        formula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    mpty <- c("->")
    results7 <- cbind(results[,c(1:5)], sig, results[,6:9], mpty, cov_matrix)
    colnames(results7)[11] <- "Variance-Covariance Matrix"
  }
  # create an empty list with names of the separate objects
  sheets <- list(crude = data.frame(),
                 covariates = data.frame(),
                 adjI = data.frame(),
                 adjII = data.frame(),
                 crude_cov = data.frame(),
                 crude_cov_adjI = data.frame(),
                 crude_cov_adjI_adjII = data.frame(),
                 formulas = vector())
  # add the results to the list
  sheets[[1]] <- results1
  sheets[[2]] <- results2
  if (exists("results3")) {
    sheets[[3]] <- results3
  }
  if (exists("results4")) {
    sheets[[4]] <- results4
  }
  sheets[[5]] <- results5
  if (exists("results6")) {
    sheets[[6]] <- results6
  }
  if (exists("results7")) {
    sheets[[7]] <- results7
  }
  sheets[[8]] <- mulas
  
  # Return the results data frame
  return(sheets)
}
#####

# Run the logistic regression
#####

# Set global contrasts for unordered and ordered factors
# this will ensure that ordered factors will be treated as ordered factors in glm()
# otherwise ordered factors would be treated as unordered factors in glm()
options(contrasts = c("contr.treatment", "contr.poly"))

if ((!any(duplicated(c(dep_var, ind_var, co_var, sI_var, sII_var))))) {
  if (length(dep_var) == 1) {
    if (length(ind_var) == 0 & length(co_var) == 0 & length(sI_var) == 0 & length(sII_var) == 0) {
      print("Independent variable(s) OR covariate(s) not selected, please select at least one independent variable and one covariate.")
    } else if (length(ind_var) > 0 & length(co_var) > 0) {
      print("Running regression.")
      # Regression
      univariate <- univar(dep_var, ind_var, co_var, sI_var, sII_var, imputed_data) # data frame
      multivariate <- multivar(dep_var, ind_var, co_var, sI_var, sII_var, imputed_data) # data frame
      # Put results in a list
      sheets <- c(univariate[-length(univariate)], multivariate[-length(multivariate)])
      mulas <- c(univariate[["formulas"]], multivariate[["formulas"]])
      #  
      # Save results
      write_xlsx(sheets, paste0(c("Results/"), paste0(f_name), c("/Logistic/regression.xlsx")))

#####

##################################################x
# Multicollinearity
# Linearity
# Goodness of Fit
# Receiver operating characteristic curve (ROC) and Area under the curve (AOC)
##################################################x

# Multicollinearity

# Multicollinearity == VIF > 5, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6900425/
#####

# empty data frame
gvif_model <- data.frame(GVIF = numeric(),
                         df = numeric(),
                         GVIF_df = numeric())
# get all GVIF in one object
for (i in 1:5) {
  formulaI <- as.formula(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var, sII_var), collapse = "+")))
  fitI <- glm(family = binomial(link = "logit"),
              data = complete(imputed_data, i),
              formula = formulaI)
  a <- gvif(fitI)
  gvif_model <- rbind(gvif_model, a)
}
# Generalized function to calculate the mean of every n-th value starting from a given row
mean_every_nth <- function(start_row, column_data, n) {
  indices <- seq(start_row, length(column_data), by = n)
  mean(column_data[indices], na.rm = TRUE)
}
# Function to calculate the means for every column in the data frame
calculate_means <- function(df, n) {
  # Apply this function to each starting row (1 to n) for each column
  results <- lapply(seq_along(df), function(col_num) {
    sapply(1:n, function(start_row) {
      mean_every_nth(start_row, df[[col_num]], n)
    })
  })
  # Convert results to a data frame for better readability
  results_df <- as.data.frame(results)
  colnames(results_df) <- colnames(df)
  
  colnames(results_df) <- names(gvif_model)
  results_df$Multicollinearity <- ifelse(results_df[,3] >= 5, "TRUE", "FALSE")
  return(results_df)
}
# Calculate the means with every 11th value
n <- nrow(a)
MCL <- calculate_means(gvif_model, n)
MCL <- cbind(rownames(a), MCL)
colnames(MCL)[1] <- "Variable"
#####

# Linearity
#####

if (linearity_plots == "ON") {
  formula <- as.formula(paste0(dep_var, "~", paste0(c(ind_var, co_var, sI_var, sII_var), collapse = "+")))
  try(
    {
      for (i in 1:5) {
        data_impu <- complete(imputed_data, i)
        model <- glm(formula = formula,
                     family = binomial(link = "logit"),
                     data = data_impu)
        # Linearity (combined model)
        probabilities <- predict(model, type = "response")
        plotdf <- data_impu[,c(ind_var, co_var, sI_var, sII_var)] %>% select_if(is.numeric)
        predictors <- colnames(plotdf)
        plotdf <- plotdf %>%
          mutate(logit = log(probabilities/(1-probabilities))) %>%
          gather(key = "predictors", value = "predictor.value", -logit)
        
        # Plot the data
        plot <- plotdf %>%
          ggplot(aes(logit, predictor.value)) +
          geom_point(size = 0.5, alpha = 0.5) +
          geom_smooth(method = "auto") + 
          theme_bw() + 
          facet_wrap(~predictors, scales = "free_y")
        ggsave(paste0(c("Results/"), paste0(f_name), c("/Logistic/"), c("Linearity/lin_imp_"), i, c(".png")),
               plot = plot, 
               width = NA, 
               height = NA, 
               dpi = 300)
      }
    }, 
    silent = FALSE
  )
}
#####

# Goodness of Fit
#####

# Omnibus Goodness of Fit and Hosmer-Lemeshow test
GOF <- data.frame(Imputation = character(),
                  Test = character(),
                  Model = character(),
                  p_value = numeric(),
                  Fit = character())

LRT <- data.frame(Imputation = character(),
                  Test = character(),
                  Model = character(),
                  Chisqr = numeric(),
                  df = numeric(),
                  p_value = numeric())

akaike <- data.frame(Imputation = character(),
                     Test = character(),
                     Model = character(),
                     AIC = numeric())

bay <- data.frame(Imputation = character(),
                     Test = character(),
                     Model = character(),
                     BIC = numeric())

OB <- data.frame(Imputation = character(),
                 Test = character(),
                 Model = character(),
                 p_value = numeric(),
                 Fit = character())

options(contrasts=c("contr.treatment", "contr.treatment"))
mulas <- append(paste0(dep_var, "~ 1"), mulas)
tryCatch(
  {
    # Initialize your result storage (assuming GOF, LRT, akaike, OB are pre-defined)
    rownumbr <- 0
    for (k in mulas) {
      for (i in 1:5) {
        rownumbr <- rownumbr + 1
        data_impu <- complete(imputed_data, i)
        full_model <- glm(as.formula(k), family = binomial(link = "logit"), data = data_impu) 
        # Hosmer-Lemeshow test with error handling
        tryCatch(
          {
            GOF[rownumbr, 1] <- i
            GOF[rownumbr, 2] <- "Hosmer-Lemeshow"
            GOF[rownumbr, 3] <- k
            GOF[rownumbr, 4] <- hoslem.test(full_model$y, fitted(full_model), g = 5)$p.value
          },
          error = function(e) {
            # If there's an error, store NA for this row and continue
            GOF[rownumbr, 4] <- NA
            message("Error in Hosmer-Lemeshow test: ", e)
          }
        )
        # Likelihood ratio test with error handling
        tryCatch(
          {
            # Fit the null model
            null_model <- glm(as.formula(paste0(dep_var, "~ 1")), family = binomial(link = "logit"), data = data_impu)
            LRT[rownumbr, 1] <- i
            LRT[rownumbr, 2] <- "Likelihood Ratio Test"
            LRT[rownumbr, 3] <- k
            LRT[rownumbr, 4:6] <- lrtest(null_model, full_model)$stats
          },
          error = function(e) {
            # If there's an error in likelihood ratio test, store NA and continue
            LRT[rownumbr, 4:6] <- NA
            message("Error in Likelihood Ratio Test: ", e)
          }
        )
        # Akaike criterion with error handling
        tryCatch(
          {
            akaike[rownumbr, 1] <- i
            akaike[rownumbr, 2] <- "Akaike Criterion"
            akaike[rownumbr, 3] <- k
            akaike[rownumbr, 4] <- AIC(full_model)
          },
          error = function(e) {
            # If there's an error in AIC calculation, store NA and continue
            akaike[rownumbr, 4] <- NA
            message("Error in AIC: ", e)
          }
        )
        # Bayesian criterion with error handling
        tryCatch(
          {
            bay[rownumbr, 1] <- i
            bay[rownumbr, 2] <- "Bayesian Criterion"
            bay[rownumbr, 3] <- k
            bay[rownumbr, 4] <- BIC(full_model)
          },
          error = function(e) {
            # If there's an error in AIC calculation, store NA and continue
            bay[rownumbr, 4] <- NA
            message("Error in BIC: ", e)
          }
        )
        
        # Omnibus Goodness of Fit (from 'rms' package) with error handling
        tryCatch(
          {
            OBtest <- lrm(as.formula(k), data = data_impu, y = T, x = T, linear.predictors = TRUE)
            OB[rownumbr, 1] <- i
            OB[rownumbr, 2] <- "Omnibus Goodness of Fit"
            OB[rownumbr, 3] <- k
            OB[rownumbr, 4] <- residuals(OBtest, type = "gof")[5]  # Assuming you want the 5th residual value
          },
          error = function(e) {
            # If there's an error in the Omnibus Goodness of Fit calculation, store NA and continue
            OB[rownumbr, 4] <- NA
            message("Error in Omnibus Goodness of Fit: ", e)
          }
        )
      }
    }
    GOF[, 5] <- ifelse(GOF[, 4] > 0.05, "Good", "Poor")
    OB[, 5] <- ifelse(OB[, 4] > 0.05, "Good", "Poor")
  }, 
  silent = FALSE
)
#####

# Receiver operating characteristic curve (ROC) and Area under the curve (AOC)
#####

roc_auc_key <- data.frame(Imputation = character(),
                          Test = character(),
                          Model = character(),
                          Plot_number = numeric())
run <- 0
for (k in mulas) {
  for (i in 1:5) {
    tryCatch({
      # Complete imputed dataset
      data_inpu <- complete(imputed_data, i)
      
      # Fit the logistic regression model
      fit <- glm(as.formula(k), family = binomial(link = "logit"), data = data_inpu)
      run <- run + 1
      
      # Calculate ROC and AUC
      prob <- predict(fit, type = c("response"))
      data_inpu$prob <- prob
      roc_plot <- roc(as.formula(paste0(c(dep_var), "~", "prob")), data = data_inpu)
      
      # Generate and save the ROC plot
      png(filename = paste0("Results/", f_name, "/Logistic/ROC_AUC/ROC_AUC_plot_", run, ".png"))
      plot(roc_plot) # Plot ROC curve
      mtext(round(auc(roc_plot), 3), # Add AUC value to the plot
            side = 3, line = -20, at = 0.3, cex = 2)
      mtext("AUC = ", # Add AUC text to the plot
            side = 3, line = -20, at = 0.5, cex = 2)
      dev.off()
      
      # Store information for the plot
      roc_auc_key[run, 1] <- i
      roc_auc_key[run, 2] <- "ROC AUC plot"
      roc_auc_key[run, 3] <- k
      roc_auc_key[run, 4] <- run
    },
    error = function(e) {
      # Log the error message and continue
      message(paste("Error in iteration k =", k, "i =", i, ":", conditionMessage(e)))
    },
    warning = function(w) {
      # Log the warning message and continue
      message(paste("Warning in iteration k =", k, "i =", i, ":", conditionMessage(w)))
    })
  }
}

#####

# Pseudo R2
#####

Pseudo_R2 <- data.frame(Imputation_1 = numeric(),
                        Imputation_2 = numeric(),
                        Imputation_3 = numeric(),
                        Imputation_4 = numeric(),
                        Imputation_5 = numeric(),
                        Model = character())

# Loop through 'mulas' with error and warning handling
for (k in mulas) {
  tryCatch({
    fit <- with(imputed_data, glm(as.formula(k), family = binomial(link = "logit")))
    # extract model fit from a mira object
    fit_list <- getfit(fit) 
    # Estimate pseudo R2
    pseudo_r2_values <- data.frame(sapply(fit_list, function(fit) pR2(fit)))
    # Add model information and parameter row names
    pseudo_r2_values$Model <- k
    # Append to Pseudo_R2
    Pseudo_R2 <- rbind(Pseudo_R2, pseudo_r2_values)
  }, 
  error = function(e) {
    # Log the error message
    message("Error encountered with formula: ", k)
    message("Error details: ", e$message)
  }, 
  warning = function(w) {
    # Log the warning message
    message("Warning encountered with formula: ", k)
    message("Warning details: ", w$message)
  })
}

Pseudo_R2$Parameter <- rownames(Pseudo_R2)
Pseudo_R2 <- Pseudo_R2[,c(7, 1:6)]
colnames(Pseudo_R2)[2:6] <- c("Imputation 1", "Imputation 2", "Imputation 3", "Imputation 4", "Imputation 5")

#####

# Save results
#####

# Create an empty list
sheetsII <- list(MCL = data.frame(),
                 GOF = data.frame(),
                 OB = data.frame(),
                 LRT = data.frame(),
                 ak_crit = data.frame(),
                 bay_crit = data.frame(),
                 roc_auc_key = data.frame(),
                 Pseudo_R2 = data.frame())

# Add results to the list
if (exists("MCL")) {
  sheetsII[[1]] <- MCL
}
if (exists("GOF")) {
  sheetsII[[2]] <- GOF
}
if (exists("OB")) {
  sheetsII[[3]] <- OB
}
if (exists("LRT")) {
  sheetsII[[4]] <- LRT
}
if (exists("akaike")) {
  sheetsII[[5]] <- akaike
}
if (exists("bay")) {
  sheetsII[[6]] <- bay
}
if (exists("roc_auc_key")) {
  sheetsII[[7]] <- roc_auc_key
}
if (exists("Pseudo_R2")) {
  sheetsII[[8]] <- Pseudo_R2
}

# save the results
write_xlsx(sheetsII, paste0(c("Results/"), paste0(f_name), c("/Logistic/fit.xlsx")))
#####

    } else {
      print("Please selects at least one covariate and independent variable.")
    }
  } else if (length(dep_var) == 0) {
    print("Dependent variable (outcome) missing, please select one.")
  } else if (length(dep_var) > 1) {
    print("Multiple dependent variables (outcomes) selected, please select only one.")
  }
} else (print("Duplicates detected, check variable selection."))



