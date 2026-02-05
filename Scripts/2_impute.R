
# Load variable type file
variable_type <- data.frame(read_xlsx("variable_type.xlsx", sheet = "variable_type"))

# Change variable type in the DF according to variable type file
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

# Impute using mice, only for variables with <= 25% missing
mis_perc <- colMeans(is.na(DF)) * 100 # % of missing data

vars_25 <- as.vector(names(mis_perc[mis_perc <= (imp_max*100)])) # columns to be imputed

# Imputation preparation
# https://www.r-bloggers.com/2016/06/handling-missing-data-with-mice-package-a-simple-approach/
init <- mice(DF, maxit = 0) 
meth <- init$method
predM <- init$predictorMatrix
# Set method
meth[1:length(meth)] <- "cart" # set cart as prediction method
meth[setdiff(colnames(DF), vars_25)] = "" # set which variables will not be imputed = all variables with > 25% NAs
# Edit imputation matrix (columns and rows)
predM[setdiff(colnames(DF), vars_25),] = 0 # set which variables will not be used for imputation
predM[,setdiff(colnames(DF), vars_25)] = 0 # set which variables will not be used for imputation

imputed_data <- mice(DF, 
                     m = 5, 
                     maxit = 5, 
                     method = meth,
                     predictorMatrix = predM,
                     seed = seed,
                     print = FALSE)

saveRDS(imputed_data, "Data/imputed_data.Rds")

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

if (correlations_imputed == "ON") {
  
  # Correlation matrix (continuous variables)
  correlation_matrix <- function(imputed_data) {
    # Some functions
    fisher_trans <- function(x) {
      1/2 * log((1 + x) / (1 - x))
    }
    fisher_backtrans <- function(x) {
      (exp(2 * x) - 1) / (exp(2 * x) + 1)
    }
    c_var_names <- colnames(complete(imputed_data, 1))[colSums(is.na(complete(imputed_data, 1))) == 0]
    # Names of all numeric variables
    n_numer <- colnames((imputed_data %>%
                           complete("all") %>%
                           map(select, one_of(c_var_names)) %>%
                           map(select_if, is.numeric))[[1]])
    # Correlation matrix
    if (length(n_numer) > 1) {
      core_matrx <- imputed_data %>%
        complete("all") %>%
        map(select, one_of(c_var_names)) %>%
        map(select_if, is.numeric) %>% 
        map(stats::cor, method = c("spearman")) %>%
        map(fisher_trans)
      c_mat <- Reduce("+", core_matrx) / length(core_matrx)
      c_mat <- replace_upper_triangle(round(fisher_backtrans(c_mat), digits = 2), by = "", diagonal = FALSE)
      return(c_mat)
    } 
  }
  
  crl_matrx <- correlation_matrix(imputed_data)
  
  # Cramers V
  cram_list <- list()
  for (i in 1:5) {
    # Use tryCatch to handle any potential errors
    result <- tryCatch({
      # Complete the imputed data
      categorical_columns <- complete(imputed_data, i) %>% 
        select_if(is.factor) %>% 
        select_if(~ n_distinct(.) > 1) # Keep only meaningful variables
      
      # Generate combinations
      df_comb <- data.frame(t(combn(sort(names(categorical_columns)), 2)), stringsAsFactors = FALSE)
      
      # Apply the function to each variable combination
      cat_corr <- map2_df(df_comb$X1, df_comb$X2, cramer)
      
      # Assign to the list
      cram_name <- paste0("cram_", i)
      cram_list[[cram_name]] <- cat_corr
      
      # Return success
      TRUE
    }, error = function(e) {
      # If there's an error, print it and return FALSE
      message("Error in iteration ", i, ": ", e$message)
      return(FALSE)
    })
    
    # If the result was not successful, skip to the next iteration
    if (!result) next
  }
  
  # Assuming all computations went smoothly, proceed to create the data frames
  ch <- data.frame(cram_list[[1]][[3]],
                   cram_list[[2]][[3]],
                   cram_list[[3]][[3]],
                   cram_list[[4]][[3]],
                   cram_list[[5]][[3]])
  ch$average <- rowMeans(ch[1:4])
  
  v <- data.frame(cram_list[[1]][[4]],
                  cram_list[[2]][[4]],
                  cram_list[[3]][[4]],
                  cram_list[[4]][[4]],
                  cram_list[[5]][[4]])
  v$average <- rowMeans(v[1:4])
  
  cram_v <- data.frame(cram_list[[1]][[1]],
                       cram_list[[1]][[2]],
                       ch$average,
                       v$average)
  colnames(cram_v) <- names(cram_list[[1]])
 
  # save results
  imp_cor_sh <- list(spearman = data.frame(),
                     cramerV = data.frame())
  
  if (exists("crl_matrx")) {
    imp_cor_sh$spearman <- crl_matrx
  }
  if (exists("cram_v")) {
    imp_cor_sh$cramerV <- cram_v
  }
  
  write_xlsx(imp_cor_sh, "Results/correlation_imputed_data.xlsx")
}
