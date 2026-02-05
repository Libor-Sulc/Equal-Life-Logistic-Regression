
##############################################
# Karls Sex Differences paper
# 1. Descriptive statistics: LINE ~ 15
# 2. Logistic regression (ADHD diagnosis): LINE ~ 452
# 3. Logistic regression Multicollinearity, linearity, regression fit, ROC/AUC, pseuso r2: LINE ~ 746
# 4. Linear regression (age at diagnosis): LINE ~ 1131
# 5. Linear regression Multicollinearity, linearity, regression fit: LINE ~ 1415
##############################################

##############################################
# 1. Descriptive statistics
##############################################

#####

# Create data frame and set variable type

# Load selected variables for regression
k_var <- data.frame(read_xlsx("variable_type.xlsx", sheet = "sex_differences"))
otcm <- as.vector(na.omit(k_var[1,2]))
sex <- as.vector(na.omit(k_var[2,2]))
cob <- as.vector(na.omit(k_var[3,2]))
edu <- as.vector(na.omit(k_var[4,2]))
pop <- as.vector(na.omit(k_var[5,2]))
yob <- as.vector(na.omit(k_var[6,2]))
dage <- as.vector(na.omit(k_var[7,2]))
lage <- as.vector(na.omit(k_var[8,2]))

if (length(otcm) == 1 & length(c(sex, cob, edu, pop, yob)) > 3) {
  
  k_frame <- DF[, c(otcm, dage, sex, cob, edu, pop, yob, lage)]
  
  if (length(otcm) == 1) {
    k_frame[,otcm] <- as.factor(k_frame[,otcm])
  }
  
  if (length(sex) == 1) {
    k_frame[,sex] <- as.factor(k_frame[,sex])
  }
  
  if (length(cob) == 1) {
    k_frame[,cob] <- as.factor(k_frame[,cob])
  }
  
  if (length(edu) == 1) {
    k_frame[,edu] <- as.factor(k_frame[,edu])
  }
  
  if (length(pop) == 1) {
    k_frame[,pop] <- as.numeric(as.character(k_frame[,pop]))
  }
  
  if (length(yob) == 1) {
    k_frame[,yob] <- as.numeric(as.character(k_frame[,yob]))
  }
  
  if (length(dage) == 1) {
    k_frame[,dage] <- as.numeric(as.character(k_frame[,dage]))
  }
  
  if (length(lage) == 1) {
    k_frame[,lage] <- as.numeric(as.character(k_frame[,lage]))
  }
  
  # Create a results frame
  Parameter <- c("Study size (% and n)",
                 "Pop. GHS NA (%)",
                 "Pop. GHS mean",
                 "Pop. GHS SD",
                 "Pop. GHS median",
                 "Pop. GHS IQR",
                 "Pop. GHS Q1 (%)",
                 "Pop. GHS Q2 (%)",
                 "Pop. GHS Q3 (%)",
                 "Pop. GHS Q4 (%)",
                 "Education NA (%)",
                 "Education low (%)",
                 "Education mid (%)",
                 "Education high (%)",
                 "COB NA (%)",
                 "COB inside (%)",
                 "COB outside (%)",
                 "Diag. age NA (%)",
                 "Diag. age mean",
                 "Diag. age SD",
                 "Diag. age median",
                 "Diag. age IQR",
                 "Last age NA (%)",
                 "Last age mean",
                 "Last age SD",
                 "Last age median",
                 "Last age IQR",
                 "YOB NA (%)",
                 "YOB mean",
                 "YOB SD",
                 "YOB median",
                 "YOB IQR")
  
  k_stat <- data.frame(parameter = Parameter,
                       study = NA,
                       positive_boys = NA,
                       negative_boys = NA,
                       positive_girls = NA,
                       negative_girls = NA)
  
  # Size of study and % (absolute) cases
  if (length(sex) == 1 & length(otcm) == 1) {
    k_stat[1,2] <- nrow(k_frame)
    k_stat[1,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1) / nrow(k_frame)) * 100
    k_stat[1,3] <- paste0(k_stat[1, 3], " ", "(",sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1), ")")
    k_stat[1,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0) / nrow(k_frame)) * 100
    k_stat[1,4] <- paste0(k_stat[1, 4], " ", "(", sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0), ")")
    k_stat[1,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1) / nrow(k_frame)) * 100
    k_stat[1,5] <- paste0(k_stat[1, 5], " ", "(", sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1), ")")
    k_stat[1,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0) / nrow(k_frame)) * 100
    k_stat[1,6] <- paste0(k_stat[1, 6], " ", "(", sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0), ")")
    
  } else {
    k_stat[1,] <- "Not provided or not available for this study"
  }
  
  b_p <- sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1)
  b_n <- sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0)
  g_p <- sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1)
  g_n <- sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0)
  
  # Population density
  if (length(otcm) == 1 & length(sex) == 1 & length(pop) == 1) {
    # NAs
    k_stat[2,2] <- sum(is.na(k_frame[,pop]))
    k_stat[2,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & is.na(k_frame[,pop])) / nrow(k_frame)) * 100
    k_stat[2,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & is.na(k_frame[,pop])) / nrow(k_frame)) * 100
    k_stat[2,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & is.na(k_frame[,pop])) / nrow(k_frame)) * 100
    k_stat[2,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & is.na(k_frame[,pop])) / nrow(k_frame)) * 100
    # mean
    k_stat[3,2] <- mean(k_frame[,pop], na.rm = T)
    k_stat[3,3] <- mean(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[3,4] <- mean(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    k_stat[3,5] <- mean(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[3,6] <- mean(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    # sd
    k_stat[4,2] <- sd(k_frame[,pop], na.rm = T)
    k_stat[4,3] <- sd(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[4,4] <- sd(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    k_stat[4,5] <- sd(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[4,6] <- sd(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    # median
    k_stat[5,2] <- median(k_frame[,pop], na.rm = T)
    k_stat[5,3] <- median(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[5,4] <- median(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    k_stat[5,5] <- median(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[5,6] <- median(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    # iqr
    k_stat[6,2] <- IQR(k_frame[,pop], na.rm = T)
    k_stat[6,3] <- IQR(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[6,4] <- IQR(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    k_stat[6,5] <- IQR(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,pop], na.rm = T)
    k_stat[6,6] <- IQR(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,pop], na.rm = T)
    
    q1 <- quantile(k_frame[, paste0(pop)], 0.25, na.rm = T)
    q2 <- quantile(k_frame[, paste0(pop)], 0.5, na.rm = T)
    q3 <- quantile(k_frame[, paste0(pop)], 0.75, na.rm = T)
    q4 <- quantile(k_frame[, paste0(pop)], 1, na.rm = T)
    
    # Q1 study
    k_stat[7,2] <- (nrow(k_frame[k_frame[,pop] <= q1, ]) / nrow(k_frame)) * 100
    # Q1 boys positive
    k_stat[7,3] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,pop] <= q1, ]) / nrow(k_frame)) * 100
    # Q1 boys negative
    k_stat[7,4] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,pop] <= q1, ]) / nrow(k_frame)) * 100
    # Q1 girls positive
    k_stat[7,5] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,pop] <= q1, ]) / nrow(k_frame)) * 100
    # Q1 girls negative
    k_stat[7,6] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,pop] <= q1, ]) / nrow(k_frame)) * 100
    
    # Q2 study
    k_stat[8,2] <- (nrow(k_frame[k_frame[,pop] > q1 & k_frame[,pop] <= q2, ]) / nrow(k_frame)) * 100
    # Q2 boys positive
    k_stat[8,3] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,pop] > q1 & k_frame[,pop] <= q2, ]) / nrow(k_frame)) * 100
    # Q2 boys negative
    k_stat[8,4] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,pop] > q1 & k_frame[,pop] <= q2, ]) / nrow(k_frame)) * 100
    # Q2 girls positive
    k_stat[8,5] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,pop] > q1 & k_frame[,pop] <= q2, ]) / nrow(k_frame)) * 100
    # Q2 girls negative
    k_stat[8,6] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,pop] > q1 & k_frame[,pop] <= q2, ]) / nrow(k_frame)) * 100
    
    # Q3 study
    k_stat[9,2] <- (nrow(k_frame[k_frame[,pop] > q2 & k_frame[,pop] <= q3, ]) / nrow(k_frame)) * 100
    # Q2 boys positive
    k_stat[9,3] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,pop] > q2 & k_frame[,pop] <= q3, ]) / nrow(k_frame)) * 100
    # Q2 boys negative
    k_stat[9,4] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,pop] > q2 & k_frame[,pop] <= q3, ]) / nrow(k_frame)) * 100
    # Q2 girls positive
    k_stat[9,5] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,pop] > q2 & k_frame[,pop] <= q3, ]) / nrow(k_frame)) * 100
    # Q2 girls negative
    k_stat[9,6] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,pop] > q2 & k_frame[,pop] <= q3, ]) / nrow(k_frame)) * 100
    
    # Q4 study
    k_stat[10,2] <- (nrow(k_frame[k_frame[,pop] > q3 & k_frame[,pop] <= q4, ]) / nrow(k_frame)) * 100
    # Q2 boys positive
    k_stat[10,3] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,pop] > q3 & k_frame[,pop] <= q4, ]) / nrow(k_frame)) * 100
    # Q2 boys negative
    k_stat[10,4] <- (nrow(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,pop] > q3 & k_frame[,pop] <= q4, ]) / nrow(k_frame)) * 100
    # Q2 girls positive
    k_stat[10,5] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,pop] > q3 & k_frame[,pop] <= q4, ]) / nrow(k_frame)) * 100
    # Q2 girls negative
    k_stat[10,6] <- (nrow(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,pop] > q3 & k_frame[,pop] <= q4, ]) / nrow(k_frame)) * 100
    
  } else {
    k_stat[c(2:10), c(2:6)] <- "Not provided or not available for this study"
  }
  
  # Education
  if (length(otcm) == 1 & length(sex) == 1 & length(edu) == 1) {
    # NAs
    k_stat[11,2] <- (sum(is.na(k_frame[,edu])) / nrow(k_frame)) * 100
    k_stat[11,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & is.na(k_frame[,edu])) / b_p) * 100
    k_stat[11,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & is.na(k_frame[,edu])) / b_n) * 100
    k_stat[11,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & is.na(k_frame[,edu])) / g_p) * 100
    k_stat[11,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & is.na(k_frame[,edu])) / g_n) * 100
    # Low education
    k_stat[12,2] <- (sum(k_frame[,edu] == 3, na.rm = TRUE) / nrow(k_frame)) * 100
    k_stat[12,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,edu] == 3, na.rm = TRUE) / b_p) * 100
    k_stat[12,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,edu] == 3, na.rm = TRUE) / b_n) * 100
    k_stat[12,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,edu] == 3, na.rm = TRUE) / g_p) * 100
    k_stat[12,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,edu] == 3, na.rm = TRUE) / g_n) * 100
    # Mid education
    k_stat[13,2] <- (sum(k_frame[,edu] == 2, na.rm = TRUE) / nrow(k_frame)) * 100
    k_stat[13,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,edu] == 2, na.rm = TRUE) / b_p) * 100
    k_stat[13,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,edu] == 2, na.rm = TRUE) / b_n) * 100
    k_stat[13,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,edu] == 2, na.rm = TRUE) / g_p) * 100
    k_stat[13,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,edu] == 2, na.rm = TRUE) / g_n) * 100
    # High education
    k_stat[14,2] <- (sum(k_frame[,edu] == 1, na.rm = TRUE) / nrow(k_frame)) * 100
    k_stat[14,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,edu] == 1, na.rm = TRUE) / b_p) * 100
    k_stat[14,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,edu] == 1, na.rm = TRUE) / b_n) * 100
    k_stat[14,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,edu] == 1, na.rm = TRUE) / g_p) * 100
    k_stat[14,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,edu] == 1, na.rm = TRUE) / g_n) * 100
  } else {
    k_stat[c(11:14), c(2:6)] <- "Not provided or not available for this study"
  }
  
  
  # country of birth
  if (length(otcm) == 1 & length(sex) == 1 & length(cob) == 1) {
    # NAs
    k_stat[15,2] <- (sum(is.na(k_frame[,cob])) / nrow(k_frame)) * 100
    k_stat[15,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & is.na(k_frame[,cob])) / nrow(k_frame)) * 100
    k_stat[15,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & is.na(k_frame[,cob])) / nrow(k_frame)) * 100
    k_stat[15,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & is.na(k_frame[,cob])) / nrow(k_frame)) * 100
    k_stat[15,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & is.na(k_frame[,cob])) / nrow(k_frame)) * 100
    # study country
    k_stat[16,2] <- (colSums(k_frame[cob] == 0, na.rm = T) / nrow(k_frame)) * 100
    k_stat[16,3] <- (colSums(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][cob] == 0, na.rm = T) / b_p) * 100
    k_stat[16,4] <- (colSums(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][cob] == 0, na.rm = T) / b_n) * 100
    k_stat[16,5] <- (colSums(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][cob] == 0, na.rm = T) / g_p) * 100
    k_stat[16,6] <- (colSums(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][cob] == 0, na.rm = T) / g_n) * 100
    # outside country
    k_stat[17,2] <- (colSums(k_frame[cob] == 1, na.rm = T) / nrow(k_frame)) * 100
    k_stat[17,3] <- (colSums(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][cob] == 1, na.rm = T) / b_p) * 100
    k_stat[17,4] <- (colSums(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][cob] == 1, na.rm = T) / b_n) * 100
    k_stat[17,5] <- (colSums(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][cob] == 1, na.rm = T) / g_p) * 100
    k_stat[17,6] <- (colSums(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][cob] == 1, na.rm = T) / g_n) * 100
  } else {
    k_stat[c(15:17), c(2:6)] <- "Not provided or not available for this study"
  }
  
  
  # Age at diagnosis
  if (length(otcm) == 1 & length(sex) == 1 & length(dage) == 1) {
    dage_set <- k_frame[k_frame[otcm] == 1, ]
    # NAs
    k_stat[18,2] <- sum(is.na(dage_set[,dage]))
    k_stat[18,3] <- (sum(dage_set[,sex] == 1 & dage_set[,otcm] == 1 & is.na(dage_set[,dage])) / nrow(k_frame)) * 100
    k_stat[18,4] <- "not applicable"
    k_stat[18,5] <- (sum(dage_set[,sex] == 2 & dage_set[,otcm] == 1 & is.na(dage_set[,dage])) / nrow(k_frame)) * 100
    k_stat[18,6] <- "not applicable"
    # mean
    k_stat[19,2] <- mean(dage_set[,dage], na.rm = T)
    k_stat[19,3] <- mean(dage_set[dage_set[,sex] == 1 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[19,4] <- "not applicable"
    k_stat[19,5] <- mean(dage_set[dage_set[,sex] == 2 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[19,6] <- "not applicable"
    # sd
    k_stat[20,2] <- sd(dage_set[,dage], na.rm = T)
    k_stat[20,3] <- sd(dage_set[dage_set[,sex] == 1 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[20,4] <- "not applicable"
    k_stat[20,5] <- sd(dage_set[dage_set[,sex] == 2 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[20,6] <- "not applicable"
    # median
    k_stat[21,2] <- median(dage_set[,dage], na.rm = T)
    k_stat[21,3] <- median(dage_set[dage_set[,sex] == 1 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[21,4] <- "not applicable"
    k_stat[21,5] <- median(dage_set[dage_set[,sex] == 2 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[21,6] <- "not applicable"
    # iqr
    k_stat[22,2] <- IQR(dage_set[,dage], na.rm = T)
    k_stat[22,3] <- IQR(dage_set[dage_set[,sex] == 1 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[22,4] <- "not applicable"
    k_stat[22,5] <- IQR(dage_set[dage_set[,sex] == 2 & dage_set[,otcm] == 1, ][,dage], na.rm = T)
    k_stat[22,6] <- "not applicable"
  } else {
    k_stat[c(18:22), c(2:6)] <- "Not provided or not available for this study"
  }
  
  
  # Age at the last assesment
  if (length(otcm) == 1 & length(sex) == 1 & length(lage) == 1) {
    lage_set <- k_frame[k_frame[otcm] == 1, ]
    # NAs
    k_stat[23,2] <- sum(is.na(lage_set[,lage]))
    k_stat[23,3] <- (sum(lage_set[,sex] == 1 & lage_set[,otcm] == 1 & is.na(lage_set[,lage])) / nrow(k_frame)) * 100
    k_stat[23,4] <- "not applicable"
    k_stat[23,5] <- (sum(lage_set[,sex] == 2 & lage_set[,otcm] == 1 & is.na(lage_set[,lage])) / nrow(k_frame)) * 100
    k_stat[23,6] <- "not applicable"
    # mean
    k_stat[24,2] <- mean(lage_set[,lage], na.rm = T)
    k_stat[24,3] <- mean(lage_set[lage_set[,sex] == 1 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[24,4] <- "not applicable"
    k_stat[24,5] <- mean(lage_set[lage_set[,sex] == 2 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[24,6] <- "not applicable"
    # sd
    k_stat[25,2] <- sd(lage_set[,lage], na.rm = T)
    k_stat[25,3] <- sd(lage_set[lage_set[,sex] == 1 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[25,4] <- "not applicable"
    k_stat[25,5] <- sd(lage_set[lage_set[,sex] == 2 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[25,6] <- "not applicable"
    # median
    k_stat[26,2] <- median(lage_set[,lage], na.rm = T)
    k_stat[26,3] <- median(lage_set[lage_set[,sex] == 1 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[26,4] <- "not applicable"
    k_stat[26,5] <- median(lage_set[lage_set[,sex] == 2 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[26,6] <- "not applicable"
    # iqr
    k_stat[27,2] <- IQR(lage_set[,lage], na.rm = T)
    k_stat[27,3] <- IQR(lage_set[lage_set[,sex] == 1 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[27,4] <- "not applicable"
    k_stat[27,5] <- IQR(lage_set[lage_set[,sex] == 2 & lage_set[,otcm] == 1, ][,lage], na.rm = T)
    k_stat[27,6] <- "not applicable"
  } else {
    k_stat[c(23:27), c(2:6)] <- "Not provided or not available for this study"
  }
  
  
  # Year of birth
  if (length(otcm) == 1 & length(sex) == 1 & length(yob) == 1) {
    # NAs
    k_stat[28,2] <- sum(is.na(k_frame[,yob]))
    k_stat[28,3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & is.na(k_frame[,yob])) / nrow(k_frame)) * 100
    k_stat[28,4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & is.na(k_frame[,yob])) / nrow(k_frame)) * 100
    k_stat[28,5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & is.na(k_frame[,yob])) / nrow(k_frame)) * 100
    k_stat[28,6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & is.na(k_frame[,yob])) / nrow(k_frame)) * 100
    # mean
    k_stat[29,2] <- mean(k_frame[,yob], na.rm = T)
    k_stat[29,3] <- mean(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[29,4] <- mean(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    k_stat[29,5] <- mean(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[29,6] <- mean(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    # sd
    k_stat[30,2] <- sd(k_frame[,yob], na.rm = T)
    k_stat[30,3] <- sd(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[30,4] <- sd(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    k_stat[30,5] <- sd(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[30,6] <- sd(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    # median
    k_stat[31,2] <- median(k_frame[,yob], na.rm = T)
    k_stat[31,3] <- median(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[31,4] <- median(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    k_stat[31,5] <- median(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[31,6] <- median(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    # iqr
    k_stat[32,2] <- IQR(k_frame[,yob], na.rm = T)
    k_stat[32,3] <- IQR(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[32,4] <- IQR(k_frame[k_frame[,sex] == 1 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    k_stat[32,5] <- IQR(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 1, ][,yob], na.rm = T)
    k_stat[32,6] <- IQR(k_frame[k_frame[,sex] == 2 & k_frame[,otcm] == 0, ][,yob], na.rm = T)
    # YOB quantiles
    yob_u <- unique(k_frame[,yob])
    yob_frame <- as.data.frame(matrix(nrow = length(yob_u),
                                      ncol = ncol(k_stat)))
    colnames(yob_frame) <- names(k_stat)
    yob_frame$parameter <- yob_u
    # fill in yob % per year
    for (i in yob_u) {
      yob_frame[which(yob_frame[1] == i),2] <- (sum(k_frame[,yob] == i) / nrow(k_frame)) * 100
      yob_frame[which(yob_frame[1] == i),3] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 1 & k_frame[,yob] == i) / b_p) * 100
      yob_frame[which(yob_frame[1] == i),4] <- (sum(k_frame[,sex] == 1 & k_frame[,otcm] == 0 & k_frame[,yob] == i) / b_n) * 100
      yob_frame[which(yob_frame[1] == i),5] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 1 & k_frame[,yob] == i) / g_p) * 100
      yob_frame[which(yob_frame[1] == i),6] <- (sum(k_frame[,sex] == 2 & k_frame[,otcm] == 0 & k_frame[,yob] == i) / g_n) * 100
    }
    k_stat <- rbind(k_stat, yob_frame)
  } else {
    k_stat[c(28:32), c(2:6)] <- "Not provided or not available for this study"
  }
  
    # Correlation matrix
    num_columns <- k_frame %>% 
      select_if(is.numeric) %>% 
      select_if(~ any(!is.na(.)))
    if (ncol(num_columns) > 1) {
      k_c_mat <- cor_test(num_columns, conf.level = 0.95, method = "spearman")
    } else {
      print("Not enough continuous variables present in the data set, correlation matrix not created.")
    }
    # Cramers V
    # https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
    # 0 = no association; 1 = 100% association between variables
    tryCatch({
      # Select the columns from DF based on the provided vector
      categorical_columns <- k_frame %>% 
        select_if(is.factor) %>% 
        select_if(~ n_distinct(.) > 1) # Keep only meaningful variables
      if (ncol(categorical_columns) > 1) {
        # Generate combinations of column pairs
        df_comb <- data.frame(t(combn(sort(names(categorical_columns)), 2)), stringsAsFactors = FALSE)
        # Apply the cramer function to each variable combination
        k_cat_corr <- map2_df(df_comb$X1, df_comb$X2, cramer)
      } else {
        print("Not enough categorical variables present in the data set, Cramers association not generated.")
      }
    }, silent = FALSE)
  
  # add outcome name
  column_names <- names(k_stat)
  outcome <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(outcome) <- column_names
  outcome [1,] <- paste0(otcm)
  k_stat <- rbind(outcome, k_stat)
  
  # Create a list of results
  frames <- list(descriptives = data.frame(),
                 spearman = data.frame(),
                 cramerV = data.frame())
  
  if (exists("k_stat")) {
    frames$descriptives <- k_stat
  }
  if (exists("k_c_mat")) {
    frames$spearman <- k_c_mat
  }
  if (exists("k_cat_corr")) {
    frames$cramerV <- k_cat_corr
  }
  # save the results
  write_xlsx(frames, paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/descriptives.xlsx")))
} else {
  print("Please assign missing variables to the sex differences sheet.")
}
#####

##############################################
# 2. Logistic regression (ADHD/ASD diagnosis)
##############################################

#####

# Set global contrasts for unordered and ordered factors
# this will ensure that ordered factors will be treated as ordered factors in glm()
# otherwise ordered factors would be treated as unordered factors in glm()
options(contrasts = c("contr.treatment", "contr.poly"))

# Karls sex differences logit code
# Load selected variables for regression
k_var <- data.frame(read_xlsx("variable_type.xlsx", sheet = "sex_differences"))
k_dep_var <- as.vector(na.omit(k_var[1,2]))
#k_ind_var <- as.vector(na.omit(k_var[c(2:6),2]))
sex <- as.vector(na.omit(k_var[2,2]))
cob <- as.vector(na.omit(k_var[3,2]))
edu <- as.vector(na.omit(k_var[4,2]))
pop <- as.vector(na.omit(k_var[5,2]))
yob <- as.vector(na.omit(k_var[6,2]))
#dage <- as.vector(na.omit(k_var[7,2]))
#k_dep_varII <- as.vector(na.omit(k_var[7,2]))

if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) > 3) {
  ## MODEL 1a
  # Autism ~ COB + EDU + POP + YOB + SEX + COB:SEX + EDU:SEX + POP:SEX + YOB:SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu,  pop, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, yob, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formula)))
  }
  ## MODEL 1b
  # Autism ~ COB + EDU + POP + YOB + SEX + COB:SEX + EDU:SEX + POP:SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modelb <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulb)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(edu,  pop, yob, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modelb <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulb)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"))
    modelb <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulb)))
  }
  ## MODEL 1c
  # Autism ~ COB + EDU + POP + YOB + SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu,  pop, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulc)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulc)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formulc)))
  }
  ## MODEL 1d
  # Autism ~ COB + EDU + POP + Sex
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, sex)) == 4) {
    formuld <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, sex), collapse = "+"))
    modeld <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formuld)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, sex)) == 3 & length(cob) == 0) {
    formuld <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, sex), collapse = "+"))
    modeld <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formuld)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, sex)) == 3 & length(pop) == 0) {
    formuld <- paste0(k_dep_var,  "~",
                      paste0(c(cob, edu, sex), collapse = "+"))
    modeld <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formuld)))
  }
  ## MODEL 1e
  # Autism  ~ COB + EDU + POP + SEX + COB:SEX + EDU:SEX + POP:SEX
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, sex)) == 4) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modele <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formule)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, sex)) == 3 & length(cob) == 0) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modele <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formule)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, sex)) == 3 & length(pop) == 0) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"))
    modele <- with(imputed_data, glm(family = binomial(link = "logit"),
                                     formula = as.formula(formule)))
  }
  # Model boys and girls separately
  
  
  # subset imputed_data (mice object) to only boys and only girls
  df_long <- complete(imputed_data ,"long", include = T)
  long_boys <- df_long[which(df_long[[sex]] == 1),]
  long_girls <- df_long[which(df_long[[sex]] == 2),]
  imputed_boys <- as.mids(long_boys)
  imputed_girls <- as.mids(long_girls)
  
  ### MODEL boys
  # Autism (boys) ~ COB + EDU + POP + YOB
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, yob)) == 4) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(cob, edu, pop, yob), collapse = "+"))
    modelboys <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                        formula = as.formula(formulboys)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, yob)) == 3 & length(cob) == 0) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(edu, pop, yob), collapse = "+"))
    modelboys <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                        formula = as.formula(formulboys)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, yob)) == 3 & length(pop) == 0) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(cob, edu, yob), collapse = "+"))
    modelboys <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                        formula = as.formula(formulboys)))
  }
  ### MODEL boys2
  # Autism (boys) ~ COB + EDU + POP
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop)) == 3) {
    formulboys2 <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, pop), collapse = "+"))
    modelboys2 <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                         formula = as.formula(formulboys2)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop)) == 2 & length(cob) == 0) {
    formulboys2 <- paste0(k_dep_var, "~",
                         paste0(c(edu, pop), collapse = "+"))
    modelboys2 <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                         formula = as.formula(formulboys2)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu)) == 2 & length(pop) == 0) {
    formulboys2 <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu), collapse = "+"))
    modelboys2 <- with(imputed_boys, glm(family = binomial(link = "logit"),
                                         formula = as.formula(formulboys2)))
  }
  ## MODEL girls
  # Autism (girls) ~ COB + EDU + POP + YOB  
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, yob)) == 4) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, pop, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                          formula = as.formula(formulgirls)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, yob)) == 3 & length(cob) == 0) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(edu, pop, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                          formula = as.formula(formulgirls)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, yob)) == 3 & length(pop) == 0) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                          formula = as.formula(formulgirls)))
  }
  ### MODEL girls2
  # Autism (girls) ~ COB + EDU + POP
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop)) == 3) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(cob, edu, pop), collapse = "+"))
    modelgirls2 <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                           formula = as.formula(formulgirls2)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop)) == 2 & length(cob) == 0) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(edu, pop), collapse = "+"))
    modelgirls2 <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                           formula = as.formula(formulgirls2)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu)) == 2 & length(pop) == 0) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(cob, edu), collapse = "+"))
    modelgirls2 <- with(imputed_girls, glm(family = binomial(link = "logit"),
                                           formula = as.formula(formulgirls2)))
  }
  
  fit_loop <- c("modela", "modelb", "modelc",
                "modeld", "modele", "modelboys",
                "modelboys2", "modelgirls", "modelgirls2")
  
  form_loop <- c(formula, formulb, formulc,
                 formuld, formule, formulboys,
                 formulboys2, formulgirls, formulgirls2)
  
  res_names <- c("res_modela", "res_modelb", "res_modelc",
                 "res_modeld", "res_modele", "res_modelboys",
                 "res_modelboys2", "res_modelgirls", "res_modelgirls2")
  
  for (i in 1:9) {
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
    # Get model summary
    fit <- get(fit_loop[i])
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    fmula <- rep(form_loop[i], time = length(term))
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
    # insert results in the file
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        fmula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        n_fitted[j])
    }
    results$"var_covar_mtrx" <- rep(c("->"), times = nrow(results))
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    results <- cbind(results[,c(1:5)], sig, results[,6:10], cov_matrix)
    colnames(results)[11] <- "Variance-Covariance Matrix"
    assign(res_names[i], results)
  }
  
  logit_sex_sheets <- list("res_modela" = res_modela, 
                           "res_modelb" = res_modelb, 
                           "res_modelc" = res_modelc,
                           "res_modeld" = res_modeld, 
                           "res_modele" = res_modele, 
                           "res_modelboys" = res_modelboys,
                           "res_modelboys2" = res_modelboys2, 
                           "res_modelgirls" = res_modelgirls, 
                           "res_modelgirls2" = res_modelgirls2)
  
  write_xlsx(logit_sex_sheets, paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/logit_regr.xlsx")))
  
} else {
  print("Logistic regression did not run, please assign missing variables to the sex differences sheet.")
}
#####

##############################################
# 3. Logistic regression: multicollinearity, linearity, regression fit, ROC/AUC, pseudo R2
##############################################

#####

if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) > 3) {
  # Multicollinearity == VIF > 5
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6900425/
  #####
  k_dep_var <- as.vector(na.omit(k_var[1,2]))
  sex <- as.vector(na.omit(k_var[2,2]))
  cob <- as.vector(na.omit(k_var[3,2]))
  edu <- as.vector(na.omit(k_var[4,2]))
  pop <- as.vector(na.omit(k_var[5,2]))
  yob <- as.vector(na.omit(k_var[6,2]))
  
  # empty data frame
  MCL <- data.frame(GVIF = numeric(),
                    df = numeric(),
                    GVIF_df = numeric(),
                    model = character(),
                    imputation = character())
  # get all GVIF in one object
  for (g in 1:9) {
    fit <- get(fit_loop[g])
    for (o in 1:5) {
      fitI <- getfit(fit, o)
      a <- gvif(fitI)
      Imputation <- rep(o, times = nrow(a))
      Model <- rep(fit_loop[g], times = nrow(a))
      a <- cbind(a, Imputation, Model)
      MCL <- rbind(MCL, a)
    }
  }
  MCL <- cbind(rownames(MCL), MCL)
  colnames(MCL)[1] <- "Variable"
  MCL[,4] <- as.numeric(MCL[,4])
  MCL$Multicollinearity <- ifelse(MCL[,4] >= 5, "TRUE", "FALSE")
  #####
  
  # Linearity
  #####
  if (linearity_plots == "ON") {
    # All
    try(
      {
        for (i in 1:5) {
          data_impu <- complete(imputed_data, i)
          xx <- paste0(k_dep_var, "~",
                       paste0(c(edu, pop, yob, sex, cob), collapse = "+"))
          model <- glm(formula = xx,
                       family = binomial(link = "logit"),
                       data = data_impu)
          # Linearity (combined model)
          probabilities <- predict(model, type = "response")
          plotdf <- data_impu[,c(pop, yob)] %>% select_if(is.numeric)
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
          ggsave(paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/"), c("Linearity/lin_imp_"), i, c(".png")),
                 plot = plot, 
                 width = NA, 
                 height = NA, 
                 dpi = 300)
        }
      }, 
      silent = TRUE
    )
    
    # Data Girls
    try(
      {
        for (i in 1:5) {
          data_impu <- complete(imputed_girls, i)
          xx <- paste0(k_dep_var, "~",
                       paste0(c(edu, pop, yob, cob), collapse = "+"))
          model <- glm(formula = xx,
                       family = binomial(link = "logit"),
                       data = data_impu)
          # Linearity (combined model)
          probabilities <- predict(model, type = "response")
          plotdf <- data_impu[,c(pop, yob)] %>% select_if(is.numeric)
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
          ggsave(paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/"), c("Linearity/lin_girls_imp_"), i, c(".png")),
                 plot = plot, 
                 width = NA, 
                 height = NA, 
                 dpi = 300)
        }
      }, 
      silent = TRUE
    )
    
    # Data boys
    try(
      {
        for (i in 1:5) {
          data_impu <- complete(imputed_boys, i)
          xx <- paste0(k_dep_var, "~",
                       paste0(c(edu, pop, yob, cob), collapse = "+"))
          model <- glm(formula = xx,
                       family = binomial(link = "logit"),
                       data = data_impu)
          # Linearity (combined model)
          probabilities <- predict(model, type = "response")
          plotdf <- data_impu[,c(pop, yob)] %>% select_if(is.numeric)
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
          ggsave(paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/"), c("Linearity/lin_boys_imp_"), i, c(".png")),
                 plot = plot, 
                 width = NA, 
                 height = NA, 
                 dpi = 300)
        }
      }, 
      silent = TRUE
    )
  }
  #####
  
  # Omnibus Goodness of Fit, Hosmer-Lemeshow test, AIC, Likelihood ratio test
  #####
  
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
  
  tryCatch(
    {
      # Initialize your result storage (assuming GOF, LRT, akaike, OB are pre-defined)
      rownumbr <- 0
      model_n <- 0
      for (k in form_loop) {
        model_n <- model_n + 1
        for (i in 1:5) {
          rownumbr <- rownumbr + 1
          
          if (model_n < 6) {
            data_impu <- complete(imputed_data, i)
          } else if (model_n == 6 | model_n == 7) {
            data_inpu <- complete(imputed_boys, i)
          } else if (model_n == 8 | model_n == 9) {
            data_impu <- complete(imputed_girls, i)
          }
          
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
              null_model <- glm(as.formula(paste0(k_dep_var, "~ 1")), family = binomial(link = "logit"), data = data_impu)
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
  # empty data frame
  
  for (i in 1:9) {
    fit <- get(fit_loop[i])
    for (k in 1:5) {
      fit_k <- getfit(fit, k)
      data_rc <-fit_k$data
      # calculate ROC and AUC
      prob <- predict(fit_k, type = c("response"))
      data_rc$prob <- prob
      #data_rc <- data_rc[complete.cases(data_rc[ , c("Edu_f_r_t")]), ]
      roc_plot <- roc(as.formula(paste0(c(dep_var),  "~", "prob")) , data = data_rc)
      png(filename = paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/"), c("ROC_AUC/ROC_AUC_plot_"), paste0(fit_loop[i]), k, c(".png")))
      plot(roc_plot) # plot
      mtext(round(auc(roc_plot), 3), # add AUC value to the plot
            side = 3,
            line = -20,
            at = 0.3,
            cex = 2)
      mtext("AUC = ", # add AUC text to the plot
            side = 3,
            line = -20,
            at = 0.5,
            cex = 2)
      dev.off()
    }
  }
  #####
  
  # Pseudo R2
  #####
  pseudo_R2 <- data.frame(X1 = NA,
                          X2 = NA,
                          X3 = NA,
                          X4 = NA,
                          X5 = NA,
                          model = NA)
  for (i in 1:9) {
    fit <- get(fit_loop[i])
    fit_k <- getfit(fit)
    # Estimate pseudo R2
    pseuso <- data.frame(sapply(fit_k, function(fit_k) pR2(fit_k)))
    model <- rep(fit_loop[i], times = nrow(pseuso))
    pseuso <- cbind(pseuso, model)
    pseudo_R2 <- rbind(pseudo_R2, pseuso)
  }
  pseudo_R2 <- pseudo_R2[-1,]
  pseudo_R2 <- cbind(rownames(pseudo_R2), pseudo_R2)
  colnames(pseudo_R2) <- c("Parameter", "Imputation 1", "Imputation 2", "Imputation 3", "Imputation 4", "Imputation 5", "Model")
  
}


# Export results

# Create an empty list
sheetsIII <- list(MCL = data.frame(),
                  GOF = data.frame(),
                  OB = data.frame(),
                  LRT = data.frame(),
                  ak_crit = data.frame(),
                  bay_crit = data.frame(),
                  Pseudo_R2 = data.frame())

# Add results to the list
if (exists("MCL")) {
  sheetsIII[[1]] <- MCL
}
if (exists("GOF")) {
  sheetsIII[[2]] <- GOF
}
if (exists("OB")) {
  sheetsIII[[3]] <- OB
}
if (exists("LRT")) {
  sheetsIII[[4]] <- LRT
}
if (exists("akaike")) {
  sheetsIII[[5]] <- akaike
}
if (exists("bay")) {
  sheetsIII[[6]] <- bay
}
if (exists("Pseudo_R2")) {
  sheetsIII[[7]] <- Pseudo_R2
}

write_xlsx(sheetsIII, paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/logit_fit.xlsx")))

#####

##############################################
# Linear regression (age at diagnosis)
##############################################

#####

# Set global contrasts for unordered and ordered factors
# this will ensure that ordered factors will be treated as ordered factors in lm()
# otherwise ordered factors would be treated as unordered factors in lm()
options(contrasts = c("contr.poly", "contr.poly"))
# Load selected variables for regression
k_var <- data.frame(read_xlsx("variable_type.xlsx", sheet = "sex_differences"))
#k_dep_var <- as.vector(na.omit(k_var[1,2]))
#k_ind_var <- as.vector(na.omit(k_var[c(2:6),2]))
sex <- as.vector(na.omit(k_var[2,2]))
cob <- as.vector(na.omit(k_var[3,2]))
edu <- as.vector(na.omit(k_var[4,2]))
pop <- as.vector(na.omit(k_var[5,2]))
yob <- as.vector(na.omit(k_var[6,2]))
k_dep_var <- as.vector(na.omit(k_var[7,2]))
#dage <- as.vector(na.omit(k_var[7,2]))
#k_dep_varII <- as.vector(na.omit(k_var[7,2]))
if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) > 3) {
  
  if (length(k_dep_var) == 1) {
    # Function to extract R-squared and adjusted R-squared from a linear model
    get_r_squared <- function(model) {
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      return(c(r_squared = r_squared, adj_r_squared = adj_r_squared))
    }
  }
  
  ## MODEL 1a
  # Autism ~ COB + EDU + POP + YOB + SEX + COB:SEX + EDU:SEX + POP:SEX + YOB:SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu,  pop, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, lm(formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, yob, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, lm(formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formula <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(yob, sex), collapse = "*"))
    modela <- with(imputed_data, lm(formula = as.formula(formula)))
  }
  ## MODEL 1b
  # Autism ~ COB + EDU + POP + YOB + SEX + COB:SEX + EDU:SEX + POP:SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu,  pop, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modelb <- with(imputed_data, lm(formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(edu,  pop, yob, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modelb <- with(imputed_data, lm(formula = as.formula(formula)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formulb <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"))
    modelb <- with(imputed_data, lm(formula = as.formula(formula)))
  }
  ## MODEL 1c
  # Autism ~ COB + EDU + POP + YOB + SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, lm(formula = as.formula(formulc)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, lm(formula = as.formula(formulc)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formulc <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, yob, sex), collapse = "+"))
    modelc <- with(imputed_data, lm(formula = as.formula(formulc)))
  }
  ## MODEL 1d
  # Autism ~ COB + EDU + POP + Sex
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, sex)) == 4) {
    formuld <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, sex), collapse = "+"))
    modeld <- with(imputed_data, lm(formula = as.formula(formuld)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, sex)) == 3 & length(cob) == 0) {
    formuld <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, sex), collapse = "+"))
    modeld <- with(imputed_data, lm(formula = as.formula(formuld)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, sex)) == 3 & length(pop) == 0) {
    formul <- paste0(k_dep_var, "~",
                     paste0(c(cob, edu, sex), collapse = "+"))
    modeld <- with(imputed_data, lm(formula = as.formula(formuld)))
  }
  ## MODEL 1e
  # Autism  ~ COB + EDU + POP + SEX + COB:SEX + EDU:SEX + POP:SEX
  if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) == 5) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, pop, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modele <- with(imputed_data, lm(formula = as.formula(formule)))
  } else if (length(k_dep_var) == 1 & length(c(sex, edu, pop, yob)) == 4 & length(cob) == 0) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(edu, pop, sex), collapse = "+"), "+",
                      paste0(c(edu, sex), collapse = "*"), "+",
                      paste0(c(pop, sex), collapse = "*"))
    modele <- with(imputed_data, lm(formula = as.formula(formule)))
  } else if (length(k_dep_var) == 1 & length(c(cob, sex, edu, yob)) == 4 & length(pop) == 0) {
    formule <- paste0(k_dep_var, "~",
                      paste0(c(cob, edu, sex), collapse = "+"), "+",
                      paste0(c(cob, sex), collapse = "*"), "+",
                      paste0(c(edu, sex), collapse = "*"))
    modele <- with(imputed_data, lm(formula = as.formula(formule)))
  }
  # Model 2 boys and girls separately
  # subset imputed_data (mice object) to only boys and only girls
  df_long <- complete(imputed_data ,"long", include = T)
  long_boys <- df_long[which(df_long[[sex]] == 1),]
  long_girls <- df_long[which(df_long[[sex]] == 2),]
  imputed_boys <- as.mids(long_boys)
  imputed_girls <- as.mids(long_girls)
  
  ### MODEL 2aboys
  # Autism (boys) ~ COB + EDU + POP + YOB
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, yob)) == 4) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(cob, edu, pop, yob), collapse = "+"))
    modelboys <- with(imputed_boys, lm(formula = as.formula(formulboys)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, yob)) == 3 & length(cob) == 0) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(edu, pop, yob), collapse = "+"))
    modelboys <- with(imputed_boys, lm(formula = as.formula(formulboys)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, yob)) == 3 & length(pop) == 0) {
    formulboys <- paste0(k_dep_var, "~",
                         paste0(c(cob, edu, yob), collapse = "+"))
    modelboys <- with(imputed_boys, lm(formula = as.formula(formulboys)))
  }
  ### MODEL 2bboys
  # Autism (boys) ~ COB + EDU + POP
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop)) == 3) {
    formulboys2 <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, pop), collapse = "+"))
    modelboys2 <- with(imputed_boys, lm(formula = as.formula(formulboys2)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop)) == 2 & length(cob) == 0) {
    formulboys2 <- paste0(k_dep_var, "~",
                          paste0(c(edu, pop), collapse = "+"))
    modelboys2 <- with(imputed_boys, lm(formula = as.formula(formulboys2)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu)) == 2 & length(pop) == 0) {
    formulboys2 <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu), collapse = "+"))
    modelboys2 <- with(imputed_boys, lm(formula = as.formula(formulboys2)))
  }
  ## MODEL 2agirls
  # Autism (girls) ~ COB + EDU + POP + YOB  
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop, yob)) == 4) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, pop, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, lm(formula = as.formula(formulgirls)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop, yob)) == 3 & length(cob) == 0) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(edu, pop, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, lm(formula = as.formula(formulgirls)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu, yob)) == 3 & length(pop) == 0) {
    formulgirls <- paste0(k_dep_var, "~",
                          paste0(c(cob, edu, yob), collapse = "+"))
    modelgirls <- with(imputed_girls, lm(formula = as.formula(formulgirls)))
  }
  ### MODEL 2bgirls
  # Autism (girls) ~ COB + EDU + POP
  if (length(k_dep_var) == 1 & length(c(cob, edu, pop)) == 3) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(cob, edu, pop), collapse = "+"))
    modelgirls2 <- with(imputed_girls, lm(formula = as.formula(formulgirls2)))
  } else if (length(k_dep_var) == 1 & length(c(edu, pop)) == 2 & length(cob) == 0) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(edu, pop), collapse = "+"))
    modelgirls2 <- with(imputed_girls, lm(formula = as.formula(formulgirls2)))
  } else if (length(k_dep_var) == 1 & length(c(cob, edu)) == 2 & length(pop) == 0) {
    formulgirls2 <- paste0(k_dep_var, "~",
                           paste0(c(cob, edu), collapse = "+"))
    modelgirls2 <- with(imputed_girls, lm(formula = as.formula(formulgirls2)))
  }
  
  form_loop <- c(formula, formulb, formulc,
                 formuld, formule, formulboys,
                 formulboys2, formulgirls, formulgirls2)
  
  for (i in 1:9) {
    results <- data.frame(
      Variable = character(),
      Formula = character(),
      Estimate = numeric(),
      Std.Error = numeric(),
      p_value = numeric(),
      OR = numeric(),
      CI_low = numeric(),
      CI_high = numeric(),
      r_2 = numeric(),
      adj_r_2 = numeric(),
      n_fitted = numeric())
    covar_mat <- data.frame(NA)
    # fit model
    fit <- get(fit_loop[i])
    # Get model summary
    summary_fit <- summary(pool(fit))
    # Get model results
    term <- as.character(summary_fit[,1])
    fmula <- rep(form_loop[i], time = length(term))
    
    
    estimate <- summary_fit[,2]
    std_error <- summary_fit[,3]
    p_value <- summary_fit[,6]
    # Get OR + 95%CI
    odds_ratio <- exp(estimate)
    ci_low <- exp(estimate - 1.96 * std_error)
    ci_high <- exp(estimate + 1.96 * std_error)
    
    # Get the covariance matrix
    cov_matrix <- data.frame(pool_vcov(fit))
    #Get R2 and adjusted R2
    r_vals <- lapply(fit$analyses, get_r_squared) # Apply the function to each imputed model
    r_sqr <- as.data.frame(do.call(rbind, r_vals)) # Convert the list to a data frame
    r_2 <- mean(r_sqr$r_squared)
    adj_r_2 <- mean(r_sqr$adj_r_squared)
    n_fitted <- nobs(fit$analyses[[1]])
    # insert results in the file
    for (j in 1:length(term)) {
      results[nrow(results) + 1,] <- list(
        term[j],
        fmula[j],
        estimate[j],
        std_error[j],
        p_value[j],
        odds_ratio[j],
        ci_low[j],
        ci_high[j],
        r_2[1],
        adj_r_2[1],
        n_fitted[j])
    }
    results$"var_covar_mtrx" <- rep(c("->"), times = nrow(results))
    sig <- ifelse(results[,"p_value"]  > 0.05, "ns",
                  ifelse(results[,"p_value"] <= 0.05 & results[,"p_value"] > 0.01, "*",
                         ifelse(results[,"p_value"] <= 0.01 & results[,"p_value"] > 0.001, "**",
                                ifelse(results[,"p_value"] <= 0.001 & results[,"p_value"] > 0.0001, "***",
                                       ifelse(results[,"p_value"] <= 0.0001, "****", results[,"p_value"])))))
    results <- cbind(results[,c(1:5)], sig, results[,6:12], cov_matrix)
    colnames(results)[13] <- "Variance-Covariance Matrix"
    colnames(results)[7] <- "RR"
    assign(res_names[i], results)
  }
  
  line_sex_sheets <- list("res_modela" = res_modela, 
                          "res_modelb" = res_modelb, 
                          "res_modelc" = res_modelc,
                          "res_modeld" = res_modeld, 
                          "res_modele" = res_modele, 
                          "res_modelboys" = res_modelboys,
                          "res_modelboys2" = res_modelboys2, 
                          "res_modelgirls" = res_modelgirls, 
                          "res_modelgirls2" = res_modelgirls2)
  
  # export results
  write_xlsx(line_sex_sheets, paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/linear_regr.xlsx")))
} else {
  print("Linear regression did not run, please assign missing variables to the sex differences sheet.")
}
#####

##############################################
# 5. Linear regression: multicollinearity and assumptions
##############################################

#####

if (length(k_dep_var) == 1 & length(c(sex, cob, edu, pop, yob)) > 3) {
  # Multicollinearity
  #####
  k_dep_var <- as.vector(na.omit(k_var[7,2]))
  sex <- as.vector(na.omit(k_var[2,2]))
  cob <- as.vector(na.omit(k_var[3,2]))
  edu <- as.vector(na.omit(k_var[4,2]))
  pop <- as.vector(na.omit(k_var[5,2]))
  yob <- as.vector(na.omit(k_var[6,2]))
  
  # empty data frame
  MCL <- data.frame(GVIF = numeric(),
                    df = numeric(),
                    GVIF_df = numeric(),
                    model = character(),
                    imputation = character())
  for (g in 1:9) {
    fit <- get(fit_loop[g])
    for (o in 1:5) {
      fitI <- getfit(fit, o)
      a <- gvif(fitI)
      Imputation <- rep(o, times = nrow(a))
      Model <- rep(fit_loop[g], times = nrow(a))
      a <- cbind(a, Imputation, Model)
      MCL <- rbind(MCL, a)
    }
  }
  MCL <- cbind(rownames(MCL), MCL)
  colnames(MCL)[1] <- "Variable"
  MCL[,4] <- as.numeric(MCL[,4])
  MCL$Multicollinearity <- ifelse(MCL[,4] >= 5, "TRUE", "FALSE")
  write_xlsx(MCL, paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/linear_multicol.xlsx")))
  #####
  
  # lm diagnostics plots
  #####
  for (j in 1:5) { # imputation iteration 
    tation <- complete(imputed_data, j)
    for (i in 1:5) { # only models working with imputed_data 
      png(filename = paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/LM_assump/"), paste0(fit_loop[i]), j, c(".png")),
          width = 600, height = 600, units = "px")
      # Fit the linear model on the ith imputed dataset
      fit <- lm(formula = as.formula(form_loop[i]), data = tation)
      # Generate diagnostic plots for the model
      par(mfrow = c(2, 2)) # Adjust the plotting area to show multiple plots
      plot(fit)
      dev.off()  
    }
  }
  
  for (j in 1:5) { # imputation iteration 
    tation <- complete(imputed_boys, j)
    for (i in 6:7) { # only models working with imputed_data 
      png(filename = paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/LM_assump/"), paste0(fit_loop[i]), j, c(".png")),
          width = 600, height = 600, units = "px")
      # Fit the linear model on the ith imputed dataset
      fit <- lm(formula = as.formula(form_loop[i]), data = tation)
      # Generate diagnostic plots for the model
      par(mfrow = c(2, 2)) # Adjust the plotting area to show multiple plots
      plot(fit)
      dev.off()  
    }
  }
  
  for (j in 1:5) { # imputation iteration 
    tation <- complete(imputed_girls, j)
    for (i in 8:9) { # only models working with imputed_data 
      png(filename = paste0(c("Results/"), paste0(f_name), c("/Sex_Differences/LM_assump/"), paste0(fit_loop[i]), j, c(".png")),
          width = 600, height = 600, units = "px")
      # Fit the linear model on the ith imputed dataset
      fit <- lm(formula = as.formula(form_loop[i]), data = tation)
      # Generate diagnostic plots for the model
      par(mfrow = c(2, 2)) # Adjust the plotting area to show multiple plots
      plot(fit)
      dev.off()  
    }
  }
  #####
}

#####
