
DF <- data.frame(read.csv("Data/master_data.csv"))

# Guestimate the data type
format <- function(x) {
  if (length(unique(x)) > 14) {
    return("C")
  } else {
    return("U")  # O for "Other"
  }
}

variable_type <- data.frame(Variable_Name = names(DF),
                            n_Uniques = sapply(DF, function(x) length(unique(x))),
                            Type = sapply(DF, format),
                            Missing_Perc = (colSums(is.na(DF)) / nrow(DF)) * 100,
                            Dependent_Variable = NA,
                            Independent_Variables = NA,
                            Covariates = NA,
                            leave_empty1 = NA,
                            leave_empty2 = NA,
                            leave_empty3 = NA)

Parameter <- c("ADHD or Autism variable",
               "Sex of the child",
               "Age of child at diagnosis",
               "Mothers age at birth",
               "Fathers age at birth",
               "Mothers BMI",
               "Smoking during pregnancy",
               "Income variable",
               "Mothers education",
               "Mothers county of birth",
               "Fathers education",
               "Fathers country of birth",
               "Child birth year")

desc_stat <- data.frame(Parameter = Parameter,
                        Variable_Name = NA)

sex <- data.frame(Variable = c("Autism/ADHD",
                               "Child Sex",
                               "Mothers country of birth",
                               "Mothers education",
                               "Pop. density (GHS)",
                               "Child birthyear",
                               "Child age at diagnosis",
                               "Child age at the last measurement"),
                              Variable_Name = NA)

sheets <- list(variable_type = variable_type,
               desc_stat = desc_stat,
               sex_differences = sex)

# Create a new workbook
wb <- createWorkbook()
center_style <- createStyle(halign = "center", valign = "center")
clmns <- list(c(1:10), c(1:2), c(1:2)) 
wdths <- list(c(21, 15, 11, 16, 22, 25, 14, 16, 16, 16), c(25, 17), c(32, 17))

# Write each data frame to a separate sheet
for (i in seq_along(sheets)) {
  sheet_name <- names(sheets)[i]
  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet = sheet_name, x = sheets[[i]], tableStyle = "TableStyleLight1")
  addStyle(wb, sheet = sheet_name, style = center_style, rows = 1:(nrow(sheets[[i]]) + 1), cols = 1:ncol(sheets[[i]]), gridExpand = TRUE)
  setColWidths(wb, sheet = sheet_name, cols = clmns[[i]], widths = wdths[[i]])
}

# Save the workbook to a file
saveWorkbook(wb, file = "variable_type.xlsx", overwrite = TRUE)
