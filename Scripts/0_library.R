# First in Tools -> Global Options -> Packages -> Paste in primary CRAN repository:
# http://nexus.ki.se/repository/cran.r-project.org

# options(repos=c(CRAN="http://nexus.ki.se/repository/cran.r-project.org"))

packages <- c("openxlsx",
              "ResourceSelection",
              "rms", 
              "tidyverse", 
              "mice", 
              "writexl",
              "readxl",
              "rstatix",
              "matlib",
              "DescTools",
              "lsr",
              "glmtoolbox",
              "pROC",
              "pscl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(installed_packages, packages)
