# Install required packages
install.packages(c("readxl", "plm", "dplyr"))

# Load libraries
library(readxl)
library(plm)
library(dplyr)

# Import data
data <- read_excel("C:/Users/DEll/Downloads/Research final data.xlsx")

# Keep only DV + IVs + controls
data <- data[, c("NPL_log",
                 "board.independence", "board.meetings", "concentrated.ownership",
                 "gender.diversity", "lending.rate", "unemployment.rate",
                 "remittance", "credit.to.deposit", "bank.size", "bank.car",
                 "first.merger", "second.merger",
                 "bank.id", "fiscal.year")]   

# Convert to panel data
pdata <- pdata.frame(data, index = c("bank.id", "fiscal.year"))

# Function to calculate descriptive stats with between/within SD
panel_desc <- function(var, id){
  overall_mean <- mean(var, na.rm = TRUE)
  overall_sd   <- sd(var, na.rm = TRUE)
  min_val      <- min(var, na.rm = TRUE)
  max_val      <- max(var, na.rm = TRUE)
  
  # Between SD (mean within each bank, then sd across banks)
  between_sd <- sd(tapply(var, id, mean, na.rm = TRUE), na.rm = TRUE)
  
  # Within SD (remove between variation)
  within_sd <- sqrt(mean(tapply(var, id, function(x) var(x, na.rm = TRUE)), na.rm = TRUE))
  
  return(c(Mean = overall_mean, SD = overall_sd, Min = min_val, Max = max_val,
           Between_SD = between_sd, Within_SD = within_sd))
}

# Apply to each variable (excluding identifiers)
results <- sapply(names(data)[!names(data) %in% c("bank.id","fiscal.year")],
                  function(v) panel_desc(data[[v]], data$bank.id))

# Transpose into a data frame
desc_table <- as.data.frame(t(results))

# Round for neatness
print(round(desc_table, 3))

