install.packages("readxl")
install.packages("psych")
# Load libraries
library(readxl)
library(psych)

# Import data
data <- read_excel("C:/Users/DEll/Downloads/without PCR.xlsx", sheet = "Sheet1")

# Select only DV and IVs
data <- data[, c("NPL_log",
                 "board independence", "board.meetings", "concentrated.ownership",
                 "gender.diversity", "lending.rate", "unemployment.rate",
                 "remittance", "credit.to.deposit", "bank.size", "bank.car",
                 "first.merger", "second.merger")]

# Descriptive Statistics
desc_stats <- psych::describe(data)
print(desc_stats)

