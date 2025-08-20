# Install packages if not already installed
install.packages(c("plm", "dplyr", "tidyverse", "readxl"))

# Load libraries
library(plm)
library(dplyr)
library(tidyverse)
library(readxl)

# 1. Load Data
df <- read_excel("C:/Users/DEll/Downloads/Research final data.xlsx")
# 2. Clean / Prepare Data
# Convert fiscal year to numeric (take first 4 digits)
df <- df %>%
  mutate(fiscal.year.num = as.numeric(substr(fiscal.year, 1, 4)))

# Remove duplicate observations (bank.id x fiscal.year.num)
df <- df %>%
  distinct(bank.id, fiscal.year.num, .keep_all = TRUE)
# 3. Declare Panel Data
pdata <- pdata.frame(df, index = c("bank.id", "fiscal.year.num"))

# 4. Optional: Check for duplicates
df %>%
  group_by(bank.id, fiscal.year.num) %>%
  filter(n() > 1)   # Should return empty if duplicates removed

# 5. Create lagged dependent variable 
df <- df %>%
  group_by(bank.id) %>%
  arrange(fiscal.year.num) %>%
  mutate(NPL_log_lag = lag(NPL_log)) %>%
  ungroup()

# Re-declare pdata.frame after new column creation
pdata <- pdata.frame(df, index = c("bank.id", "fiscal.year.num"))

# 6. Run Dynamic Panel GMM (Arellano-Bond style)
model_gmm <- pgmm(
  formula = NPL_log ~ lag(NPL_log, 1) + board.independence + board.meetings +
    concentrated.ownership + gender.diversity + lending.rate +
    unemployment.rate + remittance + credit.to.deposit + bank.size +
    bank.car + first.merger + second.merger | lag(NPL_log, 2:3),
  data = pdata,
  effect = "individual",
  model = "twosteps",
  transformation = "d"
  
# 7. Summary of Results
summary(model_gmm)

