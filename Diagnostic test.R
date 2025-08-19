# install.packages(c("plm", "lmtest", "readxl"))

library(plm)
library(lmtest)
library(readxl)

# Load your data
df <- read_excel("C:/Users/DEll/Downloads/Research final data.xlsx", sheet = "Sheet1")

# Declare panel structure
pdata <- pdata.frame(df, index = c("bank.id", "fiscal.year"))
# Fixed Effects model
fe_model <- plm(
  NPL_log ~ board.independence + board.meetings + concentrated.ownership + gender.diversity +
    lending.rate + unemployment.rate + remittance + credit.to.deposit +
    bank.size + bank.car + first.merger + second.merger,
  data = pdata,
  model = "within"
)

# Random Effects model
re_model <- plm(
  NPL_log ~ board.independence + board.meetings + concentrated.ownership + gender.diversity +
    lending.rate + unemployment.rate + remittance + credit.to.deposit +
    bank.size + bank.car + first.merger + second.merger,
  data = pdata,
  model = "random"
)
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)
lm_test <- plmtest(re_model, type = "bp")  # Breusch-Pagan LM test
print(lm_test)

#Cross-sectional dependence
# Pesaran CD test for cross-sectional dependence
pcd_test <- pcdtest(re_model, test = "cd")
print(pcd_test)
