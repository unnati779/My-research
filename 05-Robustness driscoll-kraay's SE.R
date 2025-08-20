# Install required packages if not already installed
install.packages(c("readxl", "plm", "lmtest", "sandwich"))

# Load libraries
library(readxl)
library(plm)
library(lmtest)
library(sandwich)

# Import dataset (adjust sheet name if needed)
data <- read_excel("C:/Users/DEll/Downloads/Research final data.xlsx")

# Make sure your panel identifiers are properly set
# Example: if you have 'bank' as entity and 'year' as time
# Replace 'bank' and 'year' with your actual column names
pdata <- pdata.frame(data, index = c("bank.id", "fiscal.year"))
# Main regression
model <- plm(
  PCR ~ board.independence + board.meetings + concentrated.ownership + gender.diversity +
    lending.rate + unemployment.rate + remittance + credit.to.deposit + bank.size + bank.car +
    first.merger + second.merger,
  data = pdata,
  model = "within"   # or "random" if you want Random Effects
)

# Driscoll-Kraay robust test
coeftest(model, vcov = vcovSCC(model, type = "HC1"))

