# install.packages(c("plm", "lmtest", "sandwich"))

library(plm)
library(lmtest)
library(sandwich)

# Random Effects model
re_model <- plm(
  NPL_log ~ board.independence + board.meetings + concentrated.ownership + gender.diversity +
    lending.rate + unemployment.rate + remittance + credit.to.deposit +
    bank.size + bank.car + first.merger + second.merger,
  data = pdata,
  model = "random"
)

# Driscoll-Kraay Robust Standard Errors
dk_se <- vcovSCC(re_model, type = "HC1", cluster = "time")

# Show results with DK SEs
coeftest(re_model, vcov = dk_se)
