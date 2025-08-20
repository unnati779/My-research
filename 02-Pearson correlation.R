# Install needed packages if not already
install.packages(c("readxl", "Hmisc", "knitr"))

library(readxl)
library(Hmisc)
library(knitr)

# Import data
data <- read_excel("C:/Users/DEll/Downloads/Research final data.xlsx")

data_corr <- data[, c("NPL_log","board.independence", "board.meetings", "concentrated.ownership",
                      "gender.diversity")]

# Pearson correlation matrix with significance
corr_res <- rcorr(as.matrix(data_corr), type = "pearson")

# Extract correlation coefficients and p-values
corr_mat <- round(corr_res$r, 3)
p_mat <- corr_res$P

# Function to add stars for significance
stars <- ifelse(p_mat < 0.01, "***",
                ifelse(p_mat < 0.05, "**",
                       ifelse(p_mat < 0.1, "*", "")))

# Combine coefficients with stars
corr_with_stars <- matrix(paste0(corr_mat, stars),
                          nrow = nrow(corr_mat),
                          dimnames = dimnames(corr_mat))

# Print nicely as a table
kable(corr_with_stars, caption = "Pearson Correlation Matrix")
