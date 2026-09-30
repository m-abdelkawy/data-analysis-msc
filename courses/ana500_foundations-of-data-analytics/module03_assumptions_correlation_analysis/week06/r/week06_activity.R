# ============================================================
# WEEK 6: ANALYZING AND VISUALIZING CORRELATIONS
# ============================================================
#
# This script performs the analyses required for Week 6:
#
# 1. Pearson, Spearman, and Kendall correlations
# 2. Significance tests for the correlations
# 3. Scatterplot with a linear trend line
# 4. Point-biserial correlation between education and marriage
# 5. Partial correlation between hours online and hours worked,
#    controlling for age
#
# Author: Mohammed Abdelkawy
# Created: 2026-09-30
# ============================================================


# ============================================================
# 0. LOAD PACKAGES
# ============================================================

# Install these packages once if they are not already installed.
required_packages <- c("readxl", "ggplot2", "ppcor")

# returns elements of the required_packages where the logical value is true
# if installed return true, then negate
missing_packages <- required_packages[!sapply(X=required_packages, FUN=requireNamespace, quietly=TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages, dependencies = TRUE)
}

# load packages
library(readxl)
library(ggplot2)
library(ppcor)


# ============================================================
# 1. IMPORT THE DATA
# ============================================================

# Import the Excel file.
# Make sure "gss data.xlsx" is in your working directory and change the path below accordingly
file_path <- file.path(
  "../datasets/gss data.xlsx"
)

gss <- read_excel(file_path)


# Look at the first few rows
head(gss)

# Examine the structure of the dataset
str(gss)

# Get basic descriptive statistics
summary(gss)


# ============================================================
# 2. CHECK FOR MISSING VALUES
# ============================================================

# Count missing values for each variable.
colSums(is.na(gss))

# For the main correlation analysis, we need observations
# that have values for hrsonline, hrsworked, and age.
#
# complete.cases() identifies rows where none of these
# variables are missing.

correlation_data <- gss[
  complete.cases(gss[, c("hrsonline", "hrsworked", "age")]),
  c("hrsonline", "hrsworked", "age")
]

# Number of complete observations
nrow(correlation_data)
head(correlation_data)

# ============================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================

# Compare the mean and median of hours online and hours worked.
#
# Comparing the mean and median helps us think about the
# shape of the distributions
summary(correlation_data)


# ============================================================
# 4. PEARSON CORRELATION
# ============================================================

# Pearson's correlation measures the direction and strength
# of the linear relationship between two quantitative variables.
#
# cor() calculates the correlation coefficient.

pearson_r <- cor(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "pearson"
)

pearson_r


# ------------------------------------------------------------
# Test the significance of Pearson's correlation
# ------------------------------------------------------------

# cor.test() calculates the correlation and performs the
# hypothesis test for whether the population correlation
# differs from zero.

pearson_test <- cor.test(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "pearson"
)

pearson_test


# The Pearson correlation can also be squared to obtain
# R-squared, which represents the proportion of variation
# accounted for by the linear relationship.

pearson_r^2


# ============================================================
# 5. SPEARMAN CORRELATION
# ============================================================

# Spearman's correlation is a rank-based, nonparametric
# measure of association.
#
# It is useful as a complementary analysis when there are
# concerns about assumptions underlying Pearson's correlation.

spearman_rho <- cor(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "spearman"
)

spearman_rho


# Test the significance of Spearman's correlation.
#
# exact = FALSE is used because the data contain tied values,
# which is common with variables such as hours worked.

spearman_test <- cor.test(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "spearman",
  exact = FALSE
)

spearman_test


# ============================================================
# 6. KENDALL CORRELATION
# ============================================================

# Kendall's tau is another nonparametric measure of
# association based on the ordering of observations.

kendall_tau <- cor(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "kendall"
)

kendall_tau


# Test the significance of Kendall's correlation.

kendall_test <- cor.test(
  correlation_data$hrsonline,
  correlation_data$hrsworked,
  method = "kendall"
)

kendall_test


# ============================================================
# 7. SUMMARY OF THE THREE CORRELATIONS
# ============================================================
correlation_results <- data.frame(
  Method = c("Pearson", "Spearman", "Kendall"),
  
  Correlation = c(
    pearson_test$estimate,
    spearman_test$estimate,
    kendall_test$estimate
  ),
  
  P_Value = c(
    pearson_test$p.value,
    spearman_test$p.value,
    kendall_test$p.value
  )
)

correlation_results


# ============================================================
# 8. SCATTERPLOT WITH LINEAR TREND LINE
# ============================================================

# Create a scatterplot showing the relationship between
# hours online and hours worked.
#
# geom_point() creates the individual observations.
#
# geom_smooth(method = "lm") adds a fitted linear regression
# line. The shaded area represents the confidence interval
# around the fitted line.

ggplot(
  correlation_data,
  aes(x = hrsonline, y = hrsworked)
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Hours Online vs Hours Worked",
    x = "Hours online",
    y = "Hours worked"
  ) +
  theme_minimal()


# ============================================================
# 9. LINEAR REGRESSION
# ============================================================

# lm() fits a linear regression model.
#
# hrsworked is the outcome (dependent variable).
# hrsonline is the predictor (independent variable).

hours_model <- lm(
  hrsworked ~ hrsonline,
  data = correlation_data
)

# Display the regression results

summary(hours_model)


# Display the intercept and slope

coef(hours_model)


# The regression equation has the form:
#
# predicted hrsworked =
#       intercept + slope * hrsonline


# ============================================================
# 10. POINT-BISERIAL CORRELATION
# ============================================================

# Point-biserial correlation between years of education (educ) and being married (married).
#
# A point-biserial correlation is a Pearson correlation
# between:
#
#   - one continuous variable
#   - one dichotomous variable
#
# Because married is coded as 0/1, Pearson's correlation
# can be used to calculate the point-biserial correlation.

pointbiserial_data <- gss[
  complete.cases(gss[, c("educ", "married")]),
  c("educ", "married")
]


# Calculate and test the point-biserial correlation.

pb_test <- cor.test(
  pointbiserial_data$educ,
  pointbiserial_data$married,
  method = "pearson"
)

pb_test


# ------------------------------------------------------------
# Compare mean education between the two marital groups
# ------------------------------------------------------------

# This helps us understand the direction of the
# point-biserial correlation.

aggregate(
  educ ~ married,
  data = pointbiserial_data,
  FUN = mean
)


# ============================================================
# 11. PARTIAL CORRELATION
# ============================================================

# The correlation between:
#
#   hrsonline
#   hrsworked
#
# while controlling for:
#
#   age
#
# pcor.test() calculates a partial correlation.
#
# Conceptually, partial correlation asks:
#
# "What is the relationship between hours online and hours
# worked after removing the linear effect of age from both
# variables?"

partial_data <- gss[
  complete.cases(gss[, c("hrsonline", "hrsworked", "age")]),
  c("hrsonline", "hrsworked", "age")
]


# Calculate the partial correlation.

partial_test <- pcor.test(
  partial_data$hrsonline,
  partial_data$hrsworked,
  partial_data$age
)

partial_test

