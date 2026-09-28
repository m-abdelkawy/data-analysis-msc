#==============================================================================
## Week 5: Exploring and Transforming Data to Meet Assumptions
##
## Purpose:
##   1. Check normality of mpg in the mtcars dataset.
##   2. Check homoscedasticity of mpg across cylinder groups.
##   3. Apply log and square-root transformations if problematic.
##   4. Re-check normality and homoscedasticity.
##   5. Compare the original and transformed variables.
##
## Author: Mohammed Abdelkawy
## Created: 2026-09-27
#==============================================================================

##----------------------------------------------------------------------
# 1. Packages
# - tidyverse: data manipulation and ggplot2 visualizations
# - car: Levene's test for homoscedasticity across groups
# - gridExtra: multi-panel plot arrangement
##----------------------------------------------------------------------

required_packages <- c("tidyverse", "car", "psych", "pastecs", "moments", "lmtest")

# returns elements of the required_packages where the logical value is true
# if installed return true, then negate
missing_packages <- required_packages[!sapply(X=required_packages, FUN=requireNamespace, quietly=TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages, dependencies = TRUE)
}

# load packages
library(tidyverse)
library(car)
library(psych)
library(pastecs)
library(moments)
library(lmtest)
##----------------------------------------------------------------------
# 2. Load and inspect the data
##----------------------------------------------------------------------

data(mtcars)
head(mtcars)
str(mtcars)
summary(mtcars[, c("mpg", "cyl")])

##----------------------------------------------------------------------
# 3. default custom theme
##----------------------------------------------------------------------
def_theme <- theme(
  panel.background = element_rect(fill = "grey"),
  plot.background = element_rect(fill = "grey"),
  panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
  panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2)
)

# =============================================================================
# Part A: Normality of original mpg
# =============================================================================

##----------------------------------------------------------------------
# A-1. Histogram
##----------------------------------------------------------------------

ggplot(data=mtcars, aes(x=mpg))+
  geom_histogram(bins=15, fill="skyblue", color="black")+
  labs(title="Distribution of mpg", x="mpg", y="No# cars")+
  def_theme

ggplot(data=mtcars, aes(x=mpg))+
  geom_histogram(
    aes(y=after_stat(density)),
    bins=15,
    fill="skyblue",
    color="black",
    alpha=0.50
  )+
  stat_function(fun=dnorm, args=list(mean=mean(mtcars$mpg), sd=sd(mtcars$mpg)))+
  labs(title="mpg with normal overlay")+
  def_theme

##----------------------------------------------------------------------
# A-2. Q_Q Plot
##----------------------------------------------------------------------
# A Q-Q plot compares the observed quantiles of mpg with the
# theoretical quantiles expected under a normal distribution.
# (x, y) pairs of (observed value, normal value)

ggplot(data=mtcars, aes(sample=mpg))+
  stat_qq()+
  stat_qq_line(color="red", linewidth=1)+
  labs(title = "Q-Q Plot of Miles per Gallon",
       x = "Theoretical quantiles",
       y = "Sample quantiles")+
  def_theme

# another way
ggplot(data=mtcars, aes(sample=mpg))+
  geom_qq()+
  geom_qq_line(color="red", linewidth=1)+
  labs(title = "Q-Q Plot of Miles per Gallon",
       x = "Theoretical quantiles",
       y = "Sample quantiles")+
  def_theme

##----------------------------------------------------------------------
# A-3. Shapiro-Wilk test
##----------------------------------------------------------------------
# H0: The population distribution is normal.
# H1: The population distribution is not normal.
#
# p < .05:
#   Reject H0 -> evidence against normality.
#
# p >= .05:
#   Fail to reject H0 -> insufficient evidence against normality.

shapiro_mpg <- shapiro.test(mtcars$mpg)
shapiro_mpg


##----------------------------------------------------------------------
# A-4. Extra - checking normality numerically (skewness and kurtosis)
##----------------------------------------------------------------------

# Display descriptive statistics, including skewness and kurtosis
# Type 1 — the uncorrected/basic moment-based estimates.
# Type 2 — bias-corrected estimates.
# Type 3 (the default, used by the book) — another bias-corrected convention, with a different finite-sample correction.
psych::describe(mtcars$mpg, type=3)

# Display normality-related statistics
pastecs::stat.desc(mtcars$mpg, basic = FALSE, norm = TRUE)

# note: kurtosis in both functions above is excess kurtosis = normal kurtosis - 3

# those have different calculations
moments::skewness(mtcars$mpg)
moments::kurtosis(mtcars$mpg)

# =============================================================================
# Part B: Homoscedasticity of original mpg
# =============================================================================
##----------------------------------------------------------------------
# 1. Make a categorical variable out of the cyl
##----------------------------------------------------------------------

mtcars$cyl_factor <- factor(mtcars$cyl)
table(mtcars$cyl_factor)
dim(mtcars)

##----------------------------------------------------------------------
# B-1. Compare Variance across "cyl" groups:
##----------------------------------------------------------------------
# Variance measures the amount of spread within each group.
#
# The homogeneity of variance assumption means that the group
# variances should be reasonably similar.
mpg_cyl_var <- tapply(mtcars$mpg, mtcars$cyl_factor, var)
mpg_cyl_var

# another way separate one group # just a trial
# cyl_4 = subset(x=mtcars$mpg, mtcars$cyl_factor==4)
# cyl_6 = subset(x=mtcars$mpg, mtcars$cyl_factor==6)
# cyl_8 = subset(x=mtcars$mpg, mtcars$cyl_factor==8)
# var(cyl_4)
# var(cyl_6)
# var(cyl_8)

mpg_sd <- tapply(
  mtcars$mpg,
  mtcars$cyl_factor,
  sd
)
mpg_n <- table(mtcars$cyl_factor)


variance_table <- data.frame(
  Cylinders = names(mpg_cyl_var),
  N = as.numeric(mpg_n),
  Variance = as.numeric(mpg_cyl_var),
  SD = as.numeric(mpg_sd)
)

variance_table

# check visually if variance is close across groups:
ggplot(mtcars, aes(
  x = cyl_factor,
  y = mpg
)) +
  geom_boxplot(
    width = 0.6,
    fill = "steelblue",
    alpha = 0.7
  ) +
  labs(
    title = "Miles per Gallon by cyl",
    x = "Number of cylinders",
    y = "Miles per gallon"
  ) +
  def_theme

##----------------------------------------------------------------------
# B-2. Levene's test:
##----------------------------------------------------------------------
# Levene's test formally evaluates the homogeneity of variance assumption.
#
# H0: The population variances are equal across groups.
# H1: At least one group has a different variance.
#
# p < .05:
#   Evidence that the variances are not equal.
#
# p >= .05:
#   Insufficient evidence that the variances differ.

car::leveneTest(mpg ~ cyl_factor, data=mtcars)

# B-2-2. Breusch test:
lmtest::bptest(lm(mpg ~ cyl_factor, data=mtcars))

# =============================================================================
# Part C: Create transformations
# =============================================================================

# -----------------------------------------------------------------------------
# C-1. Log transformation
# -----------------------------------------------------------------------------
# The logarithm compresses larger values more strongly than smaller values.
#
# This can be useful when a variable is positively skewed or when
# variability increases with the level of the variable.

mtcars$log_mpg <- log(mtcars$mpg)

# -----------------------------------------------------------------------------
# C-2. Square root transformation
# -----------------------------------------------------------------------------
# The square-root transformation also compresses larger values,
# yet less strongly than the logarithm.

mtcars$sqrt_mpg <- sqrt(mtcars$mpg)

# Check that the transformed variables were created correctly.
head(mtcars[, c("mpg", "log_mpg", "sqrt_mpg")])

# =============================================================================
# Part D: Normality after transformation
# =============================================================================

