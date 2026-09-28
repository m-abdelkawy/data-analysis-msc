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

required_packages <- c("tidyverse", "car", "gridExtra", "psych", "pastecs", "moments")

# returns elements of the required_packages where the logical value is true
# if installed return true, then negate
missing_packages <- required_packages[!sapply(X=required_packages, FUN=requireNamespace, quietly=TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages, dependencies = TRUE)
}

# load packages
library(tidyverse)
library(car)
library(gridExtra)
library(psych)
library(pastecs)
library(moments)

##----------------------------------------------------------------------
# 2. Load and inspect the data
##----------------------------------------------------------------------

data(mtcars)
head(mtcars)
str(mtcars)
summary(mtcars[, c("mpg", "cyl")])

##----------------------------------------------------------------------
# Reusable code blocks
##----------------------------------------------------------------------
def_theme <- theme(
  panel.background = element_rect(fill = "grey"),
  plot.background = element_rect(fill = "grey"),
  panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
  panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2)
)

plot_hist_norm <- function(data, xvar, bins=15, title = NULL){
  
  values <- data[[xvar]]
  
  m <- mean(values, na.rm = TRUE)
  s <- sd(values, na.rm = TRUE)
  
  if (is.null(title)) {
    title <- paste("Distribution of", xvar)
  }
  
  ggplot(data, aes(x = .data[[xvar]])) +
    
    # Histogram
    geom_histogram(
      aes(y = after_stat(density)),
      bins = bins,
      fill = "skyblue",
      color = "black",
      alpha = 0.50
    ) +
    # Observed density
    geom_density(
      linewidth = 1.2
    ) +
    # Theoretical normal distribution
    stat_function(
      fun = dnorm,
      args = list(mean = m, sd = s),
      linetype = "dashed",
      linewidth = 1,
      color="red"
    ) +
    labs(
      title = title,
      x = xvar,
      y = "Density"
    ) +
    def_theme
}

plot_qq <- function(data, variable, title = NULL) {
  
  if (is.null(title)) {
    title <- paste("Q-Q Plot of", variable)
  }
  
  ggplot(data, aes(sample = .data[[variable]])) +
    geom_qq()+
    geom_qq_line(color="red", linewidth=1)+
    labs(
      title = title,
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    def_theme
}


plot_boxplot <- function(data, variable, group, title = NULL){
  
  if (is.null(title)) {
    title <- paste(variable, "by", group)
  }
  
  ggplot(mtcars, aes(
    x = .data[[group]],
    y = .data[[variable]]
  )) +
    geom_boxplot(
      width = 0.6,
      fill = "steelblue",
      alpha = 0.7
    ) +
    labs(
      title = title,
      x = group,
      y = variable
    ) +
    def_theme
}

# =============================================================================
# Part A: Normality of original mpg
# =============================================================================

##----------------------------------------------------------------------
# A-1. Histogram & Q-Q plot of original mpg
##----------------------------------------------------------------------

plot_hist_norm(data=mtcars, xvar="mpg", bins=15)

plot_qq(data=mtcars, variable="mpg")

##----------------------------------------------------------------------
# A-2. Shapiro-Wilk test
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
plot_boxplot(data=mtcars, variable="mpg", group="cyl_factor")

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

levene_mpg  <- car::leveneTest(mpg ~ cyl_factor, data=mtcars)
levene_mpg
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

# -----------------------------------------------------------------------------
# D-1. Visual normality checks
# -----------------------------------------------------------------------------
plot_hist_norm(data=mtcars, xvar="log_mpg", title="Log transformed mpg")
plot_hist_norm(data=mtcars, xvar="sqrt_mpg", title="Square root transformed mpg")

plot_qq(data=mtcars, variable="log_mpg")
plot_qq(data=mtcars, variable="sqrt_mpg")

# -----------------------------------------------------------------------------
# D-2. Shapiro-Wilk tests for transformed variables
# -----------------------------------------------------------------------------

shapiro_log <- shapiro.test(mtcars$log_mpg)
shapiro_sqrt <- shapiro.test(mtcars$sqrt_mpg)

shapiro_log
shapiro_sqrt

##----------------------------------------------------------------------
# D-3. Extra - checking normality numerically (skewness and kurtosis)
##----------------------------------------------------------------------

psych::describe(mtcars$log_mpg, type=3)
psych::describe(mtcars$sqrt_mpg, type=3)

# =============================================================================
# Part E: Homoscedasticity after transformation
# =============================================================================
# -----------------------------------------------------------------------------
# E-1. Compare transformed variances across cylinder groups
# -----------------------------------------------------------------------------

log_variances <- tapply(
  mtcars$log_mpg,
  mtcars$cyl_factor,
  var
)

sqrt_variances <- tapply(
  mtcars$sqrt_mpg,
  mtcars$cyl_factor,
  var
)

log_variances
sqrt_variances

# -----------------------------------------------------------------------------
# E-2. Boxplots for transformed variables
# -----------------------------------------------------------------------------

plot_boxplot(data=mtcars, variable="log_mpg", group="cyl_factor")
plot_boxplot(data=mtcars, variable="sqrt_mpg", group="cyl_factor")

# -----------------------------------------------------------------------------
# E-3. Levene's tests after transformation
# -----------------------------------------------------------------------------

levene_log <- leveneTest(
  log_mpg ~ cyl_factor,
  data = mtcars
)

levene_sqrt <- leveneTest(
  sqrt_mpg ~ cyl_factor,
  data = mtcars
)

levene_log
levene_sqrt


# =============================================================================
# Part F: Create summary of the analysis
# =============================================================================
normality_summary <- data.frame(
  Variable = c("mpg", "log(mpg)", "sqrt(mpg)"),
  Shapiro_W = c(
    unname(shapiro_mpg$statistic),
    unname(shapiro_log$statistic),
    unname(shapiro_sqrt$statistic)
  ),
  Shapiro_p = c(
    shapiro_mpg$p.value,
    shapiro_log$p.value,
    shapiro_sqrt$p.value
  )
)

normality_summary

# Homogeneity summary
homogeneity_summary <- data.frame(
  Variable = c("mpg", "log(mpg)", "sqrt(mpg)"),
  Levene_F = c(
    levene_mpg$`F value`[1],
    levene_log$`F value`[1],
    levene_sqrt$`F value`[1]
  ),
  Levene_p = c(
    levene_mpg$`Pr(>F)`[1],
    levene_log$`Pr(>F)`[1],
    levene_sqrt$`Pr(>F)`[1]
  )
)

homogeneity_summary

