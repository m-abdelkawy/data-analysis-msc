# ================================================================
# ANA 505 - Project Part 1
# Exploratory Data Analysis, Decision Tree, and Linear Regression
# Dataset path matches the Python notebook:
# ../datasets/sales_and_customers.csv
#
# Run this script from the same working directory used by the
# Python notebook (the projects/python folder).
#
# Required packages:
# install.packages(c("rpart", "rpart.plot"))
# ================================================================

# ----------------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------------

required_packages <- c("rpart", "rpart.plot")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(rpart)
library(rpart.plot)

DATA_PATH <- "../datasets/sales_and_customers.csv"
OUTPUT_DIR <- "project_part1_outputs"

if (!file.exists(DATA_PATH)) {
  stop(
    "CSV file not found at: ", DATA_PATH,
    "\nCurrent working directory: ", getwd(),
    "\nRun the script from the same working directory as the Python notebook, ",
    "or update DATA_PATH."
  )
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Read the data
df <- read.csv(DATA_PATH, stringsAsFactors = FALSE)

# Treat text categories as factors, not numeric measurements.
df$Preferred_Channel <- factor(df$Preferred_Channel)
df$Customer_Satisfaction <- factor(
  df$Customer_Satisfaction,
  levels = c("Low", "Medium", "High")
)

cat("\n================ DATA INSPECTION ================\n")
cat("Dataset dimensions:\n")
print(dim(df))

cat("\nColumn types:\n")
str(df)

cat("\nFirst 10 rows:\n")
print(head(df, 10))

cat("\nMissing values by column:\n")
print(colSums(is.na(df)))

# Helper: copy the plot currently displayed in the active graphics
# device to a PNG file, then close only the copied PNG device.
# The original plot remains visible in the interactive plot device.
save_current_plot <- function(filename, width = 1200, height = 800, res = 140) {
  grDevices::dev.copy(
    grDevices::png,
    filename = file.path(OUTPUT_DIR, filename),
    width = width,
    height = height,
    res = res
  )
  grDevices::dev.off()
}

# ----------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
# ----------------------------------------------------------------

numeric_vars <- c("Age", "Income", "Annual_Spend", "Number_of_Visits")

cat("\n================ DESCRIPTIVE STATISTICS ================\n")

descriptive_stats <- data.frame(
  Variable = numeric_vars,
  Mean = sapply(df[numeric_vars], mean, na.rm = TRUE),
  SD = sapply(df[numeric_vars], sd, na.rm = TRUE),
  Minimum = sapply(df[numeric_vars], min, na.rm = TRUE),
  Median = sapply(df[numeric_vars], median, na.rm = TRUE),
  Maximum = sapply(df[numeric_vars], max, na.rm = TRUE)
)

descriptive_stats[-1] <- lapply(descriptive_stats[-1], round, digits = 2)
print(descriptive_stats)

write.csv(
  descriptive_stats,
  file.path(OUTPUT_DIR, "descriptive_statistics.csv"),
  row.names = FALSE
)

cat("\nPreferred channel counts and percentages:\n")
channel_counts <- table(df$Preferred_Channel)
channel_summary <- data.frame(
  Preferred_Channel = names(channel_counts),
  Count = as.integer(channel_counts),
  Percentage = round(100 * prop.table(channel_counts), 1)
)
print(channel_summary)

cat("\nCustomer satisfaction counts and percentages:\n")
satisfaction_counts <- table(df$Customer_Satisfaction)
satisfaction_summary <- data.frame(
  Customer_Satisfaction = names(satisfaction_counts),
  Count = as.integer(satisfaction_counts),
  Percentage = round(100 * prop.table(satisfaction_counts), 1)
)
print(satisfaction_summary)

# 2.1 Histograms of numerical variables
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (v in numeric_vars) {
  hist(
    df[[v]],
    breaks = 20,
    col = "steelblue",
    border = "white",
    main = paste("Distribution of", v),
    xlab = v,
    ylab = "Frequency"
  )
}
save_current_plot("numeric_histograms.png")
par(mfrow = c(1, 1))

# 2.2 Bar charts of categorical variables
par(mfrow = c(1, 2), mar = c(5, 4, 3, 1))
barplot(
  table(df$Preferred_Channel),
  col = "steelblue",
  main = "Customers by Preferred Channel",
  xlab = "Preferred Channel",
  ylab = "Number of Customers"
)
barplot(
  table(df$Customer_Satisfaction),
  col = "darkseagreen3",
  main = "Customer Satisfaction Distribution",
  xlab = "Satisfaction",
  ylab = "Number of Customers"
)
save_current_plot("categorical_bar_charts.png")
par(mfrow = c(1, 1))

# 2.3 Scatterplots: Income/Age versus Annual Spend
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(
  df$Income, df$Annual_Spend,
  col = as.integer(df$Customer_Satisfaction),
  pch = 19,
  xlab = "Income (USD)",
  ylab = "Annual Spend (USD)",
  main = "Income vs. Annual Spend"
)
legend(
  "topleft",
  legend = levels(df$Customer_Satisfaction),
  col = seq_along(levels(df$Customer_Satisfaction)),
  pch = 19,
  bty = "n"
)

plot(
  df$Age, df$Annual_Spend,
  col = as.integer(df$Preferred_Channel),
  pch = 19,
  xlab = "Age",
  ylab = "Annual Spend (USD)",
  main = "Age vs. Annual Spend"
)
legend(
  "topleft",
  legend = levels(df$Preferred_Channel),
  col = seq_along(levels(df$Preferred_Channel)),
  pch = 19,
  
  
  
  