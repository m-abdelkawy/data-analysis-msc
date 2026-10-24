# Install required packages if they are not already installed
required_packages <- c("ggplot2", "dplyr")
missing_packages <- required_packages[
  !sapply(X = required_packages, FUN = requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

library(ggplot2)
library(dplyr)

#==========================================================
## 1. Read the dataset
#==========================================================
fname <- "../datasets/customer_data_week1_updated.csv"

if (!file.exists(fname)) {
  stop(
    "Dataset not found. Update file_path so it points to datasets/penguins_size.csv."
  )
}

customer_data <- read.csv(fname, stringsAsFactors = FALSE)

# Display the first few rows
head(customer_data)

#==========================================================
## 2. Descriptive Summary
#==========================================================
# Summary statistics for all variables
summary(customer_data)

# Summary statistics for numerical variables
numerical_data <- customer_data[
  c("Age", "AnnualIncome", "MembershipYears",
    "TotalPurchases", "LastPurchaseAmount")
]

summary(numerical_data)

# Standard deviations
sapply(numerical_data, sd, na.rm = TRUE)

# Display column names
names(customer_data)

# Display variable types
str(customer_data)

# Inspect unique customer segment values
unique(customer_data$Customer_Segment)

# Count customers in each segment
table(customer_data$Customer_Segment)

#==========================================================
## 3. Basic Data visualization
#==========================================================

# Missing values in each column
colSums(is.na(customer_data))

# Total number of missing values
sum(is.na(customer_data))

# Display rows containing at least one missing value
customer_data[
  !complete.cases(customer_data),
]

## 3.1 Bar chart: Customer segment distribution
# Count customers by segment
customer_segment_counts <- table(customer_data$Customer_Segment)

# Calculate percentages
customer_segment_percentages <- prop.table(
  customer_segment_counts
) * 100

# Create summary table
segment_summary <- data.frame(
  Customer_Segment = names(customer_segment_counts),
  Count = as.integer(customer_segment_counts),
  Percentage = round(
    as.numeric(customer_segment_percentages),
    1
  )
)

segment_summary

# Bar chart
ggplot(customer_data, aes(x = Customer_Segment)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Customer Segment",
    x = "Customer Segment",
    y = "Number of Customers"
  ) +
  theme_minimal()

## 3.2 Histogram: Annual income
# Calculate mean and median income
mean_income <- mean(
  customer_data$AnnualIncome,
  na.rm = TRUE
)

median_income <- median(
  customer_data$AnnualIncome,
  na.rm = TRUE
)

# Histogram
ggplot(customer_data, aes(x = AnnualIncome)) +
  geom_histogram(
    bins = 10,
    color = "white",
    fill = "steelblue"
  ) +
  geom_vline(
    xintercept = mean_income,
    color = "red",
    linetype = "solid"
  ) +
  geom_vline(
    xintercept = median_income,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Annual Income",
    x = "Annual Income in USD",
    y = "Number of Customers"
  ) +
  theme_minimal()



#==========================================================
## more analysis for reflection
#==========================================================
# Annual income summary by segment
seg_income <- customer_data %>%
  group_by(Customer_Segment) %>%
  summarise(
    Count = n(),
    Mean = round(mean(AnnualIncome, na.rm = TRUE), 0),
    Median = round(median(AnnualIncome, na.rm = TRUE), 0)
  )

seg_income

# Age summary by segment
seg_age <- customer_data %>%
  group_by(Customer_Segment) %>%
  summarise(
    Count = n(),
    Mean = round(mean(Age, na.rm = TRUE), 0),
    Median = round(median(Age, na.rm = TRUE), 0)
  )

seg_age

ggplot(
  customer_data,
  aes(
    x = Customer_Segment,
    y = AnnualIncome
  )
) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Annual Income by Customer Segment",
    x = "Customer Segment",
    y = "Annual Income (USD)"
  ) +
  theme_minimal()
