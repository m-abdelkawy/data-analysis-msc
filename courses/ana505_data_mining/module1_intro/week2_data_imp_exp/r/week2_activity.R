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
fname <- "../datasets/employee_data_week2.csv"

if (!file.exists(fname)) {
  stop(
    "Dataset not found. Make sure employee_data_week2.csv ",
    "is in the ../datasets/ folder."
  )
}

employee_data <- read.csv(fname, stringsAsFactors = FALSE)

# Display the first few rows
head(employee_data)

# Display the dataset structure
str(employee_data)

#==========================================================
## 2. Data exploration
#==========================================================
# Summary statistics for all variables
summary(employee_data)

# Summary statistics for Salary and Experience
required_variables <- c("Salary", "Experience")

summary(employee_data[required_variables])

# Calculate requested statistics
stats <- data.frame(
  Variable = required_variables,
  Mean = sapply(
    employee_data[required_variables],
    mean,
    na.rm = TRUE
  ),
  Median = sapply(
    employee_data[required_variables],
    median,
    na.rm = TRUE
  ),
  Variance = sapply(
    employee_data[required_variables],
    var,
    na.rm = TRUE
  ),
  Standard_Deviation = sapply(
    employee_data[required_variables],
    sd,
    na.rm = TRUE
  ),
  Minimum = sapply(
    employee_data[required_variables],
    min,
    na.rm = TRUE
  ),
  Maximum = sapply(
    employee_data[required_variables],
    max,
    na.rm = TRUE
  ),
  Range = sapply(
    employee_data[required_variables],
    function(x) diff(range(x, na.rm = TRUE))
  )
)

stats

# Check for missing values
colSums(is.na(employee_data))

#==========================================================
## 3. Visualizations
#==========================================================

## 3.1 Histogram: Salary
# Calculate mean and median salary
mean_salary <- mean(employee_data$Salary, na.rm = TRUE)
median_salary <- median(employee_data$Salary, na.rm = TRUE)

# Create salary histogram
ggplot(employee_data, aes(x = Salary)) +
  geom_histogram(
    bins = 15,
    color = "white",
    fill = "steelblue"
  ) +
  geom_vline(
    xintercept = mean_salary,
    color = "red",
    linetype = "solid"
  ) +
  geom_vline(
    xintercept = median_salary,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Employee Salary",
    x = "Employee Salary",
    y = "Number of Employees"
  ) +
  theme_minimal()

## 3.2 Scatterplot: Salary and Experience by department
ggplot(
  employee_data,
  aes(x = Salary, y = Experience, color = Department)
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Salary vs. Experience by Department",
    x = "Employee Salary",
    y = "Experience in Years",
    color = "Department"
  ) +
  theme_minimal()

## 3.3 Scatterplot: Salary and Experience by education level
ggplot(
  employee_data,
  aes(x = Salary, y = Experience, color = Education_Level)
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Salary vs. Experience by Education Level",
    x = "Employee Salary",
    y = "Experience in Years",
    color = "Education Level"
  ) +
  theme_minimal()

## 3.4 Scatterplot: Engineering employees
engineering_data <- employee_data %>%
  filter(Department == "Engineering")

ggplot(
  engineering_data,
  aes(x = Salary, y = Experience)
) +
  geom_point(color = "steelblue") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Salary vs. Experience - Engineering",
    x = "Employee Salary",
    y = "Experience in Years"
  ) +
  theme_minimal()

#==========================================================
## 4. Data export
#==========================================================
# Create three salary categories using tertile cutoffs,
# matching the Python qcut(..., q = 3) code.
salary_cutoffs <- quantile(
  employee_data$Salary,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE
)

employee_data$Salary_Category <- cut(
  employee_data$Salary,
  breaks = c(-Inf, salary_cutoffs, Inf),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Review the salary categories
table(employee_data$Salary_Category)

# Save the modified dataset
modified_path <- "../datasets/employee_data_week2_modified.csv"

write.csv(
  employee_data,
  file = modified_path,
  row.names = FALSE
)

cat("Modified dataset saved to:", modified_path, "\n")
