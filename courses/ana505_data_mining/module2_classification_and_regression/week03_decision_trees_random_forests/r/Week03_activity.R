# ============================================================
# Week 3 Activity: Decision Tree Classification
# Predict Employee Performance
# ============================================================

# Required packages
# install.packages(c("caTools", "rpart", "rpart.plot"))

# Install and load required packages
required_packages <- c("rpart", "rpart.plot", "caTools", "caret", "dplyr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(dplyr)


# 1. Load and inspect the dataset ------------------------------

employees <- read.csv("../datasets/employee_performance.csv")

# Inspect the data
str(employees)
summary(employees)
head(employees)

# Check target distribution
table(employees$Performance)


# 2. Prepare predictors and target -----------------------------

# Exclude the employee identifier and target from predictors.

id_column <- intersect(
  c("EmployeeID", "Employee_ID"),
  names(employees)
)

x <- employees[, !names(employees) %in%
                 c(id_column, "Performance"),
               drop = FALSE]

y <- factor(employees$Performance)

# Convert categorical predictors to factors.
# rpart handles factors directly; one-hot encoding is not needed.

x[] <- lapply(x, function(column) {
  if (is.character(column)) factor(column) else column
})

# Combine predictors and target for splitting.

data <- data.frame(x, Performance = y)


# 3. Split into training and testing sets ----------------------

# Use a reproducible, stratified 70/30 split.

set.seed(42)

split <- sample.split(data$Performance, SplitRatio = 0.70)

train_data <- data[split, ]
test_data  <- data[!split, ]

# Display subset sizes and class distributions.

cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

table(train_data$Performance)
table(test_data$Performance)


# 4. Build the decision tree ----------------------------------

# method = "class" specifies classification.
# Gini impurity is used to evaluate candidate splits.
# maxdepth limits the tree to four levels.

dt_model <- rpart(
  Performance ~ .,
  data = train_data,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(maxdepth = 4)
)

# Inspect the fitted model.

print(dt_model)

cat(
  "Tree depth:",
  max(dt_model$frame$depth),
  "\n"
)

cat(
  "Number of leaves:",
  sum(dt_model$frame$var == "<leaf>"),
  "\n"
)


# 5. Visualize the decision tree ------------------------------

# rpart.plot provides an R-native tree visualization.
#
# type = 2: Display split conditions at internal nodes.
# extra = 104: Show predicted class and class probabilities.
# fallen.leaves: Align terminal nodes along the bottom.
# nn: Display node numbers.

rpart.plot(
  dt_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "GnBu",
  nn = TRUE,
  roundint = FALSE,
  cex = 0.9,
  main = "Employee Performance Decision Tree"
)


# 6. Export the tree visualization ----------------------------

# Save a high-resolution image for the assignment PDF.

png(
  filename = "employee_performance_tree.png",
  width = 2400,
  height = 1600,
  res = 300
)

rpart.plot(
  dt_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "GnBu",
  nn = TRUE,
  roundint = FALSE,
  cex = 1,
  main = "Employee Performance Decision Tree"
)

dev.off()


# 7. Predict on the test set ----------------------------------

predictions <- predict(
  dt_model,
  newdata = test_data,
  type = "class"
)


# 8. Evaluate the model ---------------------------------------

# Confusion matrix:
# Rows = actual classes
# Columns = predicted classes

cm <- table(
  Actual = test_data$Performance,
  Predicted = predictions
)

cat("\nConfusion Matrix:\n")
print(cm)

# Accuracy = correctly classified observations / total.

accuracy <- sum(diag(cm)) / sum(cm)

cat(
  "\nTest Accuracy:",
  round(accuracy, 3),
  "\n"
)

cat(
  "Accuracy Percentage:",
  round(accuracy * 100, 2),
  "%\n"
)

