# 01. load mtcars dataset
df <- mtcars

# 02. Explore the Dataset
# 02-1. Review the structure of the dataset
str(df)
# 02-2. Display the first six rows of the dataset
# displays the first 5 rows of the dataframe by default unless a number is provided as input
head(df, 6)
# 02-3. Summarize the dataset
summary(df)

# 03. Calculate Descriptive Statistics
# Compute the mean, median, and standard deviation for the mpg variable

# using the summary method
summary(df$mpg)

# display mean, median, and standard deviation for the mpg variable individually
print("Display mean, median, stddev *individually*")
print(sprintf("mpg mean: %f", mean(df$mpg)))
print(sprintf("mpg median: %f", median(df$mpg)))
print(sprintf("mpg standard deviation: %f", sd(df$mpg)))


# 04. Visualize the Dataset
## 04-1. Histogram of mpg
library(ggplot2)
ggplot(df, aes(x=mpg)) + 
  geom_histogram(aes(y = after_stat(density))) +
  geom_density() + 
  labs(title = "Histogram of mpg")

# 04-2. Create a scatterplot of mpg vs. hp (horsepower):
ggplot(df, aes(x = mpg, y = hp)) +
  geom_point() +
  labs(title = "Scatterplot mpg vs hp")

# 05. Group Data by Cylinders
# 05-1. Display groups
# display(df.sort_values("cyl"))
split(df, df$cyl)

# 05-2. Calculate the average mpg for each cyl group (number of cylinders)
aggregate(mpg ~ cyl, data = df, FUN = mean)

