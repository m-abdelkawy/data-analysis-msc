## ============================================================
## Week 2 Activity: The Mean as a Statistical Model (rivers)
## ============================================================


## ------------------------------------------------------------
## Step 1. Setup and Load Dataset
## ------------------------------------------------------------
data(rivers)
str(rivers)

## ------------------------------------------------------------
## Step 2. Estimate the Mean Length of the Rivers
## ------------------------------------------------------------
mean_length <- mean(rivers)
print(paste("Mean Length: ", mean_length))

## ------------------------------------------------------------
## Step 3.Produce a graph showing your model of the mean fit to the data. 
## Hint: create an index variable to identify each river and use a scatterplot
## ------------------------------------------------------------

#index for the x-axis
river_index <- 1:length(rivers)

#scatter plot
plot(x=river_index, y=rivers, 
     xlab="River", ylab="Length (miles)", 
     main="Mean Model fit to river length", 
     col="blue", pch=20)

grid()

abline(h=mean_length, col="red", lwd=2, lty=2)

legend("topright", 
       legend=paste("Mean: ", round(mean_length, 2)),
       lty=2,
       col="red")

## ------------------------------------------------------------
## Step 4. Calculate the standard error for the mean length.
## ------------------------------------------------------------

# standard deviation, sd(...) by default runs the sd using (n-1), i.e the sample sd
s <- sd(rivers)
# sample size
n <- length(rivers)

# standard error
se <- s/sqrt(n)

print(paste(
  "Sample standard deviation: ", round(s, 2),
  "sample size: ", n,
  "standard error: ", round(se, 2)
))

## ------------------------------------------------------------
## Step 5. Calculate a 95% confidence interval for the mean length.
## ------------------------------------------------------------

# Critical t-value
t_critical95 <- qt(0.975, df = n - 1)
# 0.975 = cumulative probability for a 95% two-sided CI
# n - 1 = degrees of freedom

# confidence interval
lower_bound95 <- mean_length - t_critical95 * se
upper_bound95 <- mean_length + t_critical95 * se

#print(paste(
#  "95% CI: (", round(lower_bound, 2), "to", round(upper_bound, 2), ")"
#))

cat(sprintf(
  "95%% Confidence Interval: (%.2f, %.2f)\n",
  lower_bound95,
  upper_bound95
))

## ------------------------------------------------------------
## Step 6. Calculate a 90% confidence interval for the mean length.
## ------------------------------------------------------------

# Critical t-value
t_critical90 <- qt(0.95, df = n - 1)
# 0.95 = cumulative probability for a 90% two-sided CI
# n - 1 = degrees of freedom

# confidence interval
lower_bound90 <- mean_length - t_critical90 * se
upper_bound90 <- mean_length + t_critical90 * se

#print(paste(
#  "90% CI: (", round(lower_bound, 2), "to", round(upper_bound, 2), ")"
#))

cat(sprintf(
  "90%% Confidence Interval: (%.2f, %.2f)\n",
  lower_bound90,
  upper_bound90
))

