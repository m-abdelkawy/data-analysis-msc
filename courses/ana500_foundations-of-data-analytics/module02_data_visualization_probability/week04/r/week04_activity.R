##################################################################
## Week 4 Activity: Understanding and Simulating Distributions
##################################################################

## ------------------------------------------------------------
## Step 1: Simulating Data
## ------------------------------------------------------------
# Set seed for reproducibility
set.seed(123)

## normal distribution: mean = 20, variance = 25, sd = sqrt(variance) = 5
normal_data <- rnorm(n=100, mean=20, sd=5)

# Uniform distribution: min = 10, max = 30
uniform_data <- runif(n=100, min=10, max=30)

# Binomial distribution: size = 10 trials, prob = 0.5
binomial_data <- rbinom(n=100, size=10, prob=0.5)

## ------------------------------------------------------------
## Step 2: Visualize Simulated Data
## ------------------------------------------------------------
# Configure graphical window layout: 2 row, 2 columns
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1.5))

# Normal histogram with theoretical density curve
hist(
  x=normal_data, 
  breaks = 10,
  main = "Normal Distribution", 
  xlab = "Value",
  col = "lightblue",
  border = "black",
  probability = TRUE)# show density instead of counts
curve(dnorm(x, mean = 20, sd = 5),
      col = "red", lwd = 2, add = TRUE)
abline(v = mean(normal_data), col = "blue", lwd = 2, lty = 2)

# Uniform histogram with theoretical density line
hist(uniform_data,
     breaks = 10,
     main = "Uniform Distribution (min=10, max=30)",
     xlab = "Value",
     col = "lightgreen",
     border = "black",
     probability = TRUE)
abline(h = 1/(30-10), col = "red", lwd = 2, lty = 2)

# Binomial histogram with theoretical probabilities
hist(binomial_data,
     breaks = seq(-0.5, 10.5, by = 1),  # integer bins
     main = "Binomial Distribution (size=10, prob=0.5)",
     xlab = "Number of Successes",
     col = "lightpink",
     border = "black",
     probability = TRUE,
     right = FALSE)
x <- 0:10
lines(x, dbinom(x, size = 10, prob = 0.5),
      type = "h", col = "red", lwd = 2)
points(x, dbinom(x, size = 10, prob = 0.5),
       col = "red", pch = 16)

# ------------------------------------------------------------
# Step 3. Compare mtcars$mpg to a normal distribution
# ------------------------------------------------------------


# load dataset
data(mtcars)

# Histogram of mpg with fitted normal curve
hist(mtcars$mpg,
     breaks = 10,
     main = "MPG Distribution vs Normal",
     xlab = "Miles per Gallon",
     col = "lightyellow",
     border = "black",
     probability = TRUE)

# Fit normal distribution using sample mean and sd
mpg_mean <- mean(mtcars$mpg)
mpg_sd   <- sd(mtcars$mpg)
curve(dnorm(x, mean = mpg_mean, sd = mpg_sd),
      col = "blue", lwd = 2, add = TRUE)
# Add Mean and Median Vertical Lines
abline(v = mpg_mean, col = "red", lwd = 2, lty = 1)
abline(v = median(mtcars$mpg), col = "black", lwd = 2, lty = 3)

# Reset plotting parameters
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# Step 4. Summary statistics for reflection
# ------------------------------------------------------------

# Summary statistics for simulated data
cat("Summary of normal_data:\n"); summary(normal_data); sd(normal_data)
cat("\nSummary of uniform_data:\n"); summary(uniform_data); sd(uniform_data)
cat("\nSummary of binomial_data:\n"); summary(binomial_data); sd(binomial_data)

# MPG statistics
cat("\nMPG summary:\n"); summary(mtcars$mpg)
cat("\nMPG sd:", mpg_sd, "\n")

# Shapiro-Wilk test for normality of mpg
shapiro.test(mtcars$mpg)

# QQ plot for mpg
qqnorm(mtcars$mpg, main = "QQ Plot of MPG")
qqline(mtcars$mpg, col = "red")

