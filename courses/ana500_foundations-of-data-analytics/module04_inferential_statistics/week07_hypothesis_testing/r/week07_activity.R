
# ============================================================
# Lesson 7: Inferential Statistics
# Independent t-test, paired t-test, effect sizes, and boxplots
# ============================================================

# Install these packages once if needed:
required_packages <- c("readxl", "ggplot2")

missing_packages <- required_packages[!sapply(X=required_packages, FUN=requireNamespace, quietly=TRUE)]

if(length(missing_packages) > 0){
  install.packages(missing_packages, dependencies = TRUE)
}

library(readxl)
library(ggplot2)

# 1. Load data -----------------------------------------------

file_path <- "../datasets/earnings and screen time.xlsx"
df <- read_excel(file_path)

# Set group order for consistent results and plots
df$kids <- factor(
  df$kids,
  levels = c(0, 1),
  labels = c("Without kids", "With kids")
)

alpha <- 0.05

# 2. Independent t-test: earnings ----------------------------

# Descriptive statistics
earnings_summary <- aggregate(
  earnings ~ kids,
  data = df,
  FUN = function(x) c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
)

print(earnings_summary)

# Without kids minus with kids
earnings_test <- t.test(
  df$earnings[df$kids == "Without kids"],
  df$earnings[df$kids == "With kids"],
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

print(earnings_test)

t_earnings <- unname(earnings_test$statistic)
df_earnings <- unname(earnings_test$parameter)
p_earnings <- earnings_test$p.value
mean_difference <- earnings_test$estimate[1] - earnings_test$estimate[2]

r_earnings <- sqrt(
  t_earnings^2 / (t_earnings^2 + df_earnings)
)

cat("\nEarnings results\n")
cat("Mean difference (without kids - with kids):",
    mean_difference, "thousand dollars\n")
cat("t =", t_earnings, "\n")
cat("df =", df_earnings, "\n")
cat("p-value =", p_earnings, "\n")
cat("Effect size r =", r_earnings, "\n")

if (p_earnings < alpha) {
  cat("Conclusion: Reject the null hypothesis.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis.\n")
}

# 3. Paired t-test: sleep ------------------------------------

# Keep participants with both sleep measurements
paired_data <- df[
  complete.cases(df[, c("sleep_screen", "sleep_noscreen")]),
]

# Screen minus no screen
sleep_difference <- paired_data$sleep_screen -
  paired_data$sleep_noscreen

sleep_test <- t.test(
  paired_data$sleep_screen,
  paired_data$sleep_noscreen,
  paired = TRUE,
  alternative = "two.sided",
  conf.level = 0.95
)

print(sleep_test)

t_sleep <- unname(sleep_test$statistic)
df_sleep <- unname(sleep_test$parameter)
p_sleep <- sleep_test$p.value
mean_sleep_difference <- mean(sleep_difference)

r_sleep <- sqrt(
  t_sleep^2 / (t_sleep^2 + df_sleep)
)

cat("\nSleep results\n")
cat("Number of pairs:", length(sleep_difference), "\n")
cat("Mean difference (screen - no screen):",
    mean_sleep_difference, "minutes\n")
cat("t =", t_sleep, "\n")
cat("df =", df_sleep, "\n")
cat("p-value =", p_sleep, "\n")
cat("Effect size r =", r_sleep, "\n")

if (p_sleep < alpha) {
  cat("Conclusion: Reject the null hypothesis.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis.\n")
}

# 4. Boxplot: earnings by kids -------------------------------

earnings_plot <- ggplot(
  df,
  aes(x = kids, y = earnings, fill = kids)
) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Annual Earnings by Kids Status",
    x = "Kids status",
    y = "Annual earnings (thousands of dollars)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(earnings_plot)

# 5. Boxplot: sleep condition --------------------------------

sleep_long <- data.frame(
  condition = factor(
    rep(c("Screen", "No screen"), each = nrow(paired_data)),
    levels = c("Screen", "No screen")
  ),
  sleep = c(
    paired_data$sleep_screen,
    paired_data$sleep_noscreen
  )
)

sleep_plot <- ggplot(
  sleep_long,
  aes(x = condition, y = sleep, fill = condition)
) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Sleep Duration: Screen vs. No-Screen Condition",
    x = "Condition",
    y = "Sleep duration (minutes)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(sleep_plot)

# 6. Final summary -------------------------------------------

cat("\n========== FINAL SUMMARY ==========\n")
cat("Earnings: mean difference =", round(mean_difference, 2),
    ", t =", round(t_earnings, 3),
    ", df =", round(df_earnings, 3),
    ", p =", signif(p_earnings, 4),
    ", r =", round(r_earnings, 3), "\n")

cat("Sleep: mean difference =", round(mean_sleep_difference, 2),
    "minutes, t =", round(t_sleep, 3),
    ", df =", df_sleep,
    ", p =", signif(p_sleep, 4),
    ", r =", round(r_sleep, 3), "\n")

cat("Significance level alpha =", alpha, "\n")

