# ANA500 Data Project Part 2
# Palmer Archipelago penguin data: distributions, correlations, and hypothesis tests

# Install required packages if they are not already installed
required_packages <- c("readr", "ggplot2", "dplyr")
missing_packages <- required_packages[
  !sapply(X = required_packages, FUN = requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

library(readr)
library(ggplot2)
library(dplyr)

# 1. Load the penguins dataset
file_path <- "../datasets/penguins_size.csv"

if (!file.exists(file_path)) {
  stop(
    "Dataset not found. Update file_path so it points to datasets/penguins_size.csv."
  )
}

df <- read_csv(file_path, show_col_types = FALSE)

# 2. Explore the dataset
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("\nVariable names:\n")
print(names(df))
cat("\nDataset structure:\n")
str(df)
cat("\nFirst six rows:\n")
print(head(df))

# Reusable function: summarize a numerical variable
summarize_variable <- function(data, variable) {
  values <- data[[variable]]
  values <- values[!is.na(values)]
  
  data.frame(
    variable = variable,
    n = length(values),
    mean = mean(values),
    median = median(values),
    variance = var(values),
    sd = sd(values),
    min = min(values),
    q1 = unname(quantile(values, 0.25)),
    q3 = unname(quantile(values, 0.75)),
    max = max(values),
    row.names = NULL
  )
}

# Reusable function: histogram/density with fitted normal curve
plot_hist_normal <- function(data, variable, bins = 20) {
  values <- data[[variable]]
  values <- values[!is.na(values)]
  variable_label <- gsub("_", " ", variable)
  mu <- mean(values)
  sigma <- sd(values)
  
  ggplot(data.frame(value = values), aes(x = value)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = bins,
      fill = "steelblue",
      color = "white"
    ) +
    geom_density(color = "steelblue", linewidth = 0.8) +
    stat_function(
      fun = dnorm,
      args = list(mean = mu, sd = sigma),
      color = "red",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = paste("Distribution of", variable_label),
      x = variable_label,
      y = "Density"
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = sprintf("Mean = %.2f\nMedian = %.2f\nSD = %.2f",
                      mu, median(values), sigma),
      hjust = 1.05,
      vjust = 1.2,
      size = 3.5
    ) +
    theme_minimal()
}

# Reusable function: normal Q-Q plot
plot_qq <- function(data, variable) {
  ggplot(data, aes(sample = .data[[variable]])) +
    stat_qq(na.rm = TRUE) +
    stat_qq_line(na.rm = TRUE, color = "red") +
    labs(
      title = paste("Q-Q Plot of", gsub("_", " ", variable)),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
}

# Reusable function: boxplot by group
plot_boxplot <- function(data, group_variable, numeric_variable, title = NULL) {
  ggplot(
    data,
    aes(x = .data[[group_variable]], y = .data[[numeric_variable]])
  ) +
    geom_boxplot(na.rm = TRUE, fill = "steelblue") +
    labs(
      title = if (is.null(title)) {
        paste(gsub("_", " ", numeric_variable), "by",
              gsub("_", " ", group_variable))
      } else {
        title
      },
      x = gsub("_", " ", group_variable),
      y = gsub("_", " ", numeric_variable)
    ) +
    theme_minimal()
}

# Reusable function: scatterplot with species groups and linear trend
plot_scatterplot <- function(data, x_variable, y_variable, title = NULL) {
  ggplot(
    data,
    aes(
      x = .data[[x_variable]],
      y = .data[[y_variable]],
      color = species
    )
  ) +
    geom_point(alpha = 0.8) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black"
    ) +
    labs(
      title = if (is.null(title)) {
        paste(gsub("_", " ", x_variable), "vs.", gsub("_", " ", y_variable))
      } else {
        title
      },
      x = gsub("_", " ", x_variable),
      y = gsub("_", " ", y_variable),
      color = "Species"
    ) +
    theme_minimal()
}

# 3. Probability and distribution analysis

## 3.1 Body mass
body_mass_n <- sum(!is.na(df$body_mass_g))
body_mass_missing <- sum(is.na(df$body_mass_g))

cat("\nBODY MASS\n")
cat("Original observations:", nrow(df), "\n")
cat("Missing body mass observations:", body_mass_missing, "\n")
cat("Usable body mass observations:", body_mass_n, "\n")

body_mass_data <- df$body_mass_g[!is.na(df$body_mass_g)]
print(summarize_variable(df, "body_mass_g"))

print(plot_hist_normal(df, "body_mass_g"))
print(plot_qq(df, "body_mass_g"))
print(plot_boxplot(df, "species", "body_mass_g",
                   title = "Body Mass by Species"))

## 3.2 Flipper length
flipper_length_n <- sum(!is.na(df$flipper_length_mm))
flipper_length_missing <- sum(is.na(df$flipper_length_mm))

cat("\nFLIPPER LENGTH\n")
cat("Original observations:", nrow(df), "\n")
cat("Missing flipper length observations:", flipper_length_missing, "\n")
cat("Usable flipper length observations:", flipper_length_n, "\n")

flipper_length_data <- df$flipper_length_mm[!is.na(df$flipper_length_mm)]
print(summarize_variable(df, "flipper_length_mm"))

print(plot_hist_normal(df, "flipper_length_mm"))
print(plot_qq(df, "flipper_length_mm"))
print(plot_boxplot(df, "species", "flipper_length_mm",
                   title = "Flipper Length by Species"))

# 4. Correlation analysis

## 4.1 Flipper length vs. body mass
flipper_mass <- df %>%
  select(flipper_length_mm, body_mass_g, species) %>%
  filter(complete.cases(.))

cat("\nFLIPPER LENGTH VS. BODY MASS\n")
cat("Original observations:", nrow(df), "\n")
cat("Usable observations:", nrow(flipper_mass), "\n")
cat("Removed observations:", nrow(df) - nrow(flipper_mass), "\n")

print(plot_scatterplot(
  flipper_mass,
  "flipper_length_mm",
  "body_mass_g",
  "Flipper Length vs. Body Mass"
))

flipper_pearson <- cor.test(
  flipper_mass$flipper_length_mm,
  flipper_mass$body_mass_g,
  method = "pearson"
)
flipper_spearman <- cor.test(
  flipper_mass$flipper_length_mm,
  flipper_mass$body_mass_g,
  method = "spearman",
  exact = FALSE
)

flipper_correlation_summary <- data.frame(
  Method = c("Pearson", "Spearman"),
  Correlation = c(
    unname(flipper_pearson$estimate),
    unname(flipper_spearman$estimate)
  ),
  p_value = c(flipper_pearson$p.value, flipper_spearman$p.value)
) %>%
  mutate(
    R_squared = c(unname(flipper_pearson$estimate)^2, NA_real_),
    Significant_at_0.05 = p_value < 0.05
  )

print(flipper_correlation_summary)
cat("Pearson R-squared:",
    round(unname(flipper_pearson$estimate)^2, 4), "\n")

## 4.2 Culmen length vs. body mass
culmen_mass <- df %>%
  select(culmen_length_mm, body_mass_g, species) %>%
  filter(complete.cases(.))

cat("\nCULMEN LENGTH VS. BODY MASS\n")
cat("Original observations:", nrow(df), "\n")
cat("Usable observations:", nrow(culmen_mass), "\n")
cat("Removed observations:", nrow(df) - nrow(culmen_mass), "\n")

print(plot_scatterplot(
  culmen_mass,
  "culmen_length_mm",
  "body_mass_g",
  "Culmen Length vs. Body Mass"
))

culmen_pearson <- cor.test(
  culmen_mass$culmen_length_mm,
  culmen_mass$body_mass_g,
  method = "pearson"
)
culmen_spearman <- cor.test(
  culmen_mass$culmen_length_mm,
  culmen_mass$body_mass_g,
  method = "spearman",
  exact = FALSE
)

culmen_correlation_summary <- data.frame(
  Method = c("Pearson", "Spearman"),
  Correlation = c(
    unname(culmen_pearson$estimate),
    unname(culmen_spearman$estimate)
  ),
  p_value = c(culmen_pearson$p.value, culmen_spearman$p.value)
) %>%
  mutate(
    R_squared = c(unname(culmen_pearson$estimate)^2, NA_real_),
    Significant_at_0.05 = p_value < 0.05
  )

print(culmen_correlation_summary)
cat("Pearson R-squared:",
    round(unname(culmen_pearson$estimate)^2, 4), "\n")

# 5. Hypothesis testing
alpha <- 0.05

## 5.1 Male vs. female body mass
sex_mass <- df %>%
  select(sex, body_mass_g) %>%
  filter(!is.na(sex), !is.na(body_mass_g)) %>%
  filter(sex %in% c("MALE", "FEMALE")) %>%
  mutate(sex = factor(sex, levels = c("FEMALE", "MALE")))

cat("\nMALE VS. FEMALE BODY MASS\n")
print(data.frame(
  original_count = nrow(df),
  usable_count = nrow(sex_mass),
  removed_count = nrow(df) - nrow(sex_mass)
))
print(table(sex_mass$sex))

sex_mass_summary <- sex_mass %>%
  group_by(sex) %>%
  summarise(
    count = n(),
    mean = mean(body_mass_g),
    median = median(body_mass_g),
    sd = sd(body_mass_g),
    variance = var(body_mass_g),
    min = min(body_mass_g),
    max = max(body_mass_g),
    .groups = "drop"
  )
print(sex_mass_summary)

print(plot_boxplot(
  sex_mass,
  "sex",
  "body_mass_g",
  "Body Mass by Sex"
))

male_mass <- sex_mass %>%
  filter(sex == "MALE") %>%
  pull(body_mass_g)
female_mass <- sex_mass %>%
  filter(sex == "FEMALE") %>%
  pull(body_mass_g)

# Welch's two-sided independent-samples t-test
sex_mass_test <- t.test(
  male_mass,
  female_mass,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

mean_male <- mean(male_mass)
mean_female <- mean(female_mass)
mean_difference_sex <- mean_male - mean_female

cat("Male sample size:", length(male_mass), "\n")
cat("Female sample size:", length(female_mass), "\n")
cat("Male mean body mass:", round(mean_male, 2), "g\n")
cat("Female mean body mass:", round(mean_female, 2), "g\n")
cat("Mean difference (male - female):",
    round(mean_difference_sex, 2), "g\n")
cat("Welch t-statistic:", round(unname(sex_mass_test$statistic), 4), "\n")
cat("Degrees of freedom:", round(unname(sex_mass_test$parameter), 4), "\n")
cat("p-value:", format.pval(sex_mass_test$p.value, digits = 6), "\n")
cat("95% confidence interval:",
    round(sex_mass_test$conf.int[1], 2), "to",
    round(sex_mass_test$conf.int[2], 2), "g\n")

if (sex_mass_test$p.value < alpha) {
  cat("Decision: Reject the null hypothesis.\n")
} else {
  cat("Decision: Fail to reject the null hypothesis.\n")
}

## 5.2 Adelie vs. Gentoo flipper length
species_flipper <- df %>%
  select(species, flipper_length_mm) %>%
  filter(!is.na(species), !is.na(flipper_length_mm)) %>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  mutate(species = factor(species, levels = c("Adelie", "Gentoo")))

cat("\nADELIE VS. GENTOO FLIPPER LENGTH\n")
print(data.frame(
  original_count = nrow(df),
  usable_count = nrow(species_flipper),
  removed_count = nrow(df) - nrow(species_flipper)
))
print(table(species_flipper$species))

species_flipper_summary <- species_flipper %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean = mean(flipper_length_mm),
    median = median(flipper_length_mm),
    sd = sd(flipper_length_mm),
    variance = var(flipper_length_mm),
    min = min(flipper_length_mm),
    max = max(flipper_length_mm),
    .groups = "drop"
  )
print(species_flipper_summary)

print(plot_boxplot(
  species_flipper,
  "species",
  "flipper_length_mm",
  "Flipper Length by Species: Adelie vs. Gentoo"
))

adelie_flipper <- species_flipper %>%
  filter(species == "Adelie") %>%
  pull(flipper_length_mm)
gentoo_flipper <- species_flipper %>%
  filter(species == "Gentoo") %>%
  pull(flipper_length_mm)

# Welch's two-sided independent-samples t-test
flipper_test <- t.test(
  adelie_flipper,
  gentoo_flipper,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

mean_adelie <- mean(adelie_flipper)
mean_gentoo <- mean(gentoo_flipper)
mean_difference_flipper <- mean_adelie - mean_gentoo

cat("Adelie sample size:", length(adelie_flipper), "\n")
cat("Gentoo sample size:", length(gentoo_flipper), "\n")
cat("Adelie mean flipper length:", round(mean_adelie, 2), "mm\n")
cat("Gentoo mean flipper length:", round(mean_gentoo, 2), "mm\n")
cat("Mean difference (Adelie - Gentoo):",
    round(mean_difference_flipper, 2), "mm\n")
cat("Welch t-statistic:", round(unname(flipper_test$statistic), 4), "\n")
cat("Degrees of freedom:", round(unname(flipper_test$parameter), 4), "\n")
cat("p-value:", format.pval(flipper_test$p.value, digits = 6), "\n")
cat("95% confidence interval:",
    round(flipper_test$conf.int[1], 2), "to",
    round(flipper_test$conf.int[2], 2), "mm\n")

if (flipper_test$p.value < alpha) {
  cat("Decision: Reject the null hypothesis.\n")
} else {
  cat("Decision: Fail to reject the null hypothesis.\n")
}

