# Data set chosen: Palmer Archipelago (Antarctica) penguin data  
# url: https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data

library(ggplot2)
library(dplyr)

##################################################################
## 1. load the penguins dataset
##################################################################
file_path <- file.path(
  "../datasets/penguins_size.csv"
)
 
df <- read.csv(file_path)

##################################################################
## 2. explore the dataset
##################################################################

# dataset dimensions
cat("Rows: ", nrow(df), "\n")
cat("Columns: ", ncol(df), "\n")

# variable names
names(df)

head(df)
str(df)

# identify unique categories
unique(df$species)
table(df$species)

unique(df$island)
table(df$island)

unique(df$sex)
table(df$sex, useNA='ifany')

# check missing values

##################################################################
## 3. explore missing values
##################################################################
# 3.1 Explore missing values
#is.na(df) returns TRUIE/FALSE matrix for missing values
missing_count <- colSums(is.na(df))
missing_mean <- colMeans(is.na(df))

# missing values
missing_values <- data.frame(
  Count = missing_count,
  Percent = sprintf("%.2f%%", missing_mean * 100)
)

print(missing_values)

# View observations with missing values
# 1 = operate across rows
# 2 = operate down columns
# in R row numbering starts at 1
# apply returns a logical vector
missing_rows <- df[apply(X=is.na(df), MARGIN=1, FUN=any), ]

print(missing_rows)

#######
# 3.2 Explore anomalies/outliers

# Identify numerical variables
numeric_cols <- sapply(df, is.numeric)

# calculate Q1 and Q3 for the numeric columns
# pass function to FUN, that calculates the quantile for the numeric columns
q1_function <- function(x) {
  quantile(x, probs=0.25, na.rm=TRUE)
}

Q1 <- sapply(X=df[numeric_cols], FUN=q1_function)
Q3 <- sapply(X=df[numeric_cols], FUN=function(x) quantile(x, probs = 0.75, na.rm = TRUE))

# calculate IQR
IQR <- Q3 - Q1

# Calculate lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

print("Lower bounds:")
print(lower_bound)

print("Upper bounds:")
print(upper_bound)

# Check each potential outlier values
# lapply takes the dataframe, applies the FUN on each col and returns the result as a list
outlier_rows <- lapply(X=df[numeric_cols], FUN=function(x) boxplot.stats(x)$out)
print(outlier_rows)

# sapply, applies a funtion to data and returns a vector or a a matrix
print(paste(
  "Number of potential outlier observations: ", sum(sapply(X=outlier_rows, FUN=length))
))

##################################################################
## 4. Calculate descriptive statistics
##################################################################

## 4.1 Overall descriptive statistics
numeric_cols = sapply(X=df, FUN=is.numeric)

# stats functions do not remove NA by defauly, so need na.rm=TRUE explicitly
descriptive_stats <- data.frame(
  mean = sapply(X=df[numeric_cols], FUN=mean, na.rm=TRUE),
  median = sapply(X=df[numeric_cols], FUN=median, na.rm=TRUE),
  var = sapply(X=df[numeric_cols], FUN=var, na.rm=TRUE), # sample var with ddof = 1
  std = sapply(X=df[numeric_cols], FUN=sd, na.rm=TRUE),
  min = sapply(X=df[numeric_cols], FUN=min, na.rm=TRUE),
  max = sapply(X=df[numeric_cols], FUN=max, na.rm=TRUE)
)

print(round(descriptive_stats, 2))

## 4.2 Descriptive stats by categorical variable
## select species
# display species
unique(df$species)

# 4.2.1 grouped mean
grouped_mean <- aggregate(
  x=df[numeric_cols],
  by=list(species = df$species),
  FUN=mean,
  na.rm=TRUE
)

grouped_mean[-1] <- round(grouped_mean[-1], 2)
print(grouped_mean)

# 4.2.2 grouped median
grouped_median <- aggregate(
  x = df[numeric_cols],
  by = list(species = df$species),
  FUN = median,
  na.rm = TRUE
)

grouped_median[-1] <- round(grouped_median[-1], 2)
print(grouped_median)

# 4.2.3 grouped variance
grouped_variance <- aggregate(
  x = df[numeric_cols],
  by = list(species = df$species),
  FUN = var,
  na.rm = TRUE
)

grouped_variance[-1] <- round(grouped_variance[-1], 2)
print(grouped_variance)


# 4.2.4 grouped standard deviation
grouped_sd <- aggregate(
  x = df[numeric_cols],
  by = list(species = df$species),
  FUN = sd,
  na.rm = TRUE
)

grouped_sd[-1] <- round(grouped_sd[-1], 2)
print(grouped_sd)


# 4.2.5 grouped min
grouped_min <- aggregate(
  x = df[numeric_cols],
  by = list(species = df$species),
  FUN = min,
  na.rm = TRUE
)

grouped_min[-1] <- round(grouped_min[-1], 2)
print(grouped_min)

# 4.2.6 grouped max
grouped_max <- aggregate(
  x = df[numeric_cols],
  by = list(species = df$species),
  FUN = max,
  na.rm = TRUE
)

grouped_max[-1] <- round(grouped_max[-1], 2)
print(grouped_max)

#################### group stats by species
# 
grouped_stats_unused <- data.frame(
  species = grouped_mean$species,
  mean = grouped_mean[-1],
  median = grouped_median[-1],
  variance = grouped_variance[-1],
  sd = grouped_sd[-1],
  min = grouped_min[-1],
  max = grouped_max[-1]
)

# explore neat output
# print(grouped_mean)
# print(t(grouped_mean[-1]))
# print(as.vector(t(grouped_mean[-1])))
# print(rep(grouped_mean$species, each=length(numeric_cols)))
# print(numeric_cols)
# print(rep(grouped_mean$species, each=sum(numeric_cols)))
# names(df[numeric_cols])

grouped_stats <- data.frame(
  species = rep(grouped_mean$species, each=sum(numeric_cols)),
  variable = rep(names(df[numeric_cols]), times=length((grouped_mean$species))),
  mean = as.vector(t(grouped_mean[-1])),
  median = as.vector(t(grouped_median[-1])),
  variance = as.vector(t(grouped_variance[-1])),
  sd = as.vector(t(grouped_sd[-1])),
  min = as.vector(t(grouped_min[-1])),
  max = as.vector(t(grouped_max[-1]))
)

print(grouped_stats)


##################################################################
## 5. Visualizations
##################################################################
## 5.1 histogram of body mass
##################################################################
mean_body_mass <- mean(df$body_mass_g, na.rm=TRUE)
median_body_mass <- median(df$body_mass_g, na.rm=TRUE)

body_mass_hist <- ggplot(data=df, aes(x=body_mass_g))
body_mass_hist +
  geom_histogram(
    bins=20,
    na.rm=TRUE,
    fill="lightblue"
  ) +
  scale_x_continuous(breaks = seq(3000, 7000, by=500)) +
  geom_vline(
    aes(
      xintercept=mean_body_mass,
      color="Mean"
    ),
    linetype = "solid"
  ) +
  geom_vline(
    aes(
      xintercept=median_body_mass,
      color="Median"
    ),
    linetype = "dashed"
  )+
  scale_color_manual(
    values=c(
      "Mean" = "red",
      "Median" = "blue"
    )
  )+
  labs(x="Body mass (g)", y="Frequency", title="Distribution of Penguin body mass", color="statistic")

##################################################################
## 5.2 Boxplot of body mass by species:
##################################################################
# calculate summary data
summary_data <- df %>%
  group_by(species) %>%
  summarize(
    Q1 = quantile(body_mass_g, 0.25, na.rm=TRUE),
    Median = median(body_mass_g, na.rm=TRUE),
    Q3 = quantile(body_mass_g, 0.75, na.rm=TRUE),
    IQR = IQR(body_mass_g, na.rm=TRUE),
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    Lower_Whisker = min(body_mass_g[body_mass_g >= lower_bound], na.rm=TRUE),
    Upper_Whisker = max(body_mass_g[body_mass_g <= upper_bound], na.rm=TRUE)
  )

# this is after seeing outliers in the Chinstrap species afte I drew the boxplot below
outliers <- df %>%
  left_join(
    summary_data %>% select(species, lower_bound, upper_bound),
    by = "species"
  ) %>%
  filter(body_mass_g < lower_bound | body_mass_g > upper_bound)


# create the boxplot
body_mass_boxplot <- ggplot(
  data=df, 
  aes(x=species, y=body_mass_g, fill=species)
)

body_mass_boxplot + 
  geom_boxplot() +
  geom_text(
    data=summary_data,
    aes(x=species, y=Q1, label=paste0("Q1=", round(Q1, 1))),
    vjust=1.5,
    size=3
  )+
  geom_text(
    data=summary_data,
    aes(x=species, y=Median, label=paste0("Median=", round(Median, 1))),
    vjust=1.5,
    size=3
  )+
  geom_text(
    data=summary_data,
    aes(x=species, y=Q3, label=paste0("Q3=", round(Q3, 1))),
    vjust=1.5,
    size=3
  )+
  geom_text(
    data=summary_data,
    aes(x=species, y=Lower_Whisker, label=paste0("lower_whisker=", round(Lower_Whisker, 1))),
    vjust=-0.5,
    size=3
  )+
  geom_text(
    data=summary_data,
    aes(x=species, y=Upper_Whisker, label=paste0("upper_whisker=", round(Upper_Whisker, 1))),
    vjust=-0.5,
    size=3
  )+
  geom_text( # for the outlier values
    data = outliers,
    aes(x = species, y = body_mass_g, label = round(body_mass_g)),
    color = "red",
    hjust = -0.5
  )+
  labs(x="Species", y="Body mass (g)")
##################################################################
## 5.3 Scatterplot of flipper length vs body mass:
##################################################################
flipper_mass_scatterplot <- ggplot(
  data = df,
  aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = species
  )
)

flipper_mass_scatterplot +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(group = 1)
  ) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    title = "Flipper Length vs. Body Mass",
    color = "Species"
  ) +
  theme_minimal()
