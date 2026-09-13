## =============================================================================
## 03_download_festival.R
##
## Purpose    : Use frequency distributions to explore data related to 
##              the hiegene of the download festival over 3 days.
## Author     : Mohammed Abdelkawy
## Date       : 2026-09-12
##
## Input      : ../datasets/DownloadFestival.dat (tab-separated, header row)
## Output     : Frequency distribution
##
## Dependencies: ggplot2
##
## Notes      : display examples of histogram and box plots
## =============================================================================

library(ggplot2)

file_path <- file.path(
  "../datasets/DownloadFestival.dat"
)

festivalData <- read.delim(file=file_path, header=TRUE)

head(festivalData)

# plot hygiene scores for day1
festivalHistogram <- ggplot(data=festivalData, aes(x=day1)) + 
  theme(legend.position = "none")+
  labs(x="Hygiene (day 1 of the festival)", y="Frequency")

festivalHistogram + geom_histogram(binwidth = 0.2, boundary=0, closed="left", colour = "white", linewidth = 0.1)


festivalHistogram <- ggplot(data=festivalData, aes(x=day1)) + 
  theme(legend.position = "none")+
  labs(x="Hygiene (day 1 of the festival)", y="Frequency")+
  scale_x_continuous(breaks=seq(0, 5, by = 0.25))+
  coord_cartesian(xlim=c(0, 5))

festivalHistogram + geom_histogram(binwidth = 0.2, boundary=0, closed="left", colour = "white", linewidth = 0.1)

# ---Boxplot example---
festivalBoxPlot = ggplot(data=festivalData, aes(x=gender, y=day1))
festivalBoxPlot +
  geom_boxplot()+
  stat_summary(
    fun=median,
    geom="text",
    aes(label=round(after_stat(y), 2)),
    vjust=-0.5
  )+
  labs(x="Gender", y="Hygiene (Day 1 of festival)")

## -----Add stats to boxplot-----
# use library dplyr for manipulating and summarizing dataframes
# it is a part of the tidyverse ecosystem
library(dplyr)

# calculate summaries by gender
boxplotStats <- festivalData %>%
  group_by(gender) %>%
  summarise(
    Minimum = min(day1, na.rm = TRUE),
    Q1 = quantile(day1, 0.25, na.rm=TRUE),
    Median = median(day1, na.rm=TRUE),
    Q3 = quantile(day1, 0.75, na.rm=TRUE),
    Maximum = max(day1, na.rm=TRUE)
  )

boxplotStats

festivalBoxPlot = ggplot(data=festivalData, aes(x=gender, y=day1))

festivalBoxPlot + 
  geom_boxplot()+
  geom_text(
    data=boxplotStats,
    aes(
      x=gender, 
      y=Q1,
      label=paste0("Q1 = ", round(Q1, 2))
      ),
    vjust=1.5
  )+
  geom_text(
    data = boxplotStats,
    aes(
      x = gender,
      y = Median,
      label = paste0("Median = ", round(Median, 2))
    ),
    vjust = -0.5                                    # Move text slightly upward
  ) +
  geom_text(
    data = boxplotStats,
    aes(
      x = gender,
      y = Q3,
      label = paste0("Q3 = ", round(Q3, 2))
    ),
    vjust = -0.5
  ) +
  labs(
    x = "Gender",
    y = "Hygiene (Day 1 of festival)"
  )
##------Another way
library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate the five-number summary separately for each gender
boxplotStats <- festivalData %>%
  group_by(gender) %>%
  summarise(
    Minimum = min(day1, na.rm = TRUE),
    Q1 = quantile(day1, 0.25, na.rm = TRUE),
    Median = median(day1, na.rm = TRUE),
    Q3 = quantile(day1, 0.75, na.rm = TRUE),
    Maximum = max(day1, na.rm = TRUE)
  )

# Convert the summary table from wide format to long format
boxplotLabels <- boxplotStats %>%
  pivot_longer(
    cols = -gender,                                 # Reshape every column except gender
    names_to = "Statistic",                         # Store statistic names here
    values_to = "Value"                             # Store numeric values here
  ) %>%
  mutate(
    Label = paste0(                                 # Create the text shown on the plot
      Statistic,
      " = ",
      round(Value, 2)
    )
  )

# Create the boxplot
ggplot(festivalData, aes(x = gender, y = day1)) +
  geom_boxplot() +
  geom_text(
    data = boxplotLabels,
    aes(
      x = gender,
      y = Value,
      label = Label
    ),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    x = "Gender",
    y = "Hygiene (Day 1 of festival)"
  )
###### Density plot
densityPlot <- ggplot(data=festivalData, aes(x=day1))

densityPlot + 
  geom_density(linewidth = 2, colour = "red")
