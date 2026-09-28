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
## Notes      : test normality of distribution visually
## =============================================================================

###############################################
## packages:
###############################################
required_packages <- c("ggplot2", "psych", "pastecs", "e1071")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load packages
library(ggplot2)
library(psych)
library(pastecs)
library(e1071)

###############################################
## Method definitions:
###############################################

plot_hist_normal <- function(data, xvar, title="Ditstribution plot", bins=25){
  # Extract the vector for summary calculations
  vals <- data[[xvar]]
  
  # Calculate summary stats (ignoring NAs)
  m <- mean(vals, na.rm = TRUE)
  med <- median(vals, na.rm = TRUE)
  std <- sd(vals, na.rm = TRUE)
  n <- sum(!is.na(vals))
  
  # Calculate skewness and excess kurtosis
  skew <- e1071::skewness(
    vals,
    na.rm = TRUE,
    type = 2
  )
  
  excess_kurt <- e1071::kurtosis(
    vals,
    na.rm = TRUE,
    type = 2
  )
  
  # Text to display on plot
  shape_text <- sprintf(
    "N = %d\nSkewness = %.2f\nExcess Kurtosis = %.2f",
    n,
    skew,
    excess_kurt
  )
  
  festivalHistogram <- ggplot(data=festivalData, aes(x=.data[[xvar]]))
  
  festivalHistogram + 
    # 1. observed data (bars)
    geom_histogram(
      aes(y=after_stat(density)),
      bins=25,
      fill="skyblue",
      color="white",
      alpha=0.50
    ) +
    # 2. Observed Density (Smooth Line): Solid dark blue
    geom_density(
      aes(color="observed_density"),
      linewidth = 1.2,
      linetype = "solid",
      key_glyph = "path"  # Draws a line in the legend instead of a square box
    ) +
    # 3. Theoretical Normal Curve (Line): Dashed red
    stat_function(
      fun=dnorm,
      args=list(mean=m, sd=std),
      aes(color="theoretical_normal"),
      linetype="dashed",
      linewidth = 1
    ) +
    
    scale_color_manual(
      name="Distribution",
      values=c(
        "observed_density"="darkblue",
        "theoretical_normal"="red"
      )
    ) +
    # 4. Skewness and kurtosis annotation
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = shape_text,
      hjust = 1.1,
      vjust = 1.5
    ) +
    labs(
      title=title,
      x=xvar,
      y="density",
      caption = sprintf("Mean = %.2f, Median = %.2f, SD = %.2f, N = %d", m, med, std, n)
    )

}

plot_qq <- function(data, xvar, title=NULL) {
  # default title
  if(is.null(title)) title <- paste("Q-Q Plot:", xvar)
  
  # qq plot
  ggplot(data=data, aes(sample=.data[[xvar]]))+
    # points representing sample quantiles
    stat_qq(alpha=0.6, color="black")+
    # reference line through 1st and 3rd quartiles
    stat_qq_line(color="steelblue", linewidth=1)+
    labs(
      title=title,
      x="Theoretical quantiles",
      y="Sample quantiles"
    )
}

###############################################
## Main:
###############################################

file_path <- file.path(
  "../datasets/DownloadFestival.dat"
)

festivalData <- read.delim(file=file_path, header=TRUE)

head(festivalData)

days <- c("day1", "day2", "day3")

for (day in days) {
  p <- plot_hist_normal(
    data = festivalData,
    xvar = day,
    title = paste("Hygiene score on", day)
  )
  print(p)  # Required inside R loops to render ggplot
}

# plot_hist_normal(data = festivalData, xvar = "day1", title = "Hygiene score on day 1")
# plot_hist_normal(data = festivalData, xvar = "day2", title = "Hygiene score on day 2")
# plot_hist_normal(data = festivalData, xvar = "day3", title = "Hygiene score on day 3")

for (day in days) {
  qq <- plot_qq(
    data = festivalData,
    xvar = day,
    title = paste("Hygiene score on", day)
  )
  print(qq)  # Required inside R loops to render ggplot
}

#############

# display the numerical values of skewness and kurtosis:
describe(festivalData$day1)
stat.desc(festivalData$day1, basic = FALSE, norm = TRUE)

describe(cbind(festivalData$day1, festivalData$day2, festivalData$day3))
stat.desc(
  cbind(festivalData$day1, festivalData$day2, festivalData$day3),
  basic = FALSE,
  norm = TRUE
)

# Display descriptive statistics, including skewness and kurtosis
# Type 1 — the uncorrected/basic moment-based estimates.
# Type 2 — bias-corrected estimates; this is the convention that aligns closely with e1071::skewness(..., type = 2) / kurtosis(..., type = 2).
# Type 3 (the default, used by the book) — another bias-corrected convention, with a different finite-sample correction.
psych::describe(festivalData[, c("day1", "day2", "day3")], type=3)

# Display normality-related statistics
pastecs::stat.desc(
  festivalData[, c("day1", "day2", "day3")],
  basic = FALSE,
  norm = TRUE
)


###############################################
## Shapiro-Wilk normality tests:
###############################################

for (day in days) {
  result <- shapiro.test(festivalData[[day]])
  
  cat("\nShapiro-Wilk test for", day, "\n")
  print(result)
}

