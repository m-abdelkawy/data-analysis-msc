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

festivalHistogram <- ggplot(data=festivalData, aes(x=day1))

festivalHistogram + 
  geom_histogram(
    aes(y=after_stat(density)),
    bins=25,
    fill="skyblue",
    color="black"
  ) +
  stat_function(
    fun=dnorm,
    args=list(mean=m, sd=s)
  )
