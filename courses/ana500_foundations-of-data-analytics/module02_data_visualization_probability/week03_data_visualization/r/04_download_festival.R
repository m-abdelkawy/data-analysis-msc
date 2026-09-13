## =============================================================================
## 04_download_festival.R
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