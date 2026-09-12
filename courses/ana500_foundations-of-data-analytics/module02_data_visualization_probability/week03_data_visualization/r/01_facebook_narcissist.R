## =============================================================================
## 01_facebook_narcissism.R
##
## Purpose    : Explore the relationship between narcissism (NPQC_R_Total)
##              and profile ratings on Facebook (Buffy? - Narcissism dataset,
##              Field 2013, "FacebookNarcissism.dat").
## Author     : Mohammed Abdelkawy
## Date       : 2026-09-12
##
## Input      : ../datasets/FacebookNarcissism.dat (tab-delimited)
## Variables  : NPQC_R_Total (narcissism score),
##              Rating (profile rating),
##              Rating_Type (type of rating given)
## Output     : three exploratory scatterplots (Rating ~ NPQC_R_Total):
##              1. plain points
##              2. points jittered & coloured by Rating_Type
##              3. points jittered & shaped by Rating_Type
##
## Dependencies: ggplot2
##
## Notes      : position = "jitter" spreads overlapping points so density
##              is visible; jittered plots show distribution, not exact values.
## =============================================================================

# Load packages
library(ggplot2)

# Load Data

filepath <- file.path(
  "../datasets/FacebookNarcissism.dat"
)


facebook_data <- read.delim(filepath, header = TRUE)

# inspect Data
# print(head(facebook_data, 20))

# plot the NPQC_R_Total , Rating
graph <- ggplot(facebook_data, aes(x = NPQC_R_Total, y = Rating))

graph + geom_point(shape = 16, size = 6)

graph + geom_point(aes(colour = Rating_Type), position = "jitter")

graph + geom_point(aes(shape = Rating_Type), position = "jitter")
############################################


