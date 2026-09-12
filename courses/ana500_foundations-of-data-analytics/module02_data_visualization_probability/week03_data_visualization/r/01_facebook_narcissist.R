# %% Load packages
library(ggplot2)

# %% Load Data

filepath <- file.path(
  "../datasets/FacebookNarcissism.dat"
)


facebook_data <- read.delim(filepath, header = TRUE)

# %% inspect Data
# print(head(facebook_data, 20))

# %% plot the NPQC_R_Total , Rating
graph <- ggplot(facebook_data, aes(x = NPQC_R_Total, y = Rating))

graph + geom_point(shape = 16, size = 6)

graph + geom_point(aes(colour = Rating_Type), position = "jitter")

graph + geom_point(aes(shape = Rating_Type), position = "jitter")
############################################333


