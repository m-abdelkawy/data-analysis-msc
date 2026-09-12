# %% Load packages
library(ggplot2)

# %% Load Data
module_dir <- file.path(
  "courses/ana500_foundations-of-data-analytics/",
  "module02_data_visualization_probability/",
  "week03_data_visualization/"
)

filepath <- file.path(
  module_dir, "datasets/FacebookNarcissism.dat"
)


facebook_data <- read.delim(filepath, header = TRUE)

# %% inspect Data
# print(head(facebook_data, 20))

# %% plot the NPQC_R_Total , Rating
graph <- ggplot(facebook_data, aes(x = NPQC_R_Total, y = Rating))

print(graph + geom_point(shape = 16, size = 6))

print(graph + geom_point(aes(colour = Rating_Type), position = "jitter"))

print(graph + geom_point(aes(shape = Rating_Type), position = "jitter"))