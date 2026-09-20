# Data set chosen: Palmer Archipelago (Antarctica) penguin data  
# url: https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data

library(ggplot2)

#1. load the penguins dataset
file_path <- file.path(
  "../datasets/penguins_size.csv"
)
 
df <- read.csv(file_path)

# explore the dataset
# dataset dimensions
cat("Rows: ", nrow(df), "\n")
cat("Columns: ", ncol(df), "\n")

# variable names
names(df)

head(df)
str(df)
