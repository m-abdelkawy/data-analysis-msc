install.packages("tidyverse")
library(tidyverse)

# vector of strings
familynames <- c("Nick", "Kim", "Olivia", "Henry")

# vector of numeric values
familyages <- c(42, 42, 12, 8)


family <- data.frame(name=familynames, age=familyages)
print(family)

# we can use the names() function to view variable names in a data frame
names(family)

# entities or observations are rows
# variables are columns
# this is the dataframe/dataset being in wide format
# long format is when variables are rows and observations are columns


# refer to the age variable using the $ sign
family$age

# create a new variable equal to the age squared
family$agesquared <- family$age ^ 2

print(family)

###############################################################################
# create a new variable using the mutate method
library(dplyr)
mutate(family, agedoubled = family$age * 2)

# we can sort a variable by a certain variable using the sort method
sort(family$age, decreasing = FALSE)
sort(family$age, decreasing = TRUE)

# we can order the whole dataframe by a variable using order() - base R
# which returns a dataframe in case we need to keep the sorted version in another variable
family[order(family$age, decreasing = TRUE), ]


# create a categorical varible named adult that indicates if someone is an adult
# adult will have the value of 1 if the person is adult, otherwise 0
family$adult <- c(rep(1, 2), rep(0, 2))

# make sure R knows we have a factor variable
family$adultfactor <- factor(family$adult)
mean(family$adultfactor)

# obtain a subset of the data frame 
# lets get a subset of only those older than 10
# keep the variables of name and age
olderthan10 <- subset(family, age>10, select = c("name", "age"))
print(olderthan10)

# missing values arise frequently in applied research
# if entering values by hand, be sure to enter missing values with NA
examplevar <- c(12, 32, 53, NA, 52, 72)

mean(examplevar)
# this gives NA because of the missing value
# we can provide param to the mean() method to remove the NA value
mean(examplevar, na.rm = TRUE)

######################### 
# list() and cbind(), and rbind()

# stack() and unstack() for restructuring columns, long format and the opposite