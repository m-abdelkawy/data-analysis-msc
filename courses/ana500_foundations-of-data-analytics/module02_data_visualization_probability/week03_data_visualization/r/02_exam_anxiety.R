## =============================================================================
## 01_exam_anxiety.R
##
## Purpose    : Explore the relationship between exam anxiety and exam
##              performance in the exam anxiety dataset (Field, 2013).
## Author     : Mohammed Abdelkawy
## Date       : 2026-09-12
##
## Input      : ../datasets/Exam Anxiety.dat (tab-separated, header row)
## Output     : scatterplot of Exam ~ Anxiety with linear regression line
##              and 95% CI ribbon (printed to screen / saved as PNG)
##
## Dependencies: ggplot2
##
## Notes      : The ribbon from geom_smooth(se = TRUE) is a confidence
##              interval for the regression line (mean prediction),
##              NOT a prediction interval for individual students.
## =============================================================================

library(ggplot2)

file_path <- file.path(
  "../datasets/Exam Anxiety.dat"
)

examData <- read.table(file_path, header = TRUE, sep = "\t")

head(examData)

scatterplot <- ggplot(data = examData, aes(x = Anxiety, y = Exam))

scatterplot + 
  geom_point() + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") +
  theme(
    panel.grid.major = element_line(linewidth = 0.4, colour = "grey", linetype = "dashed"),
    panel.grid.minor = element_line(linewidth = 0.2, color = "grey", linetype = "dashed")
  ) +
  geom_smooth(method = "lm", colour = "red", se = T, alpha = 0.3, fill = "Blue") # add a regression line

# scatter plot grouped by gender
scatterByGender <- ggplot(data=examData, aes(x=Anxiety, y=Exam, colour = Gender))
scatterByGender +
  geom_point() +
  geom_smooth(method = "lm", aes(fill=Gender), alpha=0.2) +
  labs(x="Exam Anxiety", y="Exam Performance %", colour="Gender")
