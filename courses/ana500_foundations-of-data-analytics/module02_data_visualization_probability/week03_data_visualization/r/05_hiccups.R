library(ggplot2)

file_path <- file.path(
  "../datasets/Hiccups.dat"
)

hiccupsData <- read.delim(file=file_path, header=TRUE)

head(hiccupsData)
# stack functions creates a column named ind, that is a factor in the resulting dataframe
hiccups <- stack(hiccupsData)
# head(hiccupsData)


names(hiccups) <- c("Hiccups", "Intervention")

str(hiccups)

# no need for this since intervention is a factor already after using the stack funcion
hiccups$Intervention_Factor <- factor(hiccups$Intervention, levels=unique(hiccups$Intervention))
  
str(hiccups)

line <- ggplot(hiccups, aes(x=Intervention_Factor, y=Hiccups))
line +
  stat_summary(
    fun=mean,
    geom="point"
  )+
  stat_summary(
    fun=mean,
    geom="line",
    aes(group=1), #since interventoin factor is discrete, it means the summary points are in different groups. 
    #line geom requires points to be in one group
    color="blue",
    linetype="dashed"
  )+ #add error bar
  stat_summary(
    fun.data=mean_cl_boot, # fun.data because the function returns more than one value (lower and upper bounds of the CI)
    geom="errorbar",
    width=0.20
  )+
  labs(x="Intervention", y="Mean number of hiccups")
