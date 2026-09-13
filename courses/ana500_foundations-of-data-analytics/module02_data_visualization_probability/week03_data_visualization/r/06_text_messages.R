library(ggplot2)

file_path <- file.path(
  "../datasets/TextMessages.dat"
)

data <- read.delim(file=file_path, header=TRUE)

str(data)

textMessages <- data.frame(
  Group=rep(data$Group, times=2),
  stack(data[c("Baseline", "Six_months")])
)

str(textMessages)
names(textMessages) <- c("Group", "Grammar_Score", "Time")
str(textMessages)

# no need for this since Time is a factor already after using the stack funcion, used just to change the label
textMessages$Time <- factor(textMessages$Time, levels=unique(textMessages$Time), labels=c("Baseline", "6 Months"))
str(textMessages)


line <- ggplot(data=textMessages, aes(x=Time, y=Grammar_Score, colour=Group))
line+
  stat_summary(
    fun=mean,
    geom="point",
    shape=15,
    size=2.5
  )+
  stat_summary(
    fun=mean,
    geom="line",
    aes(group=Group),
    linetype="dashed"
  )+
  stat_summary(
    fun.data=mean_cl_boot,
    geom="errorbar",
    width=0.20
  )+labs(x="Time", y="Mean Grammar Score", colour="Group")
