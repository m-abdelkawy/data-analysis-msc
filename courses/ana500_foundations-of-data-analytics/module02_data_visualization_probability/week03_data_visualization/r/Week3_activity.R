# ================================
# ggplot2 visualization
# mtcars dataset
# ================================

#imports
library(ggplot2)
library(dplyr)

# load the mtcars dataset
data(mtcars)
mtcars <- mtcars

# view the structure and the head
head(mtcars)
str(mtcars)


# =============================================================================
# 1. create a histogram to visualize the distribution of mpg
# =============================================================================
mpg_hist <- ggplot(data=mtcars, aes(x=mpg))
mpg_hist+
  geom_histogram(
    #binwidth = 0.5,
    bins=9,
    boundary=0.1,
    fill = "blue",
    color="black",
    linewidth = 0.1
  )+
  scale_x_continuous(breaks = seq(0, 40, by=5))+
  #coord_cartesian(xlim=c(0, 40))+
  geom_vline(
    xintercept = mean(mtcars$mpg), 
    linetype = "dashed", 
    color="red"
  )+
  geom_vline(
    xintercept = median(mtcars$mpg),
    linetype = "solid",
    color="black"
  )+
  labs(x="mpg (Mile per Gallon)", y="Frequency", title="Distribution of mpg")+
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "grey"),
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2),
    #panel.grid.minor = element_blank()
  )

# =============================================================================
# 2. Create a Scatterplot - Plot mpg vs hp with a trend line
# =============================================================================
scatter <- ggplot(data=mtcars, aes(x=mpg, y=hp))
scatter + 
  geom_point(size=2, color="blue")+
  geom_smooth(
    method = "lm",
    color="red",
    se=T,
    alpha=0.3,
    fill="blue",
    linetype="dashed",
    linewidth = 0.8
  )+
  labs(x="mpg (Mile per Gallon)", y="hp (horse power)", title="MPG vs Horsepower")+
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "grey"),
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2)
  )

# =============================================================================
# 3. Create a Boxplot - Visualize mpg across cylinder groups (cyl)
# =============================================================================
# calculate summary data
summary_data <- mtcars %>%
  group_by(cyl) %>%
  summarize(
    Q1 = quantile(mpg, 0.25),
    Median = median(mpg),
    Q3 = quantile(mpg, 0.75),
    IQR = IQR(mpg),
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    Lower_Whisker = min(mpg[mpg >= lower_bound]),
    Upper_Whisker = max(mpg[mpg <= upper_bound])
  )


# create the plot and display stats
box <- ggplot(data=mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl)))
box + 
  geom_boxplot()+
  geom_text(
    data=summary_data,
    aes(x=factor(cyl), y=Q1, label=paste0("Q1=", round(Q1, 1))),
    vjust=1.5,
    size=3
  )+
  geom_text(
    data=summary_data,
    aes(x=factor(cyl), y=Median, label=paste0("Median=", round(Median, 1))),
    vjust=1.5,
    size = 3
  )+
  geom_text(
    data=summary_data,
    aes(x=factor(cyl), y=Q3, label=paste0("Q3=", round(Q3, 1))),
    vjust=1.5,
    size=3
  )+
  geom_text(
    data = summary_data,
    aes(x = factor(cyl), y = Lower_Whisker, label = paste0("Lower=", round(Lower_Whisker, 1))),
    vjust = 1.5,
    size=3,
    color="purple"
  )+
  geom_text(
    data=summary_data,
    aes(x=factor(cyl), y=Upper_Whisker, label=paste0("Upper=", round(Upper_Whisker))),
    vjust=-0.5,
    size=3,
    color="purple"
  )+
  labs(x="cyl (cylinders)", y="mpg (mile per gallon)")+
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "grey"),
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2)
  )

