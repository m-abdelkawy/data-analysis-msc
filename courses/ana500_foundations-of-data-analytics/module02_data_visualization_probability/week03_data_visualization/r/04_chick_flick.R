## =============================================================================
## 04_chick_flick.R
##
## Purpose    : explore bar charts 
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
  "../datasets/ChickFlick.dat"
)

chickFlick <- read.delim(file=file_path, header = TRUE)

genderBar <- ggplot(chickFlick, aes(x=gender))
genderBar + geom_bar()

## plot the mean arousal score (y-axis) for each film (x-axis)
# option 1 using stat_summary
bar = ggplot(data=chickFlick, aes(x=film, y=arousal))

bar+
  stat_summary(
    fun=mean,
    geom="bar",
    fill="grey",
    color="black"
  )+
  labs(
    x="Film",
    y="Mean Arousal",
    title="Mean arousal by film"
  )

# option 2 calculate mean first then use geom_col()
filmMeans <- chickFlick %>%
  group_by(film) %>%
  summarise(
    mean_arousal = mean(arousal)
  )

filmMeans

bar = ggplot(data = filmMeans, aes(x=film, y=mean_arousal))
bar +
  geom_col()+
  labs(
    x="Film",
    y="Mean Arousal",
    title="Mean arousal by film"
  )


# summarize() is from dplyr, it reduces multiple rows into one or more summary rows

# -----Error bar chart-----
# displayes a central estimate together with an estimation of uncertainty
# or variability around the estimate
bar = ggplot(data=chickFlick, aes(x=film, y=arousal))

bar+
  stat_summary(
    fun=mean,
    geom="bar",
    fill="grey",
    color="black"
  )+
  stat_summary(
    fun.data=mean_cl_normal,
    geom="pointrange" # or errorbar
    )+
  labs(
    x="Film",
    y="Mean Arousal",
    title="Mean arousal by film"
  )

# --- 4.9.1.2. bar charts for several independent variables ---
# factor out gender
bar = ggplot(data=chickFlick, aes(x=film, y=arousal, fill=gender))
bar +
  stat_summary(fun=mean, geom="bar", position="dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width=0.2)+
  labs(x="Film", y="Mean Arousal", fill="Gender")+
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "grey"),
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.2)
    #panel.grid.minor = element_blank()
  )+
  scale_fill_manual(name="Gender", values=c("Female"="Blue", "Male"="Green"))

# second method to factor out gender is to use facet
bar <- ggplot(data=chickFlick, aes(x=film, y=arousal, fill=film))
bar+
  stat_summary(
    fun=mean,
    geom="bar",
    #fill="grey",
    color="black"
  )+
  stat_summary(
    fun.data=mean_cl_normal,
    geom="errorbar", # or errorbar
    width=0.3
  )+
  facet_wrap(~gender)+
  labs(
    x="Film",
    y="Mean Arousal",
    title="Mean arousal by film"
  )
