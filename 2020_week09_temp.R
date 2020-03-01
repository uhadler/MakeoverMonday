week9mm <- readr::read_csv("data/2020 week 9/data/week9mm.csv")

library(ggplot2)
library(extrafont)
library(scales)
loadfonts()

week9mm$grade <- forcats::fct_inorder(as.factor(week9mm$grade))
size = 8
ggplot(week9mm) + theme_minimal() +
  geom_segment(aes(x=grade, xend = grade,
                   y=hours_averaged, yend = hours_needed),
               colour = muted("darkred", l = 30), size = size-4) + 
  #point outlines
  geom_point(aes(x=grade, y=hours_needed), size = size+2,
             colour = muted("lightgreen")) +
  geom_point(aes(x=grade, y=hours_averaged), size = size+2,
             colour = "black") +
  #inner
  geom_point(aes(x=grade, y=hours_needed), size = size,
             colour = "lightgrey") + 
  geom_point(aes(x=grade, y=hours_averaged), size = size,
             colour = "lightgray") + 
  theme(plot.background = element_rect(fill = "#3f3f3f", colour = NA),
        panel.background = element_rect(fill="#3f3f3f", colour = NA),
        panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.minor = element_line(colour="gray", linetype="dotted"),
        text = element_text(colour = "grey", family = "Calibri"),
        axis.text.y = element_text(colour="grey"),
        axis.text.x = element_text(colour="grey", angle=45, hjust=1))
