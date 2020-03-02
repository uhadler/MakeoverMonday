week9mm <- readr::read_csv("data/2020 week 9/data/week9mm.csv")

library(ggplot2)
library(extrafont)
library(scales)
library(ggtext)
loadfonts()

week9mm$grade <- forcats::fct_inorder(as.factor(week9mm$grade))
size = 9
ggplot(week9mm) + theme_minimal() +
  geom_segment(aes(x=grade, xend = grade,
                   y=hours_averaged, yend = hours_needed),
               colour = "#D55E00", size = size-6) + 
  #point outlines
  geom_point(aes(x=grade, y=hours_needed), size = size+2,
             colour = "#009E73") +
  geom_point(aes(x=grade, y=hours_averaged), size = size+2,
             colour = "#CC79A7") +
  #inner
  geom_point(aes(x=grade, y=hours_needed), size = size,
             colour = "grey") + 
  geom_point(aes(x=grade, y=hours_averaged), size = size,
             colour = "grey") + 
  #text in points
  geom_text(aes(x=grade, y=hours_needed, label = as.character(hours_needed)), fontface="bold") +
  geom_text(aes(x=grade, y=hours_averaged, label=as.character(hours_averaged)), fontface="bold") +
  labs(x = NULL, y="Hours",
       title = "Sleep deficit of american students",
       subtitle = "Hours of sleep <span style = 'color:#009E73';>needed</span>, <span style='color:#CC79A7';>averaged</span> and the resulting <span style='color:#D55E00';>sleep deficit</span>. Only first and third grade students average over 90% of sleep needed, whereas<br> students in ninth grade and above all get less than 85% of sleep needed.",
       caption = "Visualization: Uwe Hadler | #MakeoverMonday 2020 Week 9 | Source: savvysleeper.org/costing-kids-sleep/") +
  theme(plot.background = element_rect(fill = "#3f3f3f", colour = NA),
        panel.background = element_rect(fill="#3f3f3f", colour = NA),
        panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.minor = element_line(colour="gray", linetype="dotted"),
        text = element_text(colour = "grey", family = "Calibri"),
        axis.text.y = element_text(colour="grey"),
        axis.text.x = element_text(colour="grey", angle=45, hjust=1),
        plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_markdown(family = "Calibri", size = 14)) 


ggsave(path = "output", filename="2020-03-02__sleep_deficit.png", dpi = 600, width = 12, height = 9, type="cairo")

