data <- readr::read_csv("data/w06/data/us_life_at_war.csv")

summary(data)
library(ggplot2)
library(extrafont)
loadfonts()

barsize <- 1.8
offset <- 0.005

data <- dplyr::mutate(data,
              age = 2020-birth_year,
              age_group = ifelse(age <= 18, "0-18",
                                 ifelse(age > 18 & age <= 40, "19-40",
                                        ifelse(age > 40 & age <= 60, "41-60",
                                               ifelse(age>60 & age < 75, "60-75",
                                                      "75 and over"))
                                              )
                                 ),
              at_war = ifelse(of_your_life_the_us_has_been_at_war > 1, 1, of_your_life_the_us_has_been_at_war),
              at_peace = ifelse(1-at_war <= offset, offset, 1-at_war))

wars <- data.frame(name = c("WW1", "WW2", "Korean War", "Vietnam War",
                            "Persian Gulf War", "War in Afghanistan"),
                   start = c(1917, 1941, 1950, 1965, 1990, 2001),
                   end = c(1918, 1945, 1953, 1975, 1991, 2020))
label <- data.frame(label = c("Percentage of life lived", "...at war", "... at peace"),
                    x = c(1906, 1933, 1933),
                    y= c(0.8, 0.8, 0.7),
                    colour = c("0", "1", "2"))



ggplot(data) + theme_minimal() + 
  geom_rect(data=wars, mapping = aes(xmin=start-0.25, xmax = end-0.5,
                                     ymin = -1, ymax = 1), fill = "#a9a9a9", alpha = 0.33) + 
  geom_segment(mapping = aes(x=birth_year, xend = birth_year,
                             y = offset, yend = at_war), colour = "darkred", size = barsize) + 
  geom_segment(mapping = aes(x=birth_year, xend = birth_year,
                             y = -offset, yend = -at_peace), colour = "steelblue", size = barsize) +
  geom_text(data=wars, mapping=aes(x = end-1, y= -0.99, label = name),
             fontface = "bold", angle = 90, hjust = 0, vjust = 0) +
  annotate("rect", xmin = 1905, xmax = 1947, ymin = 0.65, ymax = 0.85, fill = "white") +
  geom_text(data=label, mapping=aes(x=x, y=y, label = label, colour = colour),
            fontface = "bold", hjust = 0, size = 4) + 
  scale_color_manual(values=c("1" = "darkred", "2" = "steelblue", "0" = "black")) + 
  coord_cartesian(expand = FALSE, xlim = c(1904, 2020), ylim = c(-1, 1)) + 
  scale_y_continuous(labels = abs) + 
  theme(legend.position = "none",
        text = element_text(family="Calibri"),
        plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=12)) +
  labs(x = "Year of birth", y = NULL, 
       title = "More and more Americans have spent most of their lifes in times of war",
       subtitle = stringr::str_wrap("Americans born in 1980 have been the last to have lived half their life in times of peace, while todays 19 year olds have never known the US at peace.",
                                    120),
       caption = "Visualization: Uwe Hadler | #MakeoverMonday 2020 Week 6 | Source: Washington Post")

ggsave(path = "output", filename="2020_week6__usa-wars.png", dpi = 400, width = 8, height = 6, type="cairo")
