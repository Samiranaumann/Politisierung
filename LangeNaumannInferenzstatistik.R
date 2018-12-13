install.packages("plotrix")
install.packages("jmv")

library(tidyverse)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotrix)
library(jmv)

install.packages("robo_pflege")

data_robot <- readRDS("data/robo_pflege.rds")


t.test(filter(data_robot, gender == "weiblich") $ robot_food,
       filter(data_robot, gender == "männlich") $ robot_food)


data_robot %>%
  filter(gender != "keine Angabe") %>%
  group_by(gender) %>%



#Frauen lassen sich eher von einem Roboter verpflegen als Männer
ggplot(data = robo_pflege) +
  aes(x = gender, weight = robo_food) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Frauen akzeptieren eher die Verpflegung durch einen Roboter als Männer",
       x = "Geschlecht",
       y = "Ernährung durch Roboter",
       subtitle = "Boxplot der Akzeptanz nach Geschlecht") +
  theme_minimal()
