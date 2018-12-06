# Analyse Script
print("Hier werden Pakete eingebunden")

# Data Cleaning ----
print("Hier wird der Datensatz aufbereitet. Thema am 09.11.2018")

# Skalenberechnung ----
print("Hier werden später Skalen berechnet. Thema am 09.11.2018")

# Analyse ----
print("Hier werden später statistische Analysen durchgeführt. Thema ab dem 16.11.2018")

# Graphik erstellung ---- 
print("Hier werden später Grafiken erstellt. Thema ab dem 16.11.2018")

install.packages("tidyverse")
install.packages("lubridate")
install.packages("psych")
install.packages("esquisse")
install.packages("ggthemes")
install.packages("ggplot2")

install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")

source("surveymonkey.R")

# Datei laden ----

filename <- "data/Digitale_Muendigkeit.csv"
raw <- load_surveymonkey_csv(filename)

raw.short <- raw[,c(-1:-9,-13,-15:-118,-137)]

names( raw.short ) %>% View()

names(raw.short)[1] <- "age"
names(raw.short)[2] <- "gender"
names(raw.short)[3] <- "education"
names(raw.short)[4] <- "profession"
names(raw.short)[5] <- "fbnutzen"
names(raw.short)[6] <- "fbaktivpassiv"
names(raw.short)[7] <- "fbinteraktion"
names(raw.short)[8] <- "fbinteresse"
names(raw.short)[9] <- "fbwofuer"
names(raw.short)[10] <- "followpolitik1"
names(raw.short)[11] <- "followpolitik2"
names(raw.short)[12] <- "postspolitik1"
names(raw.short)[13] <- "postspolitik2"
names(raw.short)[14] <- "comments"
names(raw.short)[15] <- "politischeinhalte"
names(raw.short)[16] <- "politischemeinung"
names(raw.short)[17] <- "politikreaktion"
names(raw.short)[18] <- "politischinformiert"
names(raw.short)[19] <- "politischemeinungposts1"
names(raw.short)[20] <- "politischemeinungposts2"
names(raw.short)[21] <- "politischemeinungposts3"
names(raw.short)[22] <- "fbpolitischethemen"

skala.zustimmung <- c("Trifft gar nicht zu",
                               "Trifft nicht zu",
                               "Trifft eher nicht zu",
                               "Trifft eher zu",
                               "Trifft zu",
                               "Trifft voll zu")

raw.short$gender <- factor(raw.short$gender)
raw.short$fbpolitischethemen <- ordered(raw.short$fbpolitischethemen, 
                                        levels = c("Trifft gar nicht zu",
                                                   "Trifft nicht zu",
                                                   "Trifft eher nicht zu",
                                                   "Trifft eher zu",
                                                   "Trifft zu",
                                                   "Trifft voll zu"))

raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)
raw.short$politischemeinungposts2 <- ordered(raw.short$politischemeinungposts2, levels = skala.zustimmung)
raw.short$politischemeinungposts1 <- ordered(raw.short$politischemeinungposts1, levels = skala.zustimmung)
raw.short$politischinformiert <- ordered(raw.short$politischinformiert, levels = skala.zustimmung)
raw.short$politikreaktion <- ordered(raw.short$politikreaktion, levels = skala.zustimmung)
raw.short$politischemeinung <- ordered(raw.short$politischemeinung, levels = skala.zustimmung)
raw.short$politischeinhalte <- ordered(raw.short$politischeinhalte, levels = skala.zustimmung)
raw.short$comments <- ordered(raw.short$comments, levels = skala.zustimmung)

raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)
raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)
raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)
raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)

raw.short$postspolitik2
raw.short$postspolitik1
raw.short$followpolitik2
raw.short$followpolitik1
raw.short$fbwofuer
raw.short$fbinteresse
raw.short$fbinteraktion
raw.short$fbaktivpassiv
raw.short$fbnutzen
raw.short$profession
raw.short$education
raw.short$gender
raw.short$age

raw.short$fbaktivpassiv <- ordered(raw.short$fbaktivpassiv, 
                                        levels = c("stimme gar nicht zu",
                                                   "stimme nicht zu",
                                                   "stimme eher nicht zu",
                                                   "stimme eher zu",
                                                   "stimme zu",
                                                   "stimme voll zu"))

raw.short




