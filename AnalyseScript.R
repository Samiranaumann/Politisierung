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

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("psych")
#install.packages("esquisse")
#install.packages("ggthemes")
#install.packages("ggplot2")

#install.packages("devtools")
#library(devtools)
#devtools::install_github("HCIC/r-tools")

#source("surveymonkey.R")



#library(tidyverse)
#source("Digitale_Muendigkeit.csv")
#filename <- "data/Digitale_Muendigkeit.csv"
#raw <- surveymonkey.R_csv(filename)

#View(raw)

#raw.short <- raw[,c(-1:-8)]

t.test( filter(data,gender=="männlich")$KUT, 
       filter(data,gender=="weiblich")$KUT )




