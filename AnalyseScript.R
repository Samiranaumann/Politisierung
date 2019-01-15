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

filename <- "data/DigitaleMuendigkeit_all.csv"
raw <- load_surveymonkey_csv(filename)

#Dummydatensätze gelöscht

raw.short <- raw[c(-544:-554),c(-1:-9,-13,-15:-118,-137)]

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
library(psych)
schluesselliste <- list(fbpolitik = c("politischinformiert", "politikreaktion", "politischemeinung", "politischeinhalte", "comments"),
                        fbmeinung = c("fbpolitischethemen", "politischemeinungposts3", "politischemeinungposts2", "politischemeinungposts1"),
                        fbaktiv = c("fbaktivpassiv"))
scores <- scoreItems(schluesselliste, raw.short, min = 1, max = 6)
scores$alpha
scores$scores
data <- bind_cols(raw.short, as.tibble(scores$scores))
View(data)
data <- data %>%
  select(-starts_with("fbpolitik", ignore.case = F)) %>%
  select(-starts_with("fbmeinung", ignore.case = F)) %>%
  select(-starts_with("fbaktiv", ignore.case = F))
saveRDS(data, "data/DigitaleMuendigkeit_all.rds")

#### Unterschiedsshypothesen
#1: Es gibt einen Unterschied zwischen der Nutzungshäufigkeit sozialer Netzwerke von Männern und Frauen.
#2:Facebook-Nutzer die politisch motivierten Seiten folgen, sind in ihrer Meinung unbeeinflussbarer, 
#als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen.
#3:Facebook-Nutzer die die Plattform hauptsächlich als Informationsquelle nutzen, sind nicht politisch aktiver, 
#als die Facebook-Nutzer, die diese zur Unterhaltung und Kontaktpflege nutzen.
  
#### Zusammenhangsshypothesen
#1: Ob man sich in seiner politischen Meinung beeinflussen lässt, hängt davon ab, ob man auf Facebook politisch motivierten Seiten folgt.
#2: Welche User eher Politikern auf Facebook folgen, ist abhängig vom Geschlecht.
#3. Welche User eher politische motivierte Beiträge auf Facebook kommentieren, ist abhängig vom Geschlecht.

#df <- hcictools::robot_care 
#jmv::linReg(df, dep = c("robo_bed"), covs = c("kut"), blocks = list("kut"), 
#            r2Adj = T, stdEst = T, anova=T)  


## FEEDBACK: Da Sie in den meisten Fällen keinen Code zu der Hypothese geschrieben haben, kann ich Ihnen leider kein Feedback zum Code geben. 
# Tipp: Solche Aufzählungen immer auskommentieren, sonst weiß R damit nichts anzufangen.
# Unterschiedshypothese 1 ist unglücklich formuliert. Es geht um den Unterschied IN der Nutzungshäufigkeit.
# In Unterschiedshypothese 3 wirkt der Unterschied zwischen den Gruppen etwas willkürlich und nicht sehr trennscharf.
# Zusammenhangshypothesen:
# ad 1: Hier geht etwas unter, dass sie die Beeinflussbarkeit nicht messen können. Sie messen die selbst empfundene Beinflussbarkeit.
# ad 2 und 3: Hier ist ein richtig fetter Fehler drin: Geschlecht ist keine ordinale Variable, d.h. wenn überhaupt können Sie das mit Unterschiedshypothesen überprüfen.
# Die Formulierung mit "Welche" ist auch ein bisschen irreführend.


#### Unterschiedsshypothesen überarbeitet
#1: Es gibt einen Unterschied in der Nutzungshäufigkeit sozialer Netzwerke zwischen Männern und Frauen.
#2: Facebook-Nutzer, die politisch motivierten Seiten folgen, sind in ihrer Meinung unbeeinflussbarer, als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen.
#3: Facebook-Nutzer, die die Plattform ausschließlich als Informationsquelle nutzen, sind nicht politisch aktiver, als die Facebook-Nutzer, die diese ausschließlich zur Unterhaltung und Kontaktpflege nutzen.


#### Zusammenhangsshypothesen überarbeitet
#1: Ob man sich in seiner politischen Meinung beeinflusst fühlt, hängt davon ab, ob man auf Facebook politisch motivierten Seiten folgt.
#2: User, die auf Facebook einer politischen Partei/ einem Politiker folgen, veröffentlichen ihre politische Meinung auf der Plattform.
#3. User, die auf Facebook politisches Geschehen verfolgen, lassen sich eher in ihrer politischen Meinung beeinflussen. 

raw.short <- raw(-544)
