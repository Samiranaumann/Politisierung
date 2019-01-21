### AnalyseScript.R

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("psych")
#install.packages("esquisse")
#install.packages("ggthemes")
#install.packages("ggplot2")

# install.packages("devtools")
# library(devtools)
# devtools::install_github("HCIC/r-tools")

source("surveymonkey.R")

# Datei laden ----

filename <- "data/DigitaleMuendigkeit_all.csv"
raw <- load_surveymonkey_csv(filename)

#Dummydatensätze und nicht relevante Variablen löschen:

raw.short <- raw[c(-544:-554),c(-1:-9,-13,-15:-118,-137)]

# Variablen umbenennen:

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

# Variablen das richtige Skalenniveau zuordnen:

raw.short$gender <- factor(raw.short$gender)
raw.short$education <- factor(raw.short$education)
raw.short$profession <- factor(raw.short$profession)

raw.short$fbnutzen <- ordered(raw.short$fbnutzen, levels = c("gar nicht",
                                                             "monatlich",
                                                             "mehrmals monatlich",
                                                             "wöchentlich",
                                                             "mehrmals wöchentlich",
                                                             "täglich",
                                                             "mehrmals täglich"))

raw.short$fbaktivpassiv <- ordered(raw.short$fbaktivpassiv, 
                                   levels = c("stimme gar nicht zu",
                                              "stimme nicht zu",
                                              "stimme eher nicht zu",
                                              "stimme eher zu",
                                              "stimme zu",
                                              "stimme voll zu"))

raw.short$fbinteraktion <- factor(raw.short$fbinteraktion)
raw.short$fbinteresse <- factor(raw.short$fbinteresse)
raw.short$fbwofuer <- factor(raw.short$fbwofuer)

#### TODO: Wie sehen die Skalen aus?

raw.short$followpolitik1 <- ordered(raw.short$followpolitik1, 
                                    levels =c("...keiner politischen Partei.",
                                              "...einer politischen Partei.",
                                              "...mehreren politischen Parteien."))


raw.short$followpolitik2 <- ordered(raw.short$followpolitik2, 
                                    levels =c("...keinen Politikern.",
                                              "...einem Politiker/ einer Politikerin.",
                                              "...mehreren Politikern."
                                              ))

raw.short$postspolitik1 <- ordered(raw.short$postspolitik1, 
                                   levels =c("...gar nicht",
                                             "...indem ich sie bewerte (\"Like\", \"Love\", \"Haha\", \"Wow\", \"Traurig\", \"Wütend\").",
                                             "...indem ich sie kommentiere.",
                                             "...indem ich sie teile."))

raw.short$postspolitik2 <- ordered(raw.short$postspolitik2, 
                                   levels =c("...gar nicht",
                                             "...indem ich sie bewerte (\"Like\", \"Love\", \"Haha\", \"Wow\", \"Traurig\", \"Wütend\").",
                                             "...indem ich sie kommentiere.",
                                             "...indem ich sie teile."))

skala.zustimmung <- c("Trifft gar nicht zu",
                      "Trifft nicht zu",
                      "Trifft eher nicht zu",
                      "Trifft eher zu",
                      "Trifft zu",
                      "Trifft voll zu")

raw.short$comments <- ordered(raw.short$comments, levels = skala.zustimmung)
raw.short$politischeinhalte <- ordered(raw.short$politischeinhalte, levels = skala.zustimmung)
raw.short$politischemeinung <- ordered(raw.short$politischemeinung, levels = skala.zustimmung)
raw.short$politikreaktion <- ordered(raw.short$politikreaktion, levels = skala.zustimmung)
raw.short$politischinformiert <- ordered(raw.short$politischinformiert, levels = skala.zustimmung)
raw.short$politischemeinungposts1 <- ordered(raw.short$politischemeinungposts1, levels = skala.zustimmung)
raw.short$politischemeinungposts2 <- ordered(raw.short$politischemeinungposts2, levels = skala.zustimmung)
raw.short$politischemeinungposts3 <- ordered(raw.short$politischemeinungposts3, levels = skala.zustimmung)
raw.short$fbpolitischethemen <- ordered(raw.short$fbpolitischethemen, levels = skala.zustimmung)


library(psych)
schluesselliste <- list(FBPOLITIK = c("politischinformiert", "politikreaktion", "politischemeinung", "politischeinhalte", "comments"),
                        FBMEINUNG = c("fbpolitischethemen", "politischemeinungposts3", "politischemeinungposts2", "politischemeinungposts1"),
                        FBAKTIV = c("fbaktivpassiv"),
                        FOLLOWPOLITIK = c("followpolitik1", "followpolitik2"),
                        POSTSPOLTIK = c("postspolitik1", "-postspolitik2"),
                        FBNUTZEN = c("fbnutzen"))

scores <- scoreItems(schluesselliste, raw.short, min = 1, max = 6, missing = TRUE)

data <- bind_cols(raw.short, as.tibble(scores$scores))

data <- data %>%
  select(-starts_with("politi", ignore.case = F)) %>%
  select(-starts_with("comments", ignore.case = F))

saveRDS(data, "data/DigitaleMuendigkeit_all.rds")




#### Unterschiedsshypothesen
#1: Es gibt einen Unterschied zwischen der Nutzungshäufigkeit sozialer Netzwerke von Männern und Frauen.

# T-Test

t.test(filter(data, data$gender == "männlich")$FBNUTZEN, filter(data, data$gender == "weiblich")$FBNUTZEN)
### Kleiner Hinweis noch: Wenn Sie einen T-Test mit einem einzelnen Item als AV rechnen möchten ist der T-Test eigentlich der falsche Test.
### Der richtige Test heißt U-Test und wird genau so interpretiert wie der T-Test: Nullhypothese verwerfen, wenn p unter dem Signifikanzniveau liegt.
### Der U-Test geht so:
wilcox.test(filter(data, data$gender == "männlich")$FBNUTZEN, filter(data, data$gender == "weiblich")$FBNUTZEN)


#2:Facebook-Nutzer die politisch motivierten Seiten folgen, sind in ihrer Meinung unbeeinflussbarer, 
#als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen.

#3:Facebook-Nutzer die die Plattform hauptsächlich als Informationsquelle nutzen, sind nicht politisch aktiver, 
#als die Facebook-Nutzer, die diese zur Unterhaltung und Kontaktpflege nutzen.
  
#### Zusammenhangsshypothesen
#1: Ob man sich in seiner politischen Meinung beeinflussen lässt, hängt davon ab, ob man auf Facebook politisch motivierten Seiten folgt.
#2: Welche User eher Politikern auf Facebook folgen, ist abhängig vom Geschlecht.
#3. Welche User eher politische motivierte Beiträge auf Facebook kommentieren, ist abhängig vom Geschlecht.

#df <- hcictools::robot_care 
#jmv::linReg(data, dep = c("FBNUTZEN"), covs = c("kut"), blocks = list("kut"), 
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
<<<<<<< HEAD


=======
>>>>>>> 48e886a23077aee30cee56204ceec07b3bbd9d91
