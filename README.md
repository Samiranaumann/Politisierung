# Politisierung

In diesem Forschungsprojekt soll es um den möglichen Einfluss der Nutzungshäufigkeit sozialer Netzwerke auf den persönlichen politischen Entwicklungsprozess gehen. 


### Teammitglieder
```
Nils Rusch
Samira Naumann
Lena Lange
Tanin Sohi
Isabelle Kricsfalussy-Hrabar
Janina Bleeck
```

## Forschungsfrage

_Welchen Einfluss hat die Nutzungshäufigkeit von sozialen Netzwerken auf die politische Polarisierung?_

## Unterschiedshypothesen

1. Es gibt einen Unterschied zwischen der Nutzungshäufigkeit sozialer Netzwerke von Männern und Frauen.
2. Facebook-Nutzer die politisch motivierten Seiten folgen, sind in ihrer Meinung beeinflussbarer als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen.
3. Facebook-Nutzer die die Plattform hauptsächlich als Informationsquelle nutzen, sind politisch aktiver, als die Facebook-Nutzer, die diese zur Unterhaltung und Kontaktpflege nutzen. 


## Unterschiedshypothesen alt

1. Es gibt einen Unterschied zwischen der Nutzungshäufigkeit sozialer Netzwerke von Männern und Frauen.
2. Facebook-Nutzer die politisch motivierten Seiten folgen, sind in ihrer Meinung unbeeinflussbarer, als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen. 
3. Facebook-Nutzer die die Plattform hauptsächlich als Informationsquelle nutzen, sind nicht politisch aktiver, 
 als die Facebook-Nutzer, die diese zur Unterhaltung und Kontaktpflege nutzen.
   
 ## Zusammenhangsshypothesen
 1. Ob man sich in seiner politischen Meinung beeinflussen lässt, hängt davon ab, ob man auf Facebook politisch motivierten Seiten folgt.
 2. Welche User eher Politikern auf Facebook folgen, ist abhängig vom Geschlecht.
 3. Welche User eher politisch motivierte Beiträge auf Facebook kommentieren, ist abhängig vom Geschlecht.

## Hinweise zum Umgang mit P-Values

Im Sinne des Signifikanztests soll herausgefunden werden, ob die Unterschiede allein auf Zufall beruhen. Dafür nutzen wir, wie alle Sozialforscher, den p-Wert. 
Das anfänglich festgelegte α-Niveau, meist 0,05=5% wird als Maß für die Ergebnisse genutzt. Folgernd wird bei p < 0.5 die Nullhypothese verworfen. 
Wenn der p-Wert klein genug ist, kann man sagen, dass es mehr als nur ein Zufall ist. Der Schwellenwert ist in unserem Fall, wie üblich, 5%. Wenn also der p-Wert kleiner als 5% ist, könnten wir sagen, dass unser Ergebnis kein Zufall ist. 

Zusätzliches Material zum weiteren Verständnis des p-Wertes: http://blog.minitab.com/blog/adventures-in-statistics-2/not-all-p-values-are-created-equal


## Überarbeitete Hinweise zum Umgang mit P-Values

Im Sinne der Überprüfung von Hypothesen mittels Signifikanztest soll herausgefunden werden, wie wahrscheinlich ein Ergebnis ist, wenn H0 stimmt. Dafür nutzen wir, wie alle Sozialforscher, den p-Wert.
Der p-Wert ist ein Hilfsmittel zur Hypothesentestung und gibt Aufschluss darüber, ob es sich um Zufallsergebnisse handeln könnte.
Das anfänglich festgelegte α-Niveau, meist 0,05=5%, dient als Messgrenze zur Überprüfung von H0.
Folgernd wird bei p < .05 die Nullhypothese verworfen, da es wahrscheinlich ist, dass es sich um Zufallsergebisse handelt.
Wird H0 verworfen, wird die H1 angenommen. 
Sollte H0 jedoch richtig sein, trifft der p-Wert Aussagen über die Wahrscheinlichkeit der Daten. 

Zusätzliches Material zum weiteren Verständnis des p-Wertes: 
http://blog.minitab.com/blog/adventures-in-statistics-2/not-all-p-values-are-created-equal
https://www.youtube.com/watch?v=gSyGVDMcg-U


## Faktorenraum

![tooltip](images/Faktorenraum.jpg)



## Feedback zu Unterschieds- und Zusammenhangshypothesen

Tipp: Solche Aufzählungen immer auskommentieren, sonst weiß R damit nichts anzufangen.
Unterschiedshypothese 1 ist unglücklich formuliert. Es geht um den Unterschied IN der Nutzungshäufigkeit.

In Unterschiedshypothese 3 wirkt der Unterschied zwischen den Gruppen etwas willkürlich und nicht sehr trennscharf.
Zusammenhangshypothesen:

ad 1: Hier geht etwas unter, dass sie die Beeinflussbarkeit nicht messen können. Sie messen die selbst empfundene Beinflussbarkeit.

ad 2 und 3: Hier ist ein richtig fetter Fehler drin: Geschlecht ist keine ordinale Variable, d.h. wenn überhaupt können Sie das mit Unterschiedshypothesen überprüfen.

Die Formulierung mit "Welche" ist auch ein bisschen irreführend.


## Unterschiedsshypothesen überarbeitet
1: Es gibt einen Unterschied in der Nutzungshäufigkeit sozialer Netzwerke zwischen Männern und Frauen.

2: Facebook-Nutzer die politisch motivierten Seiten folgen, sind in ihrer Meinung unbeeinflussbarer, als die Facebook-Nutzer, die keinen politisch motivierten Seiten folgen.

3: Facebook-Nutzer die die Plattform ausschließlich als Informationsquelle nutzen, sind nicht politisch aktiver, als die Facebook-Nutzer, die diese ausschließlich zur Unterhaltung und Kontaktpflege nutzen.


## Zusammenhangsshypothesen überarbeitet
1: Ob man sich in seiner politischen Meinung beeinflusst fühlt, hängt davon ab, ob man auf Facebook politisch motivierten Seiten 
folgt.

2: User, die auf Facebook einer politischen Partei/ einem Politiker folgen, veröffentlichen ihre politische Meinung auf der Plattform.

3: User, die auf Facebook politisches Geschehen verfolgen, lassen sich eher in ihrer politischen Meinung beeinflussen. 

## Ergebnisse

## Unterschiedshypothese 
1. Facebook-Nutzungshäufigkeit zwischen Männern und Frauen
Der U-Test ergab einen p-Wert von 0.08008 und stellt somit kein signifikantes Ergebnis dar. Das bedeutet, dass es keinen Unterschied in der Facebook-Nutzungshäufigkeit zwischen den Geschlechtern gibt. Somit kann die Alternativhypothese zu Gunsten der Nullhypothese verworfen werden.

## Zusammenhangshypothesen
1. Beeinflussung der politischen Meinung
Die lineare Regression ergab einen P-Wert von 0.001 und zeigt somit ein signifikantes Ergebnis. Das bedeutet, dass es einen Zusammenhang zwischen der Beeinflussung der polit. Meinung und dem Folgen polit. motivierten Seiten gibt und die H1 angenommen wird.

2. Politikern folgen und politischen Meinung veröffentlichen
Auch hier konnte mit Hilfe der Linearen Regression ein signifikantes Ergebnis mit einem P-Wert von 0.001 errechnet werden. Es besteht also ein Zusammenhang zwischen dem Folgen von Politikern und dem Veröffentlichen seiner Meinung .Somit wird ebenfalls die H1 angenommen und die H0 verworfen.
