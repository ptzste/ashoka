setwd("G:/")
getwd()

library(foreign)
library(devtools)
library(haven)
library(magrittr)
library(dplyr)
library(tidyr)
#install.packages("lubridate")
library(lubridate)

######
# Einlesen der Daten
#############

##############
# Bewerber 2013 
# Lese Rohdaten ein 
bew13 <- read.csv("Bewerber_2013.csv" 
                  ,sep= ";" 
                  , dec = "," 
                  ,header = TRUE) 

################
# Transformiere die Variablen 

# Jahr der Bewerbung 
bew13$year <- 2013 
# Status der Bewerbung 
bew13$statusBewerber <- bew13$Status %in% c("","Bewerber TZ", "Bewerber VZ") # Wenn das Feld nicht ausgefÃ¼llt wurde, wird angenommen, dass es sich um Bewerber handelt (weder Finalisten noch Stipendiaten)
bew13$statusFinalist <- bew13$Status %in% c("Finalist TZ", "FInalist VZ")
bew13$statusStipendiat <- bew13$Status %in% c("Stipendiat TZ", "Stipendiat VZ")
# Geburtsjahr des Bewerbers 
bew13$applBirthyear <- bew13$Geburtsdatum
bew13$applBirthyear <- as.Date(bew13$applBirthyear, format="%d.%m.%Y")
bew13$applBirthyear <- year(bew13$applBirthyear)
# Geschlecht des Bewerbers 
bew13$applGender[bew13$Geschlecht=="weiblich"] <- "w"
bew13$applGender[bew13$Geschlecht=="männlich"] <- "m"
# Ort des Bewerbers 
bew13$applPlace <- bew13$Stadt
# Postleitzahl des Bewerbers 
bew13$applPostalCode <- bew13$Postleitzahl
# Rechtsstatus der Organisation 
bew13$orgLegalStatus <- NA # Codiere als missing im Jahrgang 2013  
# Gründungsjahr der Organisation 
bew13$orgFoundyear <- NA  # Codiere als missing im Jahrgang 2013
# Größe der Organisation 
bew13$orgSizeMembers <- bew13$Anzahl.an.aktiven.Mitgliedern
# Größe des Kernteams 
bew13$orgSizeCore <- bew13$Größe.des.Kernteams
# Themenfeld 
bew13$orgTopic <- bew13$Themenfeld..welches.Dein.Projekt.am.besten.umschreibt
bew13$orgTopic[bew13$orgTopic==""] <- NA
# Geschätzer Impact 
bew13$estImpact <- bew13$Anzahl.an.Personen..die.Dein.Projekt.erreicht 
# Pitch 
bew13$pitch <- NA 
# Problem 
bew13$problem <- NA 

# Entferne nicht benötigte Rohdaten  
bew13 <- bew13[,-(1:28)]  


################
# Bewerber 2014
bew14 <- read.csv("Bewerber_2014.csv"
                    ,sep= ";"
                    , dec = ","
                    ,header = TRUE)

################
# Transformiere die Variablen 

# Jahr der Bewerbung 
bew14$year <- 2014
# Status der Bewerbung 
bew14$statusBewerber <- bew14$Bewerber
bew14$statusFinalist <- bew14$Finalist
bew14$statusStipendiat <- bew14$Stipendiat
# Geburtsjahr der Bewerber 
bew14$applBirthyear <- bew14$Zu.Dir.als.Person...Geburtsdatum..Tag.Monat.Jahr.
bew14$applBirthyear <- as.Date(bew14$applBirthyear, format="%d.%m.%Y")
bew14$applBirthyear <- year(bew14$applBirthyear)
# Geschlecht der Bewerber 
bew14$applGender[bew14$Geschlecht=="Frau"] <- "w"
bew14$applGender[bew14$Geschlecht=="Mann"] <- "m"
# Ort des Bewerbers 
bew14$applPlace <- bew14$Zu.Dir.als.Person...Ort
# Postleitzahl des Bewerbers 
bew14$applPostalCode <- bew14$Zu.Dir.als.Person...PLZ 
# Rechtsform der Organisation 
bew14$orgLegalStatus <- bew14$Zum.Projekt....Rechtsform..wenn.vorhanden. 
# Gründungsjahr der Organisation 
bew14$orgFoundyear <- bew14$Abschließend.einige.Fragen.für.unsere.Gesamtstatistik.von.PEP...also.lieber.realistisch..als.großartig.........Wann.wurde.Dein.Projekt.gegründet.....Beginn.der.Umsetzungsphase..Tag.Monat.Jahr....Open.Ended.Response
# Größe der Organisation 
bew14$orgSizeMembers <- bew14$Wie.viele.Personen.......zählen.sich.als.Mitglieder.des.Projekts.
# Größe des Kernteams 
bew14$orgSizeCore <- bew14$Wie.viele.Personen.......engagieren.sich.im.Kernteam.
# Themenfeld 
# TODO Recodieren als Dummy-Variablen 
bew14$orgTopic <- NA 
str(bew14$In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Kultur)
bew14 <- unite_(bew14, "orgTopic", c("In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Bildung","In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Entwicklungszusammenarbeit.2.0", "In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Gesundheit.und.Ernährung","In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Jugendförderung", "In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Kultur", "In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Social.Business", "In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Umwelt", "In.welchem.Themenfeld.bist.Du.mit.Deinem.Projekt.aktiv....Sonstiges..bitte.angeben."), sep=" ", remove=F) # bew14$org_topic als summe der antworten
# Geschätzer Impact 
bew14$estImpact <- bew14$Wie.viele.Personen.......habt.Ihr.mit.Euren.Angeboten.erreicht.
# Pitch 
bew14$pitch <- bew14$Versuche.Dein.Projekt.in.wenigen.Sätzen.so.zu.erklären..dass.auch.ein.Außenstehender..der.weder.Deine.Arbeit.noch.das.gesellschaftliche.Problem.kennt..versteht.was.Du.tust.und.bewirkst...max..1.000.Zeichen....Open.Ended.Response
# Problem 
bew14$problem <- bew14$Welches.gesellschaftliche.Problem.gehst.Du.mit.Deinem.Projekt.an.und.mit.welcher.Wirkung.möchtest.Du.langfristig.zu.dessen.Lösung.beitragen...max..1.000.Zeichen....Open.Ended.Response

# Entferne nicht benötigte Rohdaten  
bew14 <- bew14[,-(1:66)]  


################
# Bewerber 2015
bew15 <- read.csv("Bewerber_2015.csv"
                  ,sep= ";"
                  , dec = ","
                  ,header = TRUE)

################
# Transformiere die Variablen 

# Jahr der Bewerbung 
bew15$year <- 2015
# Status der Bewerbung 
bew15$statusBewerber <- bew15$Bewerber
bew15$statusFinalist <- bew15$Finalist
bew15$statusStipendiat <- bew15$Stipendiat
# Geburtsjahr der Bewerber 
bew15$applBirthyear <- bew15$Geburtsdatum..Tag.Monat.Jahr.
bew15$applBirthyear <- as.Date(bew15$applBirthyear, format="%d.%m.%Y")
bew15$applBirthyear <- year(bew15$applBirthyear)
# Geschlecht des Bewerbers 
bew15$applGender[bew15$Geschlecht=="Frau"] <- "w"
bew15$applGender[bew15$Geschlecht=="Mann"] <- "m"
# Ort des Bewerbers 
bew15$applPlace <- bew15$Ort
# Postleitzahl des Bewerbers 
bew15$applPostalCode <- bew15$PLZ
# Rechtsstatus der Organisation 
bew15$orgLegalStatus <- bew15$Rechtsform..wenn.vorhanden. 
# Gründungsjahr der Organisation 
bew15$orgFoundyear <- bew15$Beantwortungen.mit.unbestimmtem.Ende
# Größe der Organisation 
bew15$orgSizeMembers <- bew15$zählen.sich.als.Mitglieder.des.Projekts.
# Größe des Kernteams 
bew15$orgSizeCore <- bew15$engagieren.sich.im.Kernteam.
# Themenfeld 
# TODO: Recodieren als Dummies  
bew15$orgTopic <- NA 
bew15 <- unite_(bew15, "org_topic", c("Bildung", "Entwicklungszusammenarbeit.2.0", "Gesundheit.und.Ernährung", "Jugendförderung", "Kultur", "Social.Business", "Umwelt", "Sonstiges..bitte.angeben."), sep=" ", remove=F)
# Geschätzter Impact 
bew15$estImpact <- bew15$habt.Ihr.mit.Euren.Angeboten.erreicht.
# Pitch 
bew15$pitch <- bew15$Dein.Pitch.in.500.Zeichen.für.alle..die.weder.Dich..noch.Dein.Projekt.oder.Dein.Team.kennen.
# Problem 
bew15$problem <- bew15$Welches.gesellschaftliche.Problem.geht.Ihr.mit.Deinem.Projekt.an.und.auf.welche.Weise.versucht.Ihr.es.zu.lösen...max..1.000.Zeichen.

# Entferne nicht benötigte Rohdaten  
bew15 <- bew15[,-(1:77)]


##############
# Aus allen drei JahrgÃ¤ngen einen Datensatz erstellen

bewall <- rbind (bew13, bew14, bew15)



###############
## Statistische Tests
##############

# Herkunftsort
table(bew13$Stipendiat, bew13$appl_place) %>% chisq.test()
table(bew13$Stipendiat, bew13$Berlin) %>% chisq.test()

table(bew14$Stipendiat, bew14$appl_place) %>% chisq.test()
table(bew14$Stipendiat, bew14$Berlin) %>% chisq.test()

table(bew15$Stipendiat, bew15$appl_place) %>% chisq.test()
table(bew15$Stipendiat, bew15$Berlin) %>% chisq.test()



