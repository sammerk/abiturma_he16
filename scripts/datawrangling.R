###################################################
# Datawrangling script abiturma <-> feedbackapp   #
###################################################

##    ---> mit strg + f nach "### Aktualisieren" suchen ########################################################

## Import und Merging FB Data ##################################################################################

## Import der Kursnummer/Kursleiterschlüssels und Ausschluss in der Zukunft liegender Kurse
kn_kl_key <- read.csv(url('https://samuel.merk%40uni-tuebingen.de:afbogena@www.abiturma.de/dornier/api/daten-fragebogen'), encoding="UTF-8")%>%
  mutate(Kursstart = lubridate::dmy(Kursstart))%>%
  filter(Kursstart < lubridate::as_date("2016-10-30"))                                                                    ### Aktualisieren 

## Import der Rohdaten aus dem Frühjahr 2016
rawdata_fr16 <- data.table::fread("data/data_raw/rawdata_fr16_utf8.csv", sep = ";", na.strings = "NA")       #x#x Missing in ID Variablen?

## Import Inkrement
rawdata_inkrement_imp <- data.table::fread("rawdata_charge14/daten.csv", sep = ";")                                     ### Aktualisieren
rawdata_inkrement_raw <- rawdata_inkrement_imp

## Match Inkrement + kn_kl_key 
rawdata_inkrement_raw$Klassen.Id <- rawdata_inkrement_raw$ID
rawdata_inkrement_raw$em2 <- NULL ## zu ändern im Questor-Codeplan
rawdata_inkrement_raw$em2 <- NULL ## zu ändern im Questor-Codeplan
rawdata_inkrement <- dplyr::left_join(rawdata_inkrement_raw, kn_kl_key, by = "Klassen.Id")%>%
  filter(is.na(Kurs.Id)==F)                                                                             ## NA entstehen durch falsche IDs auf Fragebögen


## Vorbereitung Kursdatum
rawdata_fr16$Kursdatum <- lubridate::dmy(rawdata_fr16$Kursbeginn)
rawdata_inkrement$Kursdatum <- rawdata_inkrement$Kursstart
##Vorbereitung Uhrzeit
rawdata_fr16$Uhrzeit <- rawdata_fr16$Kurszeit

##Vorbereitung Username
rawdata_fr16$Username <- "abiturma_vor_he16"

## Vorbereitung Kursort
rawdata_inkrement$Kursort <- gsub(" \\(.*", "", rawdata_inkrement$Kursbezeichnung)
rawdata_fr16$Kursort <- rawdata_fr16$Veranstaltungsort

## Vorbereitung Klassen.Id
rawdata_fr16$Klassen.Id <- rawdata_fr16$ID

## match fr_16 und Inkrement Herbst'16
rawdata <- dplyr::full_join(rawdata_inkrement, rawdata_fr16)


###################################################
# Manuelle Überarbeitungsschritte                 #
###################################################

## Kurs 44 löschen, da Springer bewertet wurde                         ### Aktualisieren ?
rawdata <- rawdata%>%
  filter(Klassen.Id != 44)

## Sehr kleine Kurse löschen möglicherweise enbtstanden durch falsches klassen.ID abschreiben der SuS
rawdata <- rawdata%>%
  group_by(Klassen.Id)%>%
  mutate(length_Klasse = n())%>%
  ungroup()%>%
  filter(length_Klasse >3)


## kursdata wrangling  ##########################################################################################

## Kursdata_ink erstellen: Der folgende Abschnitt erstellt Skalenwerte, macht grand mean, group mean centering
# Factor scores bilden
scale_this <- function(x){                                ## wegen Problemen von `scale()` in `mutate()`
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

library(tidyr)
kursdata_ink <- rawdata%>%
  select(starts_with("le"), starts_with("en"), starts_with("or"), starts_with("ci"), starts_with("ir"),
         Kursort, Kursdatum, Uhrzeit, Username, Klassen.Id)%>%
  mutate(kursleiterin             = Username,
         Kursbeginn               = Kursdatum,
         kurs                     = Klassen.Id,
         fortlID                  = 1:n(),
         Lernerfolg               = rowMeans(data.frame(le1,le2,le3,le4), na.rm = T),
         Enthusiasmus             = rowMeans(data.frame(en1,en2,en3,en4), na.rm = T),
         Gruppeninteraktion       = rowMeans(data.frame(ci1,ci2,ci3,ci4), na.rm = T),
        `Individuelle Beziehung`  = rowMeans(data.frame(ir1,ir2,ir3), na.rm = T),
         Organisation             = rowMeans(data.frame(or1,or2,or3,or4,or5), na.rm = T))%>%
  select(-num_range("le", 1:4), -num_range("en", 1:4), -num_range("ci", 1:4), -num_range("ir", 1:3), -num_range("or", 1:5))%>%  
  gather(variable, value, Lernerfolg, Enthusiasmus, Gruppeninteraktion, `Individuelle Beziehung`, Organisation)%>%
  group_by(variable)%>%
  mutate(value.std = scale_this(value))%>%
  ungroup()%>%
  group_by(fortlID)%>%
  mutate(value.pstd = scale_this(value))%>%
  ungroup()%>%
  filter(is.na(Username)==F)


# Interne Validierungen:  

### 
klassen_length <- rawdata_inkrement%>%
  group_by(Klassen.Id)%>%
  mutate(length_Klasse = n())

klassen_length %>%
  filter(length_Klasse == 4)%>%
  View()


summary(kursdata_ink)

kursdata_ink%>%
  group_by(fortlID)%>%
  summarize(shouldbezero = mean(value.pstd))

kursdata_ink%>%
  group_by(variable)%>%
  summarize(shouldbezero = mean(value.std, na.rm = T))

levels(as.factor(kursdata_ink$variable))


######## Writing Zone kursdata ###############################################################################################
### Schreiben kursdata_ink
# write.table(kursdata_ink, file = "data/data_dynamic/kursdata_inkrementiert.csv", sep = ";", row.names = F, quote = F) 
##########################################################################################################################



######## Dynamisch inkrementelles Freitext data generieren ###############################################################
freitextdata_ink <- tbl_df(full_join(rawdata_inkrement_raw,                     ## Hier wird es nur inkremente geben, da 
                                     kn_kl_key, by = "Klassen.Id"))%>%          ## Freitexte von vor Herbst '16 nicht angezeigt 
                    mutate(kursleiterin = as.factor(Username),                             ## werden.
                           score = rowMeans(data.frame(le1,le2,le3,le4,
                                                       en1,en2,en3,en4,
                                                       ci1,ci2,ci3,ci4,
                                                       ir1,ir2,ir3,
                                                       or1,or2,or3,or4,or5), na.rm = T),
                           ftk = as.factor(paste("rawdata_charge5/freitextbilder/", ftk, sep = "")))%>%                    
                    select(ftk, kursleiterin, score)
                    
######## Writing Zone  freitextdat  ######################################################################################
#   Schreiben freitextdata_ink
#   write.table(freitextdata_ink, file = "data/data_dynamic/freitextdata_inkrementiert.csv", sep = ";", row.names = F, quote = F) 
##########################################################################################################################



## likertdata wrangling ##################################################################################################
library(forcats)
likertdata_ink <- rawdata_inkrement%>%
  tbl_df()%>%
  mutate(kursleiterin = Username,
         Kursbeginn = Kursstart)%>%
  select(num_range("le", 1:4), num_range("en", 1:4), num_range("ci", 1:4), 
         num_range("ir", 1:3), num_range("or", 1:5),
         Kursbeginn, Kursort, Uhrzeit, kursleiterin)%>%
  gather(variable, value, num_range("le", 1:4), num_range("en", 1:4), num_range("ci", 1:4), 
                          num_range("ir", 1:3), num_range("or", 1:5))%>%
  mutate(value = as.factor(value),
         Kursort = as.factor(Kursort),
         value = fct_recode(value,
                            "7 = trifft vollständig zu"     = "7",
                            "1 = trifft überhaupt nicht zu" = "1"),
         variable = fct_recode(variable, 
                               "Du hast im Kurs etwas Nützliches gelernt." = "le1",
                               "Dein Interesse am Mathematik-Abistoff ist durch den Kurs gestiegen." = "le2",
                               "Du hast die Inhalte des Kurses verstanden."= "le3",
                               "Du fandest den Kurs herausfordernd und wurdest zum Denken angeregt." = "le4",
                               
                               "Der/die Kursleiter/in unterrichtet mit Begeisterung." = "en1",
                               "Der/die Kursleiter/in ist dynamisch und unterrichtet voller Energie."= "en2",
                               "Der/die Kursleiter/in lockert den Unterricht durch Humor auf." = "en3",
                               "Der/die Kursleiter/in hält Dein Interesse während des Kurses durch seinen/ihren Unterrichtsstil aufrecht." = "en4",
                               
                               "Die Erklärungen des Kursleiters/ der Kursleiterin sind verständlich."= "or1",
                               "Der/die Kursleiter/in ist gut vorbereitet und erklärt die Inhalte sorgfältig." = "or2",
                               "Du hast im Kurs einen Überblick über alle Abi-relevanten Themen erhalten." = "or3",
                               "Du hast im Kurs die Bearbeitung Abi-relevanter Aufgabentypen geübt." = "or4",
                               "Du hast durch den Kurs Wissenslücken schließen können."= "or5",
                               

                               "Der/die Kursleiter/in ermutigt die Teilnehmenden, an den Diskussionen im Kurs teilzunehmen." = "ci1",
                               "Die Kursteilnehmer/innen werden eingeladen, eigene Ideen und Lösungswege mitzuteilen." = "ci2",
                               "Die Kursteilnehmer/innen werden ermutigt, Fragen zu stellen."= "ci3",
                               "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen." = "ci4",

                               "Der/die Kursleiter/in ist den Teilnehmenden gegenüber stets freundlich." = "ir1",
                               "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl, jederzeit um Hilfe bitten zu können." = "ir2",
                               "Der/die Kursleiter/in interessiert sich aufrichtig für die Teilnehmenden." = "ir3"))
    
  
  
######## Writing Zone  likertdata  ######################################################################################
#   Schreiben likertdata_ink
#   write.table(likertdata_ink, file = "data/data_dynamic/likertdata_inkrementiert.csv", sep = ";", row.names = F, quote = F) 
##########################################################################################################################



         
##################################################################################
### Jetzt wirds Ernschd
##################################################################################

### ACHTUNG: Datei für bestehende und für aktuelle Passwörter ändern!




## Import Export KL PW ANschreiben-.csv ##################################################################################

######## Inkremetierung der Passwörter

# Import der bereits existierenden KL 
data_pw_bestehend <- read.table("data/data_kl/data_pw_inkrementiert_charge5.csv", sep = ";", header = T)      ### Aktualisieren siehe DANGERZOOOOONE!
data_pw_bestehend$Datum <- as.Date(data_pw_bestehend$Datum)                                     #                      °°°°°°°°°°°°°°°°°

# Filterung des Inkrements (= "neue" KL) und PW-Genese (für diese)
library(random)
library(lubridate)
data_pw_inkrement <- rawdata_inkrement%>%
  select(Personalnummer, Kursstart)%>%
  unique()%>%
  filter(is.na(Personalnummer) == F, !Personalnummer %in% data_pw_bestehend$Personalnummer)%>%
  mutate(Passwort = as.character(randomStrings(n=n(), len=5, digits=TRUE, upperalpha=TRUE, loweralpha=TRUE, unique=TRUE)),
         Kursstart = lubridate::ymd(Kursstart))%>%
  filter(Kursstart < lubridate::as_date("2016-10-30"))                 ### Aktualisieren 



##### Ab jetzt geht es um den Mailversand                                                    
## Filtere neue Kurse seit letzten Mailversand = die, die noch Versendedatum brauchen

letztes_Versendedatum <-  ymd("2016-10-01")                            ### Aktualisieren 

datum_neue_charge <- rawdata_inkrement%>%
  select(Kursdatum, Personalnummer)%>%
  filter(Kursdatum > letztes_Versendedatum)%>%
  unique()



## Schleife für randomisiertes Versendedatum
 datum_inkrement <- c(rep(today(),nrow(datum_neue_charge)))   #initialize

for (i in 1:nrow(datum_neue_charge)) {
 datum_inkrement[i] <- datum_inkrement[i] + 
            lubridate::days(x=1) +
            lubridate::weeks(x = as.numeric(randomNumbers(n = 1, min = 0, max = 2, col = 1)))
}

datum_neue_charge$Datum <- datum_inkrement


## Hier fehlt noch der Join ....

data_pw_inkrement <- data_pw_inkrement%>%
  select(-Kursstart)



  

# Passwörter verschlüsseln und encrypt datei erstellen
pw_scrypted_inkrementiert <- left_join(select(data_pw_inkrementiert, Personalnummer, Passwort),
                                      unique(select(kn_kl_key, Personalnummer, Username)), by = "Personalnummer" )

for (i in 1:nrow(pw_scrypted_inkrementiert)){
pw_scrypted_inkrementiert$Passwort_scrypted[i] <-  scrypt::hashPassword(pw_scrypted_inkrementiert$Passwort[i])
}

pw_scrypted_inkrementiert <- pw_scrypted_inkrementiert%>%
  mutate(Login = Username)%>%
  select(Login, Passwort_scrypted)

#############  Danger Zone  ################################################################################################################################
#######                                                                                                                                                  
####### write.table(data_pw_inkrementiert, file = paste("data/data_kl/data_pw_inkrementiert", as.character(Sys.time()), ".csv", sep = ""),               
#######                                    sep = ";", row.names = F, quote = F)     
#######
####### write.table(data_pw_inkrementiert, file = ## "data/data_kl/data_pw_inkrementiert_charge5.csv", sep = ";", row.names = F, quote = F)                      
#######                                                                                                                                                  
#######                                                   
#######                                                                                                                                                  
####### write.table(pw_scrypted_inkrementiert, file = paste("data/data_kl/data_pw_scrypted", as.character(Sys.time()), ".csv", sep = ""),                
#######                                    sep = ";", row.names = F, quote = F)                                                         
#######                     
####### write.table(pw_scrypted_inkrementiert, file = "data/data_kl/data_pw_scrypted.csv", sep = ";", row.names = F, quote = F)  
############################################################################################################################################################
