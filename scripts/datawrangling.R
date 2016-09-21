###################################################
# Datawrangling script abiturma <-> feedbackapp   #
###################################################



## Import und Merging FB Data ##################################################################################

## Import des Kursnummer/Kursleiterschlüssels
kn_kl_key <-read.csv(url('https://samuel.merk%40uni-tuebingen.de:afbogena@www.abiturma.de/dornier/api/daten-fragebogen'), encoding="UTF-8")

## Import der Rohdaten aus dem Frühjahr 2016
rawdata_fr16 <- data.table::fread("data/data_raw/rawdata_fr16_utf8.csv", sep = ";", na.strings = "NA")

## Import Inkrement
rawdata_inkrement <- data.table::fread("rawdata_charge5/daten.csv", sep = ";")

## Match Inkrement + kn_kn_key 
rawdata_inkrement$Klassen.Id <- rawdata_inkrement$ID
rawdata_inkrement$em2 <- NULL ## zu ändern im Questor-Codeplan
rawdata_inkrement$em2 <- NULL ## zu ändern im Questor-Codeplan
rawdata_inkrement <- dplyr::left_join(rawdata_inkrement, kn_kl_key, by = "Klassen.Id")


## Vorbereitung Kursdatum
rawdata_fr16$Kursdatum <- lubridate::dmy(rawdata_fr16$Kursbeginn)
rawdata_inkrement$Kursdatum <- lubridate::dmy(rawdata_inkrement$Kursstart)

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



## Data wrangling für Passung auf bisherige App ##################################################################################

## Kursdata_ink erstellen
# Factor scores bilden
scale_this <- function(x){
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
summary(kursdata_ink)

kursdata_ink%>%
  group_by(fortlID)%>%
  summarize(shouldbezero = mean(value.pstd))

kursdata_ink%>%
  group_by(variable)%>%
  summarize(shouldbezero = mean(value.std, na.rm = T))

levels(as.factor(kursdata_ink$variable))


######## Writing Zone   ##################################################################################################
### Schreiben kursdata_ink
# write.table(kursdata_ink, file = "data/data_dynamic/kursdata_inkrementiert.csv", sep = ";", row.names = F, quote = F) 
##########################################################################################################################



## Import Export KL PW ANschreiben-.csv ##################################################################################

######## Inkremetierung der Passwörter

# Import der bereits existierenden KL 
data_pw_bestehend <- read.table("data/data_kl/data_pw.csv", sep = ";", header = T)
data_pw_bestehend$Datum <- as.Date(data_pw_bestehend$Datum)

# Filterung des Inkrements (= "neue" KL) und PW-Genese
library(random)
library(lubridate)
data_pw_inkrement <- rawdata_inkrement%>%
  select(Personalnummer, Kursdatum)%>%
  unique()%>%
  filter(is.na(Personalnummer) == F, !Personalnummer %in% data_pw_bestehend$Personalnummer)%>%
  mutate(Passwort = as.character(randomStrings(n=n(), len=5, digits=TRUE, upperalpha=TRUE, loweralpha=TRUE, unique=TRUE)),
         Kursdatum = lubridate::ymd(Kursdatum))%>%
  filter(Kursdatum < lubridate::today())

## Schleife für randomisiertes Versendedatum
for (i in 1:nrow(data_pw_inkrement)) {
  datum_inkrement <- c(rep(today(),nrow(data_pw_inkrement))) #initialize
  datum_inkrement[i] <- data_pw_inkrement$Kursdatum[i] + 
            lubridate::days(x=4) +
            lubridate::weeks(x = as.numeric(randomNumbers(n = 1, min = 1, max = 3, col = 1)))
}

data_pw_inkrement$Datum <- datum_inkrement

data_pw_inkrement <- data_pw_inkrement%>%
  select(-Kursdatum)


# Join Inkrement und Bestehend
data_pw_inkrementiert <- full_join(data_pw_bestehend, data_pw_inkrement)%>%
  select(Datum, Personalnummer, Passwort)
  

# Passwörter verschlüsseln und encrypt datei erstellen
pw_scrypted_inkrementiert <- left_join(unique(select(data_pw_inkrementiert, Personalnummer, Passwort)),
                                      unique(select(kn_kl_key, Personalnummer, Username)))

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
####### write.table(data_pw_inkrementiert, file = ## "data/data_kl/data_pw_inkrementiert.csv", sep = ";", row.names = F, quote = F)                      
#######                                                                                                                                                  
#######                                                   
#######                                                                                                                                                  
####### write.table(pw_scrypted_inkrementiert, file = paste("data/data_kl/data_pw_scrypted", as.character(Sys.time()), ".csv", sep = ""),                
#######                                    sep = ";", row.names = F, quote = F)                                                         
#######                     
####### write.table(pw_scrypted_inkrementiert, file = "data/data_kl/data_pw_scrypted.csv", sep = ";", row.names = F, quote = F)  
############################################################################################################################################################

test <- data.table::fread("data/data_kl/data_pw_scrypted2016-09-21 05:27:36.csv", sep = ";", na.strings = "NA")
