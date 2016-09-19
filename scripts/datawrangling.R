###################################################
# Datawrangling script abiturma <-> feedbackapp   #
###################################################

## Import des Kursnummer/Kursleiterschlüssels
kn_kl_key <-read.csv(url('https://samuel.merk%40uni-tuebingen.de:afbogena@www.abiturma.de/dornier/api/daten-fragebogen'), encoding="UTF-8")

## Import der Rohdaten aus dem Frühjahr 2016
rawdata_fr16 <- data.table::fread("data/data_raw/rawdata_fr16_utf8.csv", sep = ";", na.strings = "NA")

## Import Inkrement
rawdata_inkrement <- data.table::fread("rawdata_charge5/daten.csv", sep = ";")

## Match Inkrement + kn_kn_key 
rawdata_inkrement$Klassen.Id <- rawdata_inkrement$ID
rawdata_inkrement$em2 <- NULL ## zu ändern im Questor-Codeplan

rawdata_inkrement <- dplyr::left_join(rawdata_inkrement, kn_kl_key, by = "Klassen.Id")


## Vorbereitung Kursdatum
rawdata_fr16$Kursdatum <- lubridate::dmy(rawdata_fr16$Kursbeginn)
rawdata_inkrement$Kursdatum <- lubridate::dmy(rawdata_inkrement$Kursstart)

## Vorbereitung Kursort
rawdata_inkrement$Kursort <- gsub(" \\(.*", "", rawdata_inkrement$Kursbezeichnung)
rawdata_fr16$Kursort <- rawdata_fr16$Veranstaltungsort

## Match fr_16 und Inkrement Herbst'16
rawdata_fr16$Klassen.Id <- rawdata_fr16$ID








kl_pw     <- read.csv("/srv/shiny-server/demo_abiturma/data/kl_pw.csv")

