###################################################
# Datawrangling script abiturma <-> feedbackapp   #
###################################################

## Import des Kursnummer/Kursleiterschlüssels
kn_kl_key <-read.csv(url('https://samuel.merk%40uni-tuebingen.de:afbogena@www.abiturma.de/dornier/api/daten-fragebogen'), encoding="UTF-8")

## Import der 
kl_pw     <- read.csv("/srv/shiny-server/demo_abiturma/data/kl_pw.csv")

