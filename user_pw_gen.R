library(scrypt)
library(feather)
kl_pw <- read.csv("/srv/shiny-server/demo_abiturma/Stuff/kl_pw.csv")

for (i in 1:nrow(kl_pw)) {
kl_pw$Passwort_scrypted[i] <- hashPassword(as.character(kl_pw$Passwort[i]))
}

verifyPassword(kl_pw[kl_pw$Login == "demouser1","Passwort_scrypted"], "demopassword1")

kl_pw$Passwort <- NULL

write_feather(kl_pw, "Stuff/kl_pw.feather")
