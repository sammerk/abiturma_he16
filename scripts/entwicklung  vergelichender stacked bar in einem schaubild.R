likertdata1 <- read.table("data/likertdata_fr16_2_demo.csv", sep = ";", header = T, na.strings = c("NA"))


items_orga <- c(
  "Die Erklärungen des Kursleiters/ der Kursleiterin sind verständlich.", #or1
  "Der/die Kursleiter/in ist gut vorbereitet und erklärt die Inhalte sorgfältig.", #or2
  "Du hast im Kurs einen Überblick über alle Abi-relevanten Themen erhalten.", #or3
  "Du hast im Kurs die Bearbeitung Abi-relevanter Aufgabentypen geübt."   , #or4
  "Du hast durch den Kurs Wissenslücken schließen können.") #or5


likertdata4 <- likertdata1%>%                          
     mutate(gmgroup = ifelse(kursleiterin == "demouser1", "meine Kurse", "abiturma rest"))%>%
     filter(variable %in% items_orga)

likertdata6 <- likertdata4%>%
  filter(is.na(value)==F)%>%
  group_by(value, gmgroup, variable)%>%
  summarize(Freq = n())%>%
  ungroup()%>%
  group_by(gmgroup, variable)%>%
  mutate(Freq_per = Freq/sum(Freq, na.rm = T)*100)

# Farbpalette
cbPalette <- c("#A51E37", "#D8516A", "#FF849D", "#F8F8F8", "#95C3DF", "#497793", "#002A46")



library(scales)
library(plotly)
library(stringr)

ggplot(likertdata6,aes(x = gmgroup, y = Freq_per, fill = value)) + 
  geom_bar(stat='identity') + coord_flip() + facet_wrap(~variable, ncol =1)

  ggplotly(ggplot(likertdata6,aes(x = gmgroup, y = Freq_per, fill = value)) + 
  geom_bar(stat='identity') + coord_flip() + facet_wrap(~variable, ncol =1))
  

