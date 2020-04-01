#Exemple de fichier GIT
# Presentation des équipes
library(tidyverse)
strFichier <- "sursaud-covid19-quotidien-2020-03-30-19h03-departement.csv"

dfCovid <- read.csv2(strFichier,sep = ",")
dfCovid$date_de_passage <- as.Date(dfCovid$date_de_passage)
dfAge <- data.frame(Age=c("0","A","B","C","D","E"),
                    Nom=c("Tous âges","moins de 15 ans","15-44 ans",
                          "45-64 ans","65-74 ans","75 et plus"))

dfCovid %>% filter(dep=="75",sursaud_cl_age_corona=="0") %>% arrange(date_de_passage) %>%
  mutate(cumTot=cumsum(nbre_pass_corona)) %>%
  ggplot(aes(x=date_de_passage,y=cumTot))+
  geom_point()+
  geom_line()
hCorona <- dfCovid %>% filter(dep=="75",sursaud_cl_age_corona=="0") %>% arrange(date_de_passage) %>%
  mutate(cumTot=cumsum(nbre_pass_corona_h)) 
hCorona$Type <- "H"
fCorona <- dfCovid %>% filter(dep=="75",sursaud_cl_age_corona=="0") %>% arrange(date_de_passage) %>%
  mutate(cumTot=cumsum(nbre_pass_corona_f)) 
fCorona$Type <- "F"
TotHP <-  dfCovid %>% filter(dep=="75",sursaud_cl_age_corona=="0") %>% arrange(date_de_passage) %>%
  mutate(cumTot=cumsum(nbre_pass_tot)) 
TotHP$Type <- "Hospitalisé"
rbind(hCorona,fCorona,TotHP)  %>%
  ggplot(aes(x=date_de_passage,y=cumTot, color=Type))+
  geom_point()+
  geom_line()
