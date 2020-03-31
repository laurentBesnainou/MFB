#Exemple de fichier GIT
# Presentation des équipes
library(tidyverse)

#Chargement du fichier Offres Responsables
#JH Dev mBOX Chiffrage
df_mBox <- data.frame(Nature=c("Mineur","Lovys","Personetics","CR"),
                      MD=c(3649.5,560,860,958))
df_mBox$Pourcentage <- df_mBox$MD/sum(df_mBox$MD)  
df_mBox %>% ggplot(aes(x = reorder(Nature,MD),y=MD,fill=Nature))+
  geom_col()+
  geom_text(aes(label=paste0(MD,"\n",scales::percent(Pourcentage))))+
  scale_fill_manual(values = c("#EEAD0E", "#BCEE68", "#1E90FF", "#8B475D", "#00868B"))+
  labs(title = "Man Days mBox",
       x=NULL,
       y="MD")+
  theme_light()+
  theme(legend.position = "none")

