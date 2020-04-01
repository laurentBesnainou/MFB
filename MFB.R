#Exemple de fichier GIT
# Presentation des équipes
library(tidyverse)
library(officer)
library(magrittr)
library(rvg)
library(lubridate)

#Chargement du fichier Offres Responsables
#JH Dev mBOX Chiffrage
df_mBox <- data.frame(Nature=c("Mineur","Lovys","Personetics","CR"),
                      MD=c(3649.5,560,860,958))
df_mBox$Pourcentage <- df_mBox$MD/sum(df_mBox$MD)  
g1 <- df_mBox %>% ggplot(aes(x = reorder(Nature,MD),y=MD,fill=Nature))+
  geom_col()+
  geom_text(aes(label=paste0(MD,"\n",scales::percent(Pourcentage))))+
  scale_fill_manual(values = c("#EEAD0E", "#BCEE68", "#1E90FF", "#8B475D", "#00868B"))+
  labs(title = "Man Days mBox",
       x=NULL,
       y="MD")+
  theme_light()+
  theme(legend.position = "none")


dblR42 <- 851.96
nbCR <- 413
RatioCT <- nbCR / dblR42
df_mBox$CT <- RatioCT * df_mBox$MD

g2 <- df_mBox %>% ggplot(aes(x = reorder(Nature,CT),y=CT,fill=Nature))+
  geom_col()+
  geom_text(aes(label=paste0(round(CT,0),"\n",scales::percent(Pourcentage))))+
  scale_fill_manual(values = c("#EEAD0E", "#BCEE68", "#1E90FF", "#8B475D", "#00868B"))+
  labs(title = "Test Cases from R4.2 ratio",
       subtitle = "Ratio de la MR4.2 : 413 cas de tests pour 851 MD mBox",
       x=NULL,
       y="Tests cases")+
  theme_light()+
  theme(legend.position = "none")

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(g1), type = "body") %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(g2), type = "body") %>% 
  print(target = "HMLV1.pptx") %>% 
  invisible()
dfPlaning <- data.frame(Projet=c("Mineur","Lovys","Personetics","CR","TNR"),
                        From=c(ymd("2020-05-17"),ymd("2020-05-18"),ymd("2020-05-18"),ymd("2020-04-06"),ymd("2020-05-25")),
                        To=c(ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10")))


g3 <- ggplot(dfPlaning, aes(x=From, xend=To, y=Projet, yend=Projet, color=Projet)) +
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=15) + #increase line width of segments in the chart
  geom_vline(xintercept = ymd("2020-06-10"),color="red",linetype="dashed")+
  geom_vline(xintercept = ymd("2020-06-19"),color="red",size=2)+
  
    labs(title='HML R4.4 Global Planning', x=NULL, y=NULL)+
  scale_x_date(date_breaks = "1 week",
               position = "top",
               date_labels = "%d/%m",
               limits = c(ymd("2020-03-30"),ymd("2020-07-10")))+
  scale_y_discrete(limits=c("TNR","Mineur","Lovys","Personetics","CR"))+
  scale_color_manual(values = c("#458B00", "#EEC900", "#9A32CD", "#1C86EE", "#8B2500"))+
  theme(legend.position = "none")
read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(g3), type = "body") %>% 
  print(target = "HMLGlobalPlanning.pptx") %>% 
  invisible()



dfPlaning <- data.frame(Projet=c("Core CR","Core Mineur","Core Lovys","Core Peronetics",
                                 "BPM CR","BPM Mineur","BPM Lovys","BPM Peronetics",
                                 "Mobile CR","Mobile Mineur","Mobile Lovys","Mobile Peronetics"),
                        From=c(ymd("2020-04-21"),ymd("2020-05-18"),ymd("2020-05-18"),ymd("2020-05-18"),
                               ymd("2020-04-21"),ymd("2020-05-18"),ymd("2020-05-18"),ymd("2020-05-18"),
                               ymd("2020-04-21"),ymd("2020-05-18"),ymd("2020-05-18"),ymd("2020-05-18")),
                        To=c(ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),
                             ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),
                             ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10"),ymd("2020-06-10")))
ggplot(dfPlaning, aes(x=From, xend=To, y=Projet, yend=Projet, color=Projet)) +
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=10) + #increase line width of segments in the chart
  geom_vline(xintercept = ymd("2020-06-10"),color="red",linetype="dashed")+
  geom_vline(xintercept = ymd("2020-06-19"),color="red",size=2)+
  
  labs(title='mBox Delivery Global Planning for R4.4 ', x=NULL, y=NULL)+
  scale_x_date(date_breaks = "1 week",
               position = "top",
               date_labels = "%d/%m",
               limits = c(ymd("2020-03-30"),ymd("2020-07-10")))+
  scale_y_discrete(limits=c("Mobile Mineur","BPM Mineur","Core Mineur",
                            "Mobile Lovys","BPM Lovys","Core Lovys",
                            "Mobile Peronetics","BPM Peronetics","Core Peronetics",
                            "Mobile CR","BPM CR","Core CR"))+
  scale_color_manual(values = c("Core CR"="#00008B", "BPM CR"="#1874CD", "Mobile CR"="#1E90FF", 
                                "Core Peronetics"="#00B2EE", "BPM Peronetics"="#8B2323", "Mobile Peronetics"="#CD2626",
                                "Core Lovys"="#EE0000", "BPM Lovys"="#FF82AB", "Mobile Lovys"="#2F4F4F",
                                "Core Mineur"="#458B00", "BPM Mineur"="#66CD00", "Mobile Mineur"="#A2CD5A"))+
  theme(legend.position = "none")
read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(g4), type = "body") %>% 
  print(target = "HMLGlobalPlanning.pptx") %>% 
  invisible()
