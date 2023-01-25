#Elias Chavarria-Mora
#Twitter paper for CIEP
#Code for obtaining tweets of electoral debate and analysis

library(academictwitteR)
library (tidyverse)
library (tidytext)
library (lubridate) #to be able to manipulate time
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/TuitiIdeology/LIWCData")

bearer_token <- "XXX" #Here goes the bearer token, not included here because it's a password

#Repretel
DebateRepretel<-get_all_tweets(query = 'Eliecer Feinzaig OR EliÃ©cer Feinzaig OR Eli Feinzaig OR Fabricio Alvarado OR Lineth SaborÃ­o OR Lineth Saborio OR Welmer Ramos OR Rodrigo Chaves OR JosÃ© MarÃ­a Figueres OR Jose Maria Figueres OR JosÃ© MarÃ­a Villalta OR Jose Maria Villalta OR #decision2022 OR #elecciones2022 OR #DebateRepretel', 
                               start_tweets = "2022-02-03T02:00:00Z", 
                               end_tweets = "2022-02-03T08:00:00Z",
                               bearer_token = bearer_token,
                               data_path = "DebateRepretel",
                               bind_tweets = F,
                               n= Inf)
df_DebateRepretel <- bind_tweets(data_path = "DebateRepretel", output_format = "tidy")
write.csv(df_DebateRepretel, 'df_DebateRepretel.csv')

#Teletica
DebateTeletica<-get_all_tweets(query = 'Eliecer Feinzaig OR EliÃ©cer Feinzaig OR Eli Feinzaig OR Fabricio Alvarado OR Lineth SaborÃ­o OR Lineth Saborio OR Welmer Ramos OR Rodrigo Chaves OR JosÃ© MarÃ­a Figueres OR Jose Maria Figueres OR JosÃ© MarÃ­a Villalta OR Jose Maria Villalta OR #decision2022 OR #elecciones2022 OR #DebateTeletica', 
                       start_tweets = "2022-02-04T01:00:00Z", 
                       end_tweets = "2022-02-04T08:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "DebateTeletica",
                       bind_tweets = F,
                       n= Inf)
df_DebateTeletica <- bind_tweets(data_path = "DebateTeletica", output_format = "tidy")
write.csv(df_DebateTeletica, 'df_DebateTeletica.csv')


#ok, tengo un problema donde el created_at se despicho, pero si estan en los tweets originales, so:
df_DebateRepretel <-read.csv('df_DebateRepretel.csv')
df_DebateTeletica <-read.csv('df_DebateTeletica.csv')

#ojo, 479 de repretel, 4016 de teletica

df_DebateRepretel<- df_DebateRepretel %>%
  mutate (Channel = "Repretel",
          .after=sourcetweet_author_id)

df_DebateTeletica<- df_DebateTeletica %>%
  mutate (Channel = "Teletica",
          .after=sourcetweet_author_id)

df<-rbind(df_DebateRepretel, df_DebateTeletica) #no problem with the dates here

#Clean the text data in spanish
df<-df %>%
  rename ("V1"="X") 
df$clean_text<-df$text
df$clean_text<-str_to_lower(df$clean_text) #get rid of upper case
df$clean_text<-str_replace_all(df$clean_text, "[\r\n]", "") #remove new line indicators 
df$clean_text<-str_replace_all(df$clean_text, "https", "")  #remove https, etc
df$clean_text<-str_replace_all(df$clean_text, "rt", "")
df$clean_text<-str_replace_all(df$clean_text, "t.co", "")
df$clean_text<-str_replace_all(df$clean_text, "amp", "")
write.csv(df, 'df_full.csv', fileEncoding="UTF-8")


#bring in the df_full_LIWC
df <-read.csv('df_fullLIWC.csv', fileEncoding="UTF-8")
#Cleanup normal
df =select(df, -1)
#df$created_at <- as.Date(df$created_at) #DO not use as.Date here, looses the original info and leaves only the day

#Analytic
df<-df%>%
  mutate(Analytic=30+Articulo+Prepos-PronPer-PronImp-VerbAux-Conjunc-Adverb-Negacio)
#Authenticity
df<-df%>%
  mutate(Authenticity=Yo+Insight+Excl+Relativ-Discrep-ElElla)

#Sentiment??

#sidenote, todo esto deberia de poder hacerlo con maps de purr en una sola linea de codigo, para practica luego
#Use regular expressions for identifying a smaller database that uses an specific keyword
#df$newobject <- grepl("[Tt][Rr][Uu][Mm][Pp]", df$text) #example for Trump, for every word in upper and lower case
#basically, one per candidate or party
df$feinzaig<-grepl("(eli|eli[eé]cer)|(feinzaig)|(eli|eli[eé]cer feinzaig)", df$clean_text) #This line creates a new column in the database identifying if the word appears in the 
#tweet or not, in this case the word is 'eliecer feinzaig', 'eliécer feinzaig', 'eli feinzaing'
df$fabricio<-grepl("(fabricio)|(alvarado)|(fabricio alvarado)", df$clean_text) #"fabricio alvarado
df$lineth<-grepl("(lineth)|(sabor[ií]o)|(lineth sabor[ií]o)", df$clean_text) #lineth saborio, lineth saborío
df$welmer<-grepl("(welmer)|(ramos)|(welmer ramos)", df$clean_text) #welmer ramos
df$chaves<-grepl("(rodrigo)|(chaves)|(rodrigo chaves)", df$clean_text) #rodrigo chaves
df$figueres<-grepl("(jos[ée] mar[íi]a figueres)|(figueres)", df$clean_text) #jose maría figueres, jose maría figueres, josé maría figueres, notese que captura figueresm pero NO jose maria
df$villalta<-grepl("(jos[ée] mar[íi]a villalta)|(villalta)", df$clean_text) #jose maría villalta, jose maria villalta

#I can create a categorical based on the dummies with an ifelse
df$candidate<-with(df, ifelse (feinzaig==T, "Feinzaig",
                               ifelse(fabricio==T, "Alvarado", 
                                      ifelse(lineth==T, "Saborío",
                                             ifelse(welmer==T, "Ramos",
                                                    ifelse(chaves==T, "Chaves",
                                                           ifelse(figueres==T, "Figueres",
                                                                  ifelse(villalta==T, "Villalta", "Multiple"))))))))

table (df$candidate)

#save the db again, this is the most complete version
write.csv(df, 'df_full_V2.csv', fileEncoding="UTF-8")

#if i need to bring it back
df <-read.csv('df_full_V2.csv', fileEncoding="UTF-8")
df =select(df, -1)

#Todo bien excepto que necesito agregar en, por ejemplo hora, no me sirve que salga por microsegundo
#No se puede usar asDate, el que sigue es lo correcto
class(df$created_at) #it says the class is character

df$created_at_asTime<-as.POSIXct(df$created_at, format="%Y-%m-%dT%H:%M:%OS")
class(df$created_at_asTime) #class is POSIXct


df_images_authenticity_repretel<-df%>%
  filter(Channel=="Repretel") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(Authenticity), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Authenticity"="name")  #rename two columns

df_images_analytic_repretel<-df%>%
  filter(Channel=="Repretel") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(Analytic), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Analytic"="name")  #rename two columns

df_images_authenticity_teletica<-df%>%
  filter(Channel=="Teletica") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(Authenticity), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Authenticity"="name")  #rename two columns

df_images_analytic_teletica<-df%>%
  filter(Channel=="Teletica") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(Analytic), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Analytic"="name")  #rename two columns




# make the graphics using facet_wrap

Authenticity_graphic_Repretel<-ggplot(data=df_images_authenticity_repretel,(aes(x=Hour, y=Authenticity,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Lenguaje auténtico")+
  xlab("")+
  theme(legend.position="none") 
Authenticity_graphic_Repretel

Analytic_graphic_Repretel<-ggplot(data=df_images_analytic_repretel,(aes(x=Hour, y=Analytic,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Lenguaje analítico")+
  xlab("")+
  theme(legend.position="none") 
Analytic_graphic_Repretel

Authenticity_graphic_Teletica<-ggplot(data=df_images_authenticity_teletica,(aes(x=Hour, y=Authenticity,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Lenguaje auténtico")+
  xlab("")+
  theme(legend.position="none") 
Authenticity_graphic_Teletica

Analytic_graphic_Teletica<-ggplot(data=df_images_analytic_teletica,(aes(x=Hour, y=Analytic,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Lenguaje analítico")+
  xlab("")+
  theme(legend.position="none") 
Analytic_graphic_Teletica

#Voy a meter analisis de sentimiento pq pq putas no, no?
df$tone<-df$EmoPos - df$EmoNeg +50 #es sentimiento pos - sentimient neg, el mas 50 es para que se mueva entre 0 y 100

df_images_tone_repretel<-df%>%
  filter(Channel=="Repretel") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(tone), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Tone"="name")

df_images_tone_teletica<-df%>%
  filter(Channel=="Teletica") %>% #so I only have the repretel tweets
  filter(candidate!="Multiple") %>% #so that I only have tweets that talk about an specific candidate
  group_by(lubridate::hour(created_at_asTime), candidate) %>% #groups by hour and candidate
  summarise_at(vars(tone), list(name=mean)) %>% #get the mean
  rename ("Hour"="lubridate::hour(created_at_asTime)",
          "Tone"="name")

Tone_graphic_Repretel<-ggplot(data=df_images_tone_repretel,(aes(x=Hour, y=Tone,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Sentimiento")+
  xlab("")+
  theme(legend.position="none") 
Tone_graphic_Repretel

Tone_graphic_Teletica<-ggplot(data=df_images_tone_teletica,(aes(x=Hour, y=Tone,group=candidate, color=candidate)))+ 
  geom_line(size=.5)+
  theme_minimal()+
  facet_wrap(~candidate)+
  theme(axis.text.x=
          element_blank())+ #makes it so axis x has no text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Sentimiento")+
  xlab("")+
  theme(legend.position="none") 
Tone_graphic_Teletica
