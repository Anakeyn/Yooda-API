### Test de l'API Yooda Insight avec R
#### Chargement des bibliothèques utiles ##########################################
#Installer
#install.packages("httr") #à éxecuter une fois - déjà installé chez moi 
#install.packages("jsonlite") #une fois
#install.packages("stringr") #une fois
#install.packages("ggplot2") #une fois
#install.packages("tm") #une fois
#install.packages("wordcloud") #une fois
#install.packages("RColorBrewer") #une fois
#Charger
library(httr) #package utile pour récupérer des données sur le Web.
library(jsonlite) #pcakage pour travailler avec les données au format JSON
library(stringr)  #travail de chaine de caractères par exemple ici str_replace_all()
library(ggplot2) #Package de visualisation
library(tm) #Pour le text mining 
library(wordcloud) #Nuage de mots clés
library(RColorBrewer) #Palette de Couleurs
###################################################################################

##### Votre Clé d'API
MyAPIKey <- "xxxxxxxxxxxxxxxxxxxxxxxxx"  #ICI indiquez votre clé d'API fournie par Yooda.


#Verification de mes crédits 
MyApiYoodaURLCredits <- paste("https://api.yooda.com/subscriptions/credits?apikey=", MyAPIKey, sep ="")
MyApiYoodaURLCredits
responseCredits <- GET(url = MyApiYoodaURLCredits) #Attention le test de crédits mange 1 crédit :-(
httr::content(responseCredits)$content #Affiche les crédits restants   

###########################################################################################
######  I) Travail sur un Domaine 
###########################################################################################
MyURL <- "yooda.com"  #Domaine étudié 

##############################################
# Importation des données
##############################################

#Récupération de l'id du site 
MyApiYoodaURLId <- paste("https://api.yooda.com/insight/domains/", MyURL, "?apikey=", MyAPIKey, sep ="")
MyApiYoodaURLId  #Vérification de l'url
responseURL <- GET(url = MyApiYoodaURLId)  #Appel API Yooda.
MyURLId <- httr::content(responseURL)$content$id  # Recupération de l'Id de site 
MyURLId #Verification

#Récupération des mots clés:  #GET /insight/domains/{domain_id}/keywords 
MyApiYoodaURLKeywords  <- paste("https://api.yooda.com/insight/domains/", MyURLId, "/keywords?apikey=",  MyAPIKey, sep ="")
MyApiYoodaURLKeywords #Vérification de l'URL de l'API
responseKeywords <- GET(url = MyApiYoodaURLKeywords)  #Appel API Yooda
httr::content(responseKeywords) #Verif  
http_type(responseKeywords) #Verif type reçu
#Récupération des données à partir du format JSON de la réponse de l'API
DataKeywords  <- fromJSON(httr::content(responseKeywords, as = "text"), flatten=TRUE)$content$items_list
str(DataKeywords) #Vérifions que l'on a bien un Data Frame

#################################################################
# Préparation des données
#################################################################
#### Transformation des variables pour traitements ultérieurs ####################################################################
DataKeywords$position <- as.integer(DataKeywords$position)  #Dernière position connue du site sur cette recherche dans Google.fr
DataKeywords$traffic <- as.integer(DataKeywords$traffic) #Nombre de visiteurs mensuels sur le site à partir de cette recherche
DataKeywords$keyword.search_volume <- as.integer(DataKeywords$keyword.search_volume) #Nombre de recherches mensuelles sur ce mot clé.
DataKeywords$keyword.competition <- as.double(DataKeywords$keyword.competition) #Concurrence sur l'expression
DataKeywords$keyword.cpc <- as.double(DataKeywords$keyword.cpc) #Estimation du Cout par clic sur Adwords
DataKeywords$keyword.results_nb <- as.integer(DataKeywords$keyword.results_nb)  #nb de résultats dans Google.fr

##### Profitons en pour sauvegarder les données dans une fichier à la date du jour 
#peut resservir dans le futur pour voir les évolutions.
MyFileName <- paste(format(Sys.time(), "%Y-%m-%d"), "-", 
                    str_replace_all(MyURL, "\\.", "-"), ".csv", sep="")
write.csv2(DataKeywords, file = MyFileName) #Ecriture du fichier .csv avec séparateur ";"  

#################################################################
# Exploration des données
#################################################################
str(DataKeywords) #Structure de DataKeywords
utils::View(DataKeywords) #pour visualiser les données sous la forme d'un tableau 

#Isolons les Variables numériques pour voir ce qu'il y a dedans
DataKeywordsNum <- DataKeywords[,c(3,5,8:11)]
summary(DataKeywordsNum) #résumés statistiques des variables numériques.

#Tests d'Histogrammes
par(mfrow=c(2,3)) #divisions de la fenêtre graphique
hist(DataKeywords$position, main = "position", breaks = 10)
hist(DataKeywords$traffic, main = "traffic", breaks = 10) 
hist(DataKeywords$keyword.search_volume, main = "keyword.search_volume", breaks = 10)
hist(DataKeywords$keyword.competition, main = "keyword.competition", breaks = 10)
hist(DataKeywords$keyword.cpc, main = "keyword.cpc", breaks = 10)
hist(DataKeywords$keyword.results_nb, main = "keyword.results_nb", breaks = 10)
par(mfrow=c(1,1)) #Raz fenêtre graphique

#tests de Boites de dispersion
par(mfrow=c(2,3)) #divisions de la fenêtre graphique
boxplot(DataKeywords$position, main = "position")
boxplot(DataKeywords$traffic, main = "traffic")
boxplot(DataKeywords$keyword.search_volume, main = "keyword.search_volume")
boxplot(DataKeywords$keyword.competition, main = "keyword.competition")
boxplot(DataKeywords$keyword.cpc, main = "keyword.cpc")
boxplot(DataKeywords$keyword.results_n, bmain = "keyword.results_nb")
par(mfrow=c(1,1)) #Raz fenêtre graphique

#tests de nuages de points 
pairs(DataKeywordsNum) #Variables numériques en nuages de points 2 à 2 - pas très lisible ici 

#Réfléchissons un peu :-) 
#Je m'intéresse aux mots clés qui apportent du trafic : quelles sont leurs caractériques ???
par(mfrow=c(2,3)) #divisions de la fenêtre graphique
plot(x = DataKeywords$position , y= DataKeywords$traffic , 
     main = "Traffic vs position", 
     col = "red", lwd = 1, pch = 10, cex=3) #Traffic vs position
plot(x = DataKeywords$keyword.competition , y= DataKeywords$traffic , 
     main = "Traffic vs keyword.competition", 
     col = "red", lwd = 1, pch = 10, cex=3) #Traffic vs keyword.competition
plot(x = DataKeywords$keyword.cpc , y= DataKeywords$traffic , 
     main = "Traffic vs keyword.cpc", 
     col = "red", lwd = 1, pch = 10, cex=3) #Traffic vs keyword.cpc
plot(x = DataKeywords$keyword.results_nb , y= DataKeywords$traffic , 
     main = "Traffic vs keyword.results_nb", 
     col = "red", lwd = 1, pch = 10, cex=3) #Traffic vs keyword.results_nb
plot(x = DataKeywords$keyword.search_volume , y= DataKeywords$traffic , 
     main = "Traffic vs keyword.search_volume", 
     col = "red", lwd = 1, pch = 10, cex=3) #Traffic vs keyword.search_volume
par(mfrow=c(1,1))

###########################################################################
# Visualisation intéressante : Trafic en fonction du volume de recherche
###########################################################################
ggplot(DataKeywords, 
       aes(
         x = keyword.search_volume , 
         y = traffic, 
         col = as.factor(position), #10 positions
         size = keyword.competition
         )
       ) + geom_jitter() + #jittering pour voir les points cachés
  scale_x_log10() + #on met en log pour être plus lisible
  scale_y_log10() +
  stat_smooth(method = "lm", col = "#C42126", se = TRUE , size = 1) +
  theme_dark() +
ggtitle("Trafic Mots clés vs Volume de Recherche - Echelle Logarithmique")

#####################################################################################
#Récupération des indicateurs clés de performance on garde le même site MyURL
MyApiYoodaURLKPI  <- paste("https://api.yooda.com/insight/domains/", MyURLId, "/kpi?apikey=",  MyAPIKey, sep ="")
MyApiYoodaURLKPI 
responseKPI <- GET(url = MyApiYoodaURLKPI) 
httr::content(responseKPI)
http_type(responseKPI)  #verif
httr::content(responseKPI, as = "parsed" )$content #Verif
MyURLKPI <- fromJSON(httr::content(responseKPI, as = "text"), flatten=TRUE)$content #importation dans une Data Frame
MyURLKPI

###########################################################################################
######  II - Travail sur un Mot clé 
MyKeyword <- "SEO"   #au hasard :-)

##############################################
# II-1 Importation des données
##############################################

#Récupération de l'id du mot clé
MyApiYoodaKeywordId <- paste("https://api.yooda.com/insight/keywords/", MyKeyword,  "?apikey=", MyAPIKey, sep ="")
MyApiYoodaKeywordId
responseKeyword <- GET(url = MyApiYoodaKeywordId) 
MyKeywordId <- httr::content(responseKeyword)$content$kw_id  # Recupération de l'Id de keyword 
MyKeywordId #Verif


#Liste des expressions de recherche à partir d'un mot-clé
#GET /insight/keywords/market/by/keywords/{kw_id}
MyApiYoodaKeywordList <- paste("https://api.yooda.com/insight/keywords/market/by/keywords/",  MyKeywordId, "?apikey=", MyAPIKey, sep ="")
MyApiYoodaKeywordList
responseKeywordList <- GET(url = MyApiYoodaKeywordList) 
httr::content(responseKeywordList, as = "parsed")

DataKeywordList  <- fromJSON(httr::content(responseKeywordList, as = "text"), flatten=TRUE)$content$items_list
str(DataKeywordList) #Vérifions que l'on a bien un Data Frame
utils::View(DataKeywordList)

#################################################################
# II-2 Nettoyage des données
#################################################################
#Recupération des donnés dans une seule chaine de caractères
AllKeywords <- as.character(paste(DataKeywordList$keyword, collapse= " "))

AllKeywords <- chartr("éèëêÉÈËÊàÀçÇ", "eeeeEEEEaAcC", AllKeywords)  #on vire les accents 
AllKeywords_Corpus <- Corpus(VectorSource(AllKeywords)) #Transformation en Corpus pour autres nettoyages.

####!Fonction de Nettoyage du texte du Corpus. 
Clean_Corpus <- function(docs) {
# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("english"))
# Supprimer les mots vides français
docs <- tm_map(docs, removeWords, stopwords("french"))
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("seo", "yooda", "seeurank")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
#Regroupement optimisation/optimization
DoOptimization <- function(x) gsub("optimisation", "optimization", x)
docs <- tm_map(docs, content_transformer(DoOptimization))  
}
###### /Clean Corpus
AllKeywords_Corpus <- Clean_Corpus(AllKeywords_Corpus)
content(AllKeywords_Corpus) #visualisation du contenu du corpus "propre"

#################################################################
# II-3 Préparation des données au bon format  pour le Wordcloud
#################################################################
####Création de la matrice des mots et du jeu de données pour le nuage.
AllKeywords_dtm <- TermDocumentMatrix(AllKeywords_Corpus)  
AllKeywords_matrix <- as.matrix(AllKeywords_dtm) #récupération dans une matrice terms / Frequence dans Docs
str(AllKeywords_matrix) #Verif
# Sum rows 
AllKeywords_frequency <- rowSums(AllKeywords_matrix) #Fréquence par mot clé.
class(AllKeywords_frequency)
#sort by frequency
AllKeywords_frequency  <- sort(AllKeywords_frequency , decreasing=TRUE)   # Tri en foncton de la fréquence.
barplot(AllKeywords_frequency[1:10],  col = "tan", las = 2)  #visualisation des principaux mots clés.
AllKeywords_df <- data.frame(word = names(AllKeywords_frequency),freq=AllKeywords_frequency) #Transformation en Data Frame.
head(AllKeywords_df ,10) #vérifions

#################################################################
# II-4 - Visualisation du nuage de mots clés.
#################################################################
set.seed(1234)  #"fixation" de la randomization.
#Afffichage du nuage de mots clés.
wordcloud(words = AllKeywords_df$word, freq = AllKeywords_df$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
colors=brewer.pal(8, "Dark2"))

#########################################################################
#II-5 Récupérer les indicateurs clés de performance d'un mot-clé
#GET /insight/keywords/{kw_id}/market/kpi
#on garde "SEO"

MyApiYoodaKeywordKPI <- paste("https://api.yooda.com/insight/keywords/",   MyKeywordId, "/market/kpi?apikey=", MyAPIKey, sep ="")
MyApiYoodaKeywordKPI
responseKeywordKPI <- GET(url = MyApiYoodaKeywordKPI) 
httr::content(responseKeywordKPI)$content #Affichage
