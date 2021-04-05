# Astier Jeanne & Blart Louise 
# Projet de séries temporelles ( 2A - ENSAE)
# Modélisation ARIMA de l'indice de production industrielle

# On supprimer ce que R a acculumé en mémoire (pas obligatoire)
rm(list=ls()) 

# On importe les packages utiles pour ce projet :
#install.packages("zoo")
#install.packages("tseries")
require(zoo)
require(tseries)

# On identifie un répertoire de travail 
setwd(dir="C:/Users/louis/OneDrive/Documents/ENSAE/2A/series_temp")
# On importe les données 
data <- read.csv("valeurs_mensuelles.csv",  sep = ";", dec = "." ,header = T, skip = 2)
# skip =2 : pour retirer les deux premières lignes qui sont une "description" des données

# On simplifie les noms des colonnes
colnames(data) = c("Periode","Indice", "Codes"); data

# On retire la colonne "code" qui est la même pour toutes nos observations ('A')
data <- subset( data, select = -c(Codes) )


# On definit les formats :
# Formats actuels : 
sapply(data,class)

# On peut se fixer au 1e du mois pour être dans un format "Date"
Periode =as.Date(as.yearmon(data$Periode))
Indice <- as.numeric(data$Indice)
data <- data.frame(Periode, Indice)

# Ou on peut egalement :
Periode = as.yearmon(data$Periode,format="%m-%y")
data <- data.frame(Periode, Indice)
# A voir dans la suite ce qui sera le plus simple

# Visualisation de la base 
head(data)

# Bref aperçu 
summary(data)

# Plot des données
plot(data$Periode, data$Indice, type = "l", xlab = "Periode",
     ylab = "Indice", col = "royalblue3")
