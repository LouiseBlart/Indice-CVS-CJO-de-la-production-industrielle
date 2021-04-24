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

### PARTIE 1 : LES DONNEES

# On identifie un répertoire de travail 
setwd(dir="C:/Users/jeann/OneDrive/Documents/scolaire/ENSAE/2A/S2/séries temporelles/projet/git")
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

# on constate que la série n'a pas l'air très stationnaire. 
# Deux tendances semblent se distinguer : de 1990 à 2000 puis de 200 à 2020. 

# on différencie la série (à l'ordre 1) pour tenter de la rendre stationnaire : 
data <- zoo(data['Indice'], order.by = Periode) 
diff_data <- diff(data, 1)
plot(diff_data, type = "l", xlab = "Periode", ylab = "Indice différencié", col = "royalblue3")

plot(cbind(data, diff_data))

summary(lm(data~Periode))

     