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
require(fUnitRoots)

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

# la série différenciée a l'air stationnaire. 

# pour vérifier ces impressions, on réalise des tests de racine unitaire ADF. 

# test ADF sur la série data
# régression pour savoir dans quel cas on se trouve : tendance et ou/ constante ? 
summary(lm(data~Periode))
# la constante et le coefficient de "Periode" sont significatifs
# => tendance avec constante

# détermination du lag k avec lequel effectuer le test ADF :

# fonction qui teste la blancheur d'une série (test de Ljung-Box) : 
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
# fonction qui détermine l'ordre auquel un test ADF est valide 
# i.e. effectue des tests ADF avec des lags croissants jusqu'à obtenir des résidus non autocorrélés
adfTest_valid <- function(series,kmax,type){
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,kmax,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}
adf <- adfTest_valid(data, 24, 'ct')
# il faut donc 21 lags pour obtenir un test ADF valide
adf
# p-value grande 
# => série non stationnaire. 

# test ADF sur la série dff_data :

summary(lm(diff_data~Periode[-1]))
# les deux p-values sont grandes => coeff non significatifs
# => pas de tendance et pas de constante : type 'nc'
adf <- adfTest_valid(diff_data, 24, 'nc')
# il faut 17 lags pour obtenir un test ADF valide 
adf
# p-value petite 
# => série stationnaire. 

# On vérifie que le test de Perron-Philipps donne une résultat cohérent :  
pp.test(diff_data)
# p-value petite, donc hypothèse nulle de racine unitaire rejetée : cohérent

# idem avec le test KPSS : 
kpss.test(diff_data)
# p-value grande, donc hypothèse nulle de stationnarité non-rejetée : cohérent

# CONCLUSION : la série différenciée est bien stationnaire, youpi. 

par(mfrow = c(1, 2)) # dispose les graphiques sur 1 ligne
plot(data, type = "l", xlab = "Periode", ylab = "Indice différencié", col = "royalblue3")
plot(diff_data, type = "l", xlab = "Periode", ylab = "Indice différencié", col = "royalblue3")



