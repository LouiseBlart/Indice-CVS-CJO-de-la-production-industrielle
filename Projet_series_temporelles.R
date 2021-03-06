# Astier Jeanne & Blart Louise 
# Projet de s�ries temporelles ( 2A - ENSAE)
# Mod�lisation ARIMA de l'indice de production industrielle

# On supprimer ce que R a acculum� en m�moire (pas obligatoire)
rm(list=ls()) 

# On importe les packages utiles pour ce projet :
#install.packages("zoo")
#install.packages("tseries")
# install.packages("fUnitRoots")
require(zoo)
require(tseries)
require(fUnitRoots)

### PARTIE 1 : LES DONNEES

# On identifie un r�pertoire de travail 
setwd(dir="C:/Users/louis/OneDrive/Documents/ENSAE/2A/series_temp/Projet")

# On importe les donn�es 
data <- read.csv("valeurs_mensuelles.csv",  sep = ";", dec = "." ,header = T, skip = 2)
# skip =2 : pour retirer les deux premi�res lignes qui sont une "description" des donn�es

# On simplifie les noms des colonnes
colnames(data) = c("Periode","Indice", "Codes"); data

# On retire la colonne "code" qui est la m�me pour toutes nos observations ('A')
data <- subset( data, select = -c(Codes) )


# On definit les formats :
# Formats actuels : 
sapply(data,class)

# On peut se fixer au 1e du mois pour �tre dans un format "Date"
Periode =as.Date(as.yearmon(data$Periode))
Indice <- as.numeric(data$Indice)
data <- data.frame(Periode, Indice)

# Ou on peut egalement :
Periode = as.yearmon(data$Periode,format="%m-%y")
data <- data.frame(Periode, Indice)
# A voir dans la suite ce qui sera le plus simple

# Visualisation de la base 
head(data)

# Bref aper�u 
summary(data)

# Plot des donn�es
plot(data$Periode, data$Indice, type = "l", xlab = "Periode",
     ylab = "Indice", col = "royalblue3")

# on constate que la s�rie n'a pas l'air tr�s stationnaire. 
# Deux tendances semblent se distinguer : de 1990 � 2000 puis de 200 � 2020. 

# on diff�rencie la s�rie (� l'ordre 1) pour tenter de la rendre stationnaire : 
data <- zoo(data['Indice'], order.by = Periode) 
diff_data <- diff(data, 1)
plot(diff_data, type = "l", xlab = "Periode", ylab = "Indice diff�renci�", col = "royalblue3")

# la s�rie diff�renci�e a l'air stationnaire. 

# pour v�rifier ces impressions, on r�alise des tests de racine unitaire ADF. 

# test ADF sur la s�rie data
# r�gression pour savoir dans quel cas on se trouve : tendance et ou/ constante ? 
summary(lm(data~Periode))
# la constante et le coefficient de "Periode" sont significatifs
# => tendance avec constante

# d�termination du lag k avec lequel effectuer le test ADF :

# fonction qui teste la blancheur d'une s�rie (test de Ljung-Box) : 
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
# fonction qui d�termine l'ordre auquel un test ADF est valide 
# i.e. effectue des tests ADF avec des lags croissants jusqu'� obtenir des r�sidus non autocorr�l�s
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
# => s�rie non stationnaire. 

# test ADF sur la s�rie dff_data :

summary(lm(diff_data~Periode[-1]))
# les deux p-values sont grandes => coeff non significatifs
# => pas de tendance et pas de constante : type 'nc'
adf <- adfTest_valid(diff_data, 24, 'nc')
# il faut 17 lags pour obtenir un test ADF valide 
adf
# p-value petite 
# => s�rie stationnaire. 

# On v�rifie que le test de Perron-Philipps donne une r�sultat coh�rent :  
pp.test(diff_data)
# p-value petite, donc hypoth�se nulle de racine unitaire rejet�e : coh�rent

# idem avec le test KPSS : 
kpss.test(diff_data)
# p-value grande, donc hypoth�se nulle de stationnarit� non-rejet�e : coh�rent

# CONCLUSION : la s�rie diff�renci�e est bien stationnaire, youpi. 

par(mfrow = c(1, 2)) # dispose les graphiques sur 1 ligne
plot(data, type = "l", xlab = "Periode", ylab = "Indice non diff�renci�", col = "royalblue3")
plot(diff_data, type = "l", xlab = "Periode", ylab = "Indice diff�renci�", col = "royalblue3")




### PARTIE 2 : MODELES ARIMA

# on �tudie les autocorr�logrammes pour d�terminer les ordres p et q possibles 

par(mfrow = c(1, 1))

# autocorr�lations
acf(diff_data)
# les auto-corr�lations 0, 1, 2 sortent clairement de la zone de confiance
# la 3 presque
# => on propose q_max = 3
qmax <- 3

#autocorr�lations partielles
pacf(diff_data)
# les auto-corr�lations partielles 1, 2, 3 sortent clairement de la zone de confiance
# => on propose p_max = 3
pmax <- 3

# on regarde si les mod�les sont valides (i.e. les r�sidus sont des bruits blancs)
# et s'ils sont ajust�s (i.e. les coefficients les plus �lev�s sont significatifs)
# pour ce faire on cr�e des fonctions :
valid_model <- function(estim, kmax, fitdf, threshold){ # dit si les r�sidus sont blancs selon le test de Ljung-Box 
  R <- 1
  for (i in (fitdf+1):kmax){
    r <- if (Qtests(estim$residuals,kmax,fitdf=fitdf)[i,2] < threshold) 0 else 1
    R = R*r
  }
  output <- if (R == 0) FALSE else TRUE
  return(output)
}

signif <- function(estim){  # r�alise des t-tests sur les coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

ajusted_model <- function(estim, p, q){ # dit si le mod�le est ajust� ou non 
  R <- 1
  r <- if (p == 0) 1 else (if (signif(estim)[3,p] > 0.05) 0 else 1)
  s <- if (q == 0) 1 else (if (signif(estim)[3,p+q] > 0.05) 0 else 1)
  R = R*r*s
  output <- if (R == 0) FALSE else TRUE
  return(output)
}


mat <- matrix(NA,nrow = pmax + 1, ncol = qmax + 1) # structure de matrice vide 
rownames(mat) <- paste0("p=",0:pmax) # renomme les lignes
colnames(mat) <- paste0("q=",0:qmax) # renomme les colonnes
validity <- mat # matrice des validit�s vide
ajustment <- mat # matrice des ajustements vide
pqs <- expand.grid(0:pmax,0:qmax) # toutes les combinaisons possibles de p et q 
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #recup p
  q <- pqs[row,2] #recup q
  estim <- try(arima(diff_data, c(p,0,q), include.mean = F)) #tente d'estimer l'ARIMA(p,0,q)
  validity[p+1,q+1] <- valid_model(estim, 24, p+q, 0.01)  #assigne la validit� 
  ajustment[p+1,q+1] <- if ((p == 0) & (q == 0)) NA else ajusted_model(estim, p, q)  #assigne l'ajustement
}
# matrice des validit�s
validity
# on constate qu'aucun mod�le n'est valide 
# => il faut �largir le nombre de mod�les �tudi�s, en augmentant p_max et q_max


# autocorr�lations
acf(diff_data)
# on constate que l'acf d'ordre 6 est nettement hors de l'intervalle de confiance
qmax <- 6

#autocorr�lations partielles
pacf(diff_data)
# idem, la pacf d'ordre 6 est nettement hors de l'intervalle de confiance
pmax <- 6

mat <- matrix(NA,nrow = pmax + 1, ncol = qmax + 1) # structure de matrice vide 
rownames(mat) <- paste0("p=",0:pmax) # renomme les lignes
colnames(mat) <- paste0("q=",0:qmax) # renomme les colonnes
validity <- mat # matrice des validit�s vide
ajustment <- mat # matrice des ajustements vide
pqs <- expand.grid(0:pmax,0:qmax) # toutes les combinaisons possibles de p et q 
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #recup p
  q <- pqs[row,2] #recup q
  estim <- try(arima(diff_data, c(p,0,q), include.mean = F)) #tente d'estimer l'ARIMA(p,0,q)
  validity[p+1,q+1] <- valid_model(estim, 24, p+q, 0.01)  #assigne la validit� 
  ajustment[p+1,q+1] <- if ((p == 0) & (q == 0)) NA else ajusted_model(estim, p, q)  #assigne l'ajustement
}
# matrice des validit�s
validity

# matrice des ajustements 
ajustment

# => les couples (p,q) pour lesquels on a un mod�le ajust� et valide sont : 
# (6,0) ; (3,4) ; (4,5) 

# on peut maintenant comparer ces mod�les entre eux � l'aide des crit�res d'information
arima600 <- arima(diff_data, c(6,0,0), include.mean = F)
arima304 <- arima(diff_data, c(3,0,4), include.mean = F)
arima405 <- arima(diff_data, c(4,0,5), include.mean = F)
models <- c("arima600","arima304","arima405")
names(models) <- models
apply(as.matrix(models),1, function(m) c("AIC"=AIC(get(m)), "BIC"=BIC(get(m))))
# l'AIC est minimis� par l'ARIMA(3,0,4)
# le BIC est minimis� par l'ARIMA(3,0,4) aussi

## CONCLUSION : l'ARIMA(3,0,4) est le meilleur mod�le �tudi� pour la s�rie diff_data. 

# pour la s�rie data, il s'agit donc d'un ARIMA(3,1,4)
arima314 <- arima(data, c(3,1,4), include.mean = F)
arima314

### PARTIE 3 : Previsions 
# install.packages("forecast")
require(forecast)

forecast (arima314, 2)
# Affichage
plot(forecast(arima314,2, level = c(95)), 
     sub = "Intervalle de confiance � 95%", 
     ylab = "Indice",
     main = "Pr�diction en T+1 et T+2",
     ) 