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
plot(data, type = "l", xlab = "Periode", ylab = "Indice non différencié", col = "royalblue3")
plot(diff_data, type = "l", xlab = "Periode", ylab = "Indice différencié", col = "royalblue3")




### PARTIE 2 : MODELES ARIMA

# on étudie les autocorrélogrammes pour déterminer les ordres p et q possibles 

par(mfrow = c(1, 1))

# autocorrélations
acf(diff_data)
# les auto-corrélations 0, 1, 2 sortent clairement de la zone de confiance
# la 3 presque
# => on propose q_max = 3
qmax <- 3

#autocorrélations partielles
pacf(diff_data)
# les auto-corrélations partielles 1, 2, 3 sortent clairement de la zone de confiance
# => on propose p_max = 3
pmax <- 3

# on regarde si les modèles sont valides (i.e. les résidus sont des bruits blancs)
# et s'ils sont ajustés (i.e. les coefficients les plus élevés sont significatifs)
# pour ce faire on crée des fonctions :
valid_model <- function(estim, kmax, fitdf, threshold){ # dit si les résidus sont blancs selon le test de Ljung-Box 
  R <- 1
  for (i in (fitdf+1):kmax){
    r <- if (Qtests(estim$residuals,kmax,fitdf=fitdf)[i,2] < threshold) 0 else 1
    R = R*r
  }
  output <- if (R == 0) FALSE else TRUE
  return(output)
}

signif <- function(estim){  # réalise des t-tests sur les coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

ajusted_model <- function(estim, p, q){ # dit si le modèle est ajusté ou non 
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
validity <- mat # matrice des validités vide
ajustment <- mat # matrice des ajustements vide
pqs <- expand.grid(0:pmax,0:qmax) # toutes les combinaisons possibles de p et q 
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #recup p
  q <- pqs[row,2] #recup q
  estim <- try(arima(diff_data, c(p,0,q), include.mean = F)) #tente d'estimer l'ARIMA(p,0,q)
  validity[p+1,q+1] <- valid_model(estim, 24, p+q, 0.01)  #assigne la validité 
  ajustment[p+1,q+1] <- if ((p == 0) & (q == 0)) NA else ajusted_model(estim, p, q)  #assigne l'ajustement
}
# matrice des validités
validity
# on constate qu'aucun modèle n'est valide 
# => il faut élargir le nombre de modèles étudiés, en augmentant p_max et q_max


# autocorrélations
acf(diff_data)
# on constate que l'acf d'ordre 6 est nettement hors de l'intervalle de confiance
qmax <- 6

#autocorrélations partielles
pacf(diff_data)
# idem, la pacf d'ordre 6 est nettement hors de l'intervalle de confiance
pmax <- 6

mat <- matrix(NA,nrow = pmax + 1, ncol = qmax + 1) # structure de matrice vide 
rownames(mat) <- paste0("p=",0:pmax) # renomme les lignes
colnames(mat) <- paste0("q=",0:qmax) # renomme les colonnes
validity <- mat # matrice des validités vide
ajustment <- mat # matrice des ajustements vide
pqs <- expand.grid(0:pmax,0:qmax) # toutes les combinaisons possibles de p et q 
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #recup p
  q <- pqs[row,2] #recup q
  estim <- try(arima(diff_data, c(p,0,q), include.mean = F)) #tente d'estimer l'ARIMA(p,0,q)
  validity[p+1,q+1] <- valid_model(estim, 24, p+q, 0.01)  #assigne la validité 
  ajustment[p+1,q+1] <- if ((p == 0) & (q == 0)) NA else ajusted_model(estim, p, q)  #assigne l'ajustement
}
# matrice des validités
validity

# matrice des ajustements 
ajustment

# => les couples (p,q) pour lesquels on a un modèle ajusté et valide sont : 
# (6,0) ; (3,4) ; (4,5) 

# on peut maintenant comparer ces modèles entre eux à l'aide des critères d'information
arima600 <- arima(diff_data, c(6,0,0), include.mean = F)
arima304 <- arima(diff_data, c(3,0,4), include.mean = F)
arima405 <- arima(diff_data, c(4,0,5), include.mean = F)
models <- c("arima600","arima304","arima405")
names(models) <- models
apply(as.matrix(models),1, function(m) c("AIC"=AIC(get(m)), "BIC"=BIC(get(m))))
# l'AIC est minimisé par l'ARIMA(3,0,4)
# le BIC est minimisé par l'ARIMA(3,0,4) aussi

## CONCLUSION : l'ARIMA(3,0,4) est le meilleur modèle étudié pour la série diff_data. 

# pour la série data, il s'agit donc d'un ARIMA(3,1,4)
arima314 <- arima(data, c(3,1,4), include.mean = F)
arima314
