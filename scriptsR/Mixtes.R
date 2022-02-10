library(FactoMineR)
library(Factoshiny)
library(dplyr)
library(stringr)   
library(missMDA)
library(factoextra)


transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")

###----- PREPROCESING -------###

transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)

rownames(transactions) <- transactions$fullVisitorId

transactions <- transactions %>% filter(timeOnSite > 100)

transactions<-transactions[!(transactions$browser_name=="Opera" | transactions$browser_name=="MIUI Browser" | transactions$browser_name=="Iron"),]

transactions$language <- sapply(transactions$language, function(x) substr(x,1,2))

#Create Variable Area
transactions$sizeArea <- transactions$browser_width * transactions$browser_height


# Payment Methode Preprocesing
transform_paymentMethod <- function(x) {
  x_lower <- tolower(x)
  print(x_lower)
  if (str_detect(x_lower, "credit") ) {
    result <-"credit_card"
  } else if (str_detect(x_lower, "gift")) {
    result <-"gift"
  } else if (str_detect(x_lower, "paypal")) {
    result <-"paypal"
  }else {
    result <- "NA"
  }
  return(result)
}
transactions$paymentMethod <- sapply(transactions$paymentMethod, function(x) transform_paymentMethod(x))
transactions$paymentMethod 



#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#Create variable activity
transactions$hits_norm <- min_max_norm(transactions$hits)
transactions$pageviews_norm <- min_max_norm(transactions$pageviews)
transactions$timeOnSite_norm <- min_max_norm(transactions$timeOnSite)
head(transactions$timeOnSite_norm)
head(transactions$pageviews_norm)
head(transactions$timeOnSite_norm)
transactions$activity <- (transactions$hits_norm + transactions$pageviews_norm + transactions$timeOnSite_norm) / 3

transactions$newVisits <- ifelse(transactions$newVisits==1, "New user","Old user")
transactions$newVisits

transactions$pageviews <- NULL
transactions$timeOnSite <- NULL
transactions$hits <- NULL
transactions$fullVisitorId <- NULL
transactions$city <- NULL




#res <- Factoshiny(transactions)


res.famd <- FAMD(transactions,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var = c(transactions$activity,transactions$sizeArea ))
round(res.famd$eig, 3)

round(res.famd$eig, 3)

barplot(res.famd$eig[,1], las = 2, cex.names = .5)

ncp <- 29
D <- dist(res.famd$ind$coord[,1:ncp])#distance euclidienne entre observations
res.hclust  <-  hclust(D,method = "ward.D2")#CAH par méthode de Ward

barplot(sort(res.hclust$height,decreasing = TRUE)[1:15],
        names.arg = 1:15,
        xlab = "index",
        ylab = "hauteur de fusion")

#kmeans
nbclasse <- 4
partition <-  cutree(res.hclust, k = nbclasse) #élagage de l'arbre

#Consolidation

centres.gravite <- by(res.famd$ind$coord[,1:ncp],
                      INDICES = partition,
                      FUN = colMeans) 

centres.gravite <- do.call(rbind, centres.gravite)#donne un objet de type "matrix", nécessaire pour pouvoir utiliser ces centres comme des valeurs initiales pour la fonction kmeans

res.kmeans <- kmeans(res.famd$ind$coord[,1:ncp],
                     centers = centres.gravite)

part.finale <- as.factor(res.kmeans$cluster)
table(part.finale)

transactions_part <- cbind.data.frame(transactions, classe = part.finale)
catdes(transactions_part, num = ncol(transactions_part))

res.famd <- FAMD(transactions_part,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var =  c(ncol(transactions_part),c(transactions$activity,transactions$sizeArea )))

res.ncp <- estim_ncpFAMD(transactions_part,
                         ncp.max = 10,
                         method.cv = "Kfold",
                         nbsim = 40)

plot(x = as.numeric(names(res.ncp$crit)),
     y = res.ncp$crit,
     xlab = "S",
     ylab = "Erreur",
     main = "Erreur de validation croisée\n en fonction du nombre d'axes",
     type = "b")

fviz_mfa_ind(res.famd, 
             habillage = "classe", # couleurs selon les modalités de la variable classe 
             palette = c("#000000B3", "#FF0000B3", "#00CD00B3","#0000FFB3","#FF00FFB3","#B8860BB3")# définition des couleurs
) 

res.dimdesc <- dimdesc(res.famd)
lapply(res.dimdesc$Dim.1, round, 3)

