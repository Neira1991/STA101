library(FactoMineR)
library(Factoshiny)
library(dplyr)
library (plyr)
library(stringr)   
library(missMDA)
library(factoextra)
library(VIM)
library(naniar)
library(ggplot2)
library(dendextend)

transactions <- read.csv(file = "/Users/stevenneira/Documents/Master/Cnam/STA101/Rapport/STA101/scriptsR/datasetClarinsTransactions.csv")
typeof(transactions)
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

var.illustratives <- c("browser_height","browser_width", "operatingSystem","hits_norm","pageviews_norm","timeOnSite_norm")

index_browser_height <- grep("browser_height", colnames(transactions))
index_browser_width <- grep("browser_width", colnames(transactions))
index_operatingSystem <-  grep("operatingSystem", colnames(transactions))
index_hits_norm <-  grep("hits_norm", colnames(transactions))
index_hits_norm
index_pageviews_norm <-  grep("pageviews_norm", colnames(transactions))
index_timeOnSite_norm <-  grep("timeOnSite_norm", colnames(transactions))
index_transactions <-  grep("transactions", colnames(transactions))

indexes_illustratives <- c(index_browser_height,
                           index_browser_width,
                           index_operatingSystem,
                           index_hits_norm,
                           index_pageviews_norm,
                           index_timeOnSite_norm,
                           index_transactions
)
transactions[-c(indexes_illustratives),]

res.famd <- FAMD(transactions,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var =indexes_illustratives)

round(res.famd$eig, 3)
barplot(res.famd$eig[,1], las = 2, cex.names = .5)

ncp <- 30
D <- dist(res.famd$ind$coord[,1:ncp])#distance euclidienne entre observations
res.hclust <-  hclust(D,method = "ward.D2")#CAH par méthode de Ward
res.hclust

plot(res.hclust, hang = -1)
ggplot(color_branches(res.hclust, k = 5), labels = FALSE)

inertie <- sort(res.hclust$height, decreasing = TRUE)
plot(inertie[1:10], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

plot(res.hclust, labels = FALSE, main = "Partition en 3, 4 ou 6 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(res.hclust, 3, border = "green3")
rect.hclust(res.hclust, 4, border = "red3")
rect.hclust(res.hclust, 6, border = "blue3")

points(c(3, 4, 6), inertie[c(3, 4, 6)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

barplot(sort(res.hclust$height,decreasing = TRUE)[1:10],
        names.arg = 1:10,
        xlab = "index",
        ylab = "hauteur de fusion")

nbclasse <- 3
partition <-  cutree(res.hclust, k = nbclasse) #élagage de l'arbre

#Consolidation

centres.gravite <- by(res.famd$ind$coord[,1:ncp],
                      INDICES = partition,
                      FUN = colMeans) 

centres.gravite <- do.call(rbind, centres.gravite)#donne un objet de type "matrix", nécessaire pour pouvoir utiliser ces centres comme des valeurs initiales pour la fonction kmeans

res.kmeans <- kmeans(res.famd$ind$coord[,1:ncp],
                     centers = centres.gravite)

res.kmeans$cluster

part.finale <- as.factor(res.kmeans$cluster)

table(part.finale)

transactions_part <- cbind.data.frame(transactions, classe = part.finale)
catdes(transactions_part, num = ncol(transactions_part))


res.ncp <- estim_ncpFAMD(transactions_part[,-indexes_illustratives],# on retire les variables illustratves qui ne sont pas gérées par la fonction (et inutile pour déterminer le nombre d'axes
                         method.cv = "Kfold",
                         nbsim = 40 #augmenter ce nombre améliore la précision des résultats, mais aussi le temps de calcul
                         
)

#res <- Factoshiny(transactions_part)



plot(x = as.numeric(names(res.ncp$crit)),
     y = res.ncp$crit,
     xlab = "S",
     ylab = "Erreur",
     main = "Erreur de validation croisée\n en fonction du nombre d'axes",
     type = "b")

res.famd <- FAMD(transactions_part,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var =c(18))
fviz_mfa_ind(res.famd, 
             habillage = "classe", # couleurs selon les modalités de la variable classe 
             palette = c("#0000FFB3","#FF00FFB3","#B8860BB3")# définition des couleurs
)

plot(res.famd,
     choix = "quali.var[3]",
     invisible = c("quanti","ind","quali.sup","quali.var" )
)
plot(res.famd, choix = "ind", invisible = "quali", select = "contrib 35")
transactions_part["pws8es0xeatk8pg5",]
transactions_part["e3j73atayt436kyc",]
transactions_part["vhd1nyq6j63gn78b",]
transactions_part["e3j73atayt436kyc",]
transactions_part["e3j73atayt436kyc",]
transactions_part["e3j73atayt436kyc",]