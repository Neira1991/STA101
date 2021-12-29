library(tidyverse)
library(stargazer)
library(Rcmdr)
library(car)
library(lattice)
library(RColorBrewer)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")
transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
options(scipen = 100)
variables_quantitatives$totalTransactionRevenue

variables_quantitatives <- select(transactions, hits, 
                                  newVisits, pageviews, 
                                  totalTransactionRevenue, 
                                  transactions, timeOnSite, 
                                  browser_width, browser_height, 
                                  itemCount)

variables_qualitatives <- select(transactions, deviceCategory, browser_name, 
                                 operatingSystem, language, 
                                  country, 
                                  city, paymentMethod)

stargazer(variables_quantitatives, type = "text")
stargazer(variables_quantitatives, type = "latex")

variables_quantitatives.scale <- scale(variables_quantitatives)
head(round(variables_quantitatives.scale, 2))


#variables_quantitatives %>% filter(hits > 690)
#transactions %>% filter(hits > 690)

n<-nrow(variables_quantitatives)#nombre d'individus
k<-ceiling(1 + log(n)/log(2))#nombre de classes

Boxplot( ~ itemCount, data=variables_quantitatives,main="Boîte à moustaches pour la variable itemCount")

## construction de l'histogramme
histogram(variables_quantitatives$itemCount,nint=20)
min(variables_quantitatives$browser_height)
hist(variables_quantitatives$totalTransactionRevenue)
table(variables_quantitatives$hits)
histogram(variables_quantitatives$newVisits)
histogram(table(variables_qualitatives$deviceCategory))

pie(table(variables_quantitatives$newVisits),
    col=c("grey","seagreen3"),
    main = "nouvel utilisateur?",
    labels=c("No","Oui")
    )



#VariablesQualitatives
head(variables_qualitatives)
table(variables_qualitatives$deviceCategory)
coul <- brewer.pal(5, "Set2") 

cities = sort(table(variables_qualitatives$city), decreasing = TRUE)
cities
names(cities)
barplot(sort(table(variables_qualitatives$deviceCategory), decreasing = TRUE),
        col=coul,
        cex.axis=0.8,
        cex.names=0.8,
        las=2)

pie(table(variables_qualitatives$paymentMethod),
    col=c("deepskyblue","seagreen3", "goldenrod1"),
    main = "paymentMethod"
)