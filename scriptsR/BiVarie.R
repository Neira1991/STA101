library(tidyverse)
library(stargazer)
#library(Rcmdr)
library(car)
library(lattice)
library(RColorBrewer)
library(questionr)
library(rgrs)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")
#/Users/stevenneira/Documents/Master/Cnam/STA101/Rapport/STA101/
transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
rownames(transactions) <- transactions$fullVisitorId
options(scipen = 100)
transactions <- transactions %>% filter(timeOnSite > 100)
transactions<-transactions[!(transactions$browser_name=="Opera" | transactions$browser_name=="MIUI Browser" | transactions$browser_name=="Iron"),]

variables_quantitatives <- select(transactions, hits, pageviews, 
                                  totalTransactionRevenue, 
                                  timeOnSite, 
                                  browser_width, browser_height, 
                                  itemCount)

variables_qualitatives <- select(transactions, deviceCategory, browser_name, 
                                 operatingSystem, language, 
                                 country, 
                                 city, paymentMethod)

pairs(variables_quantitatives,cex=.4)

mat.cor<-cor(variables_quantitatives)
print(round(mat.cor,2))

table(variables_qualitatives$deviceCategory)


#transactions["19100317581818704",]
#transactions[-c("npwcqk2j4d2sng4f", "xqeqqa0ay7w9w4bx","19100317581818704","cc0s8x7bzq7v8jc4")]


tmp <- Boxplot(totalTransactionRevenue~browser_name, data = transactions, axes=FALSE)


histogram( ~timeOnSite | browser_name, data = transactions,
           col="lightgreen",
           ylab="",
           nint=12)

variables_quantitatives$timeOnSite


#Variables suplementaires
transactions$sizeArea <- transactions$browser_width * transactions$browser_height
Boxplot( ~ sizeArea, data=transactions,main="Boîte à moustaches pour la variable sizeArea")
head(transactions$sizeArea)



#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization 
transactions$hits_norm <- min_max_norm(transactions$hits)
transactions$pageviews_norm <- min_max_norm(transactions$pageviews)
transactions$timeOnSite_norm <- min_max_norm(transactions$timeOnSite)
head(transactions$timeOnSite_norm)
head(transactions$pageviews_norm)
head(transactions$timeOnSite_norm)

transactions$activity <- (transactions$hits_norm + transactions$pageviews_norm + transactions$timeOnSite_norm) / 3
Boxplot( ~ activity, data=transactions,main="Boîte à moustaches pour la variable activity")
head(transactions$activity)


##Test chi-deux

tab <- table(transactions$operatingSystem, transactions$deviceCategory)
lprop(tab)
chisq.test(tab)
mosaicplot(tab)
mosaicplot(tab, las = 3, shade = TRUE)

