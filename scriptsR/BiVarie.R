library(tidyverse)
library(stargazer)
#library(Rcmdr)
library(car)
library(lattice)
library(RColorBrewer)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")
transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
options(scipen = 100)
transactions <- transactions %>% filter(timeOnSite > 100)

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


coul <- brewer.pal(5, "Set2")

tmp <- Boxplot(totalTransactionRevenue~browser_name, data = transactions, axes=FALSE)
text(seq_along(transactions$browser_name), par("usr")[3] - 0.5, labels = names(transactions$browser_name), srt = 90, adj = 1, xpd = TRUE);

histogram( ~totalTransactionRevenue | browser_name, data = transactions,
           col="lightgreen",
           ylab="",
           nint=12)

variables_quantitatives$timeOnSite



