library(tidyverse)
library(stargazer)
library(Rcmdr)
library(car)
library(lattice)
library(RColorBrewer)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")
transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
options(scipen = 100)

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



Boxplot(timeOnSite~deviceCategory, data = transactions)

histogram( ~itemCount | deviceCategory, data = transactions,type="density"
           ,col="lightgreen"
           ,ylab="",
           nint=12)

