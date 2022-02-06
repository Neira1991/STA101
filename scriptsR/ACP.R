library(FactoMineR)
library(Factoshiny)
library(dplyr)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")

transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
rownames(transactions) <- transactions$fullVisitorId

variables_ACP <- select(transactions,hits, 
                        pageviews, 
                        totalTransactionRevenue, 
                        transactions, timeOnSite, 
                        browser_width, browser_height, 
                        itemCount, browser_name)

variables_qualitatives <- select(transactions, deviceCategory, browser_name, 
                                 operatingSystem, language, 
                                 country, 
                                 city, paymentMethod)


res <- Factoshiny(variables_qualitatives)