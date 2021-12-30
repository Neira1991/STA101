library(FactoMineR)
library(Factoshiny)
library(dplyr)

transactions <- read.csv(file = "/Users/steven/Documents/cnam/methodes_descriptives/scriptsR/datasetClarinsTransactions.csv")

transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)

variables_ACP <- select(transactions,hits, 
                                  totalTransactionRevenue,
                                  browser_width, browser_height, 
                                  itemCount, browser_name, deviceCategory,paymentMethod)


res <- Factoshiny(variables_ACP)