library(tidyverse)
library(stargazer)
library(Rcmdr)
library(car)
library(lattice)
library(RColorBrewer)

transactions <- read.csv(file = "/Users/stevenneira/Documents/Master/Cnam/STA101/Rapport/STA101/scriptsR/datasetClarinsTransactions.csv")
transactions <- distinct(transactions, fullVisitorId, .keep_all= TRUE)
rownames(transactions) <- transactions$fullVisitorId
options(scipen = 100)
variables_quantitatives$totalTransactionRevenue

#Methode payment
transform_language <- function(x) {
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

cities_to_include <- c("", "Paris", "Dublin", "Cork", "Lyon", "Den Helder")
testcities <- filter(transactions, city %in% cities_to_include)
pie(sort(table(testcities$city),  decreasing = TRUE),
    main = "nouvel utilisateur?",
    c("NA","Paris", "Dublin", "Cork", "Lyon", "Den Helder")
)

countries_to_include <- c("France", "Netherlands", "Ireland", "United States")

transactions$country <- filter(transactions, country %in% countries_to_include)
test
country = sort(table(transactions$country), decreasing = TRUE)
country

transactions$paymentMethod <- sapply(transactions$paymentMethod, function(x) transform_language(x))
transactions$paymentMethod 

transactions$language <- sapply(transactions$language, function(x) substr(x,1,2))


pie(table(transactions$paymentMethod),
    col=c("darkseagreen1","lightcoral","gray7","lightgoldenrod1"),
    main = "Payment Method"
)




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


barplot(sort(table(transactions$language), decreasing = TRUE),
        col=coul,
        cex.axis=0.8,
        cex.names=0.8,
        las=2)

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
length(cities)

variables_qualitatives %>% filter(mass > mean(mass, na.rm = TRUE))

country = sort(filter(table(variables_qualitatives$country),France), decreasing = TRUE)
country



names(cities)
barplot(sort(table(variables_qualitatives$country), decreasing = TRUE),
        col=coul,
        cex.axis=0.8,
        cex.names=0.8,
        las=2)

pie(table(variables_qualitatives$paymentMethod),
    col=c("deepskyblue","seagreen3", "goldenrod1"),
    main = "paymentMethod"
)