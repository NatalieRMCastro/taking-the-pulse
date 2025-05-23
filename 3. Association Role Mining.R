##########################
# 1. ENVIRONMENT CREATION --------------------------
##########################
install.packages("arules", dependencies = TRUE)
install.packages("TSP")
install.packages("data.table")
install.packages("arulesViz", dependencies = TRUE)
install.packages("sp")
install.packages("dplyr", dependencies = TRUE)
install.packages("purrr", dependencies = TRUE)
install.packages("devtools", dependencies = TRUE)
install.packages("tidyr")
install.packages("extrafont")
extrafont::font_import(prompt = FALSE)
install.packages("devtools")
devtools::install_github("mhahsler/arules")

Sys.which("make")

library(Matrix)
library(viridis)
require(arules)
library(TSP)
library(data.table)
#library(ggplot2)
#library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)
library(RColorBrewer)
library(igraph)


extrafont::loadfonts(device = "win")


par(
  bg = "white",                         # Overall background (plot + margins)
  col.axis = "black",                   # Axis text color
  col.lab = "black",                    # Axis label color
  col.main = "black",                   # Title color
  family = "Times New Roman",           # Font
  fg = "black"                          # Axis/box color
)


##########################
# 2. DATA IMPORT           --------------------------
##########################
basket_data <- read.transactions("Sample Post Basket Data.csv",
                                 rm.duplicates=FALSE,
                                 format='basket',
                                 header = FALSE,
                                 sep=',',
                                 cols=NULL)

inspect(basket_data[0:1])

##########################
# 3. ASSOCIATION ROLE MINING  --------------------------
##########################

apriori_rules <- arules::apriori(basket_data,
                                parameter = list(support=.1,
                                                 confidence=.1,
                                                 minlen=2,maxlen=5))
inspect(apriori_rules)

## Plotting the most frequent items



arules::itemFrequencyPlot(
  basket_data,
  topN =9,
  col = "black",
  main = "Most Frequent Words for Sample Posts on International Workers Day",
  type = "relative",
  ylab = "Relative Item Frequency"
)

## Sorting the rules
rules_sorted <- sort(apriori_rules,by='lift',
                     decreasing=TRUE)

inspect(rules_sorted[1:10])

## Saving the rules to a CSV
write(rules_sorted,
      file='Full Association Rules.csv',
      sep= ',',
      quote=TRUE,
      row.names = FALSE)

## Visualizing the rules
plot(rules_sorted,method='graph',shading='confidence')

##########################
# 3. ASSOCIATION RULE MINING  --------------------------
#
# Climate Specific Rules 
##########################

## Climate Specific Rules
climate_rules <- arules::apriori(data=basket_data,
                                 parameter = list(support=.01,conf=.01,minlen=2,maxlen=5),
                                 appearance= list(lhs="climate"),
                                 control = list(verbose=TRUE))

inspect(climate_rules)

climate_rules_sorted <- sort(climate_rules,by='lift',
                             decreasing=TRUE)

inspect(climate_rules_sorted[0:10])


## Saving the rules to a CSV
write(climate_rules_sorted,
      file='Climate Association Rules.csv',
      sep= ',',
      quote=TRUE,
      row.names = FALSE)


## Visualizing the rules
plot(climate_rules_sorted,method='graph',shading='confidence')
plot(climate_rules_sorted,method='graph',shading='confidence',engine='interactive')

## Note: I tried climate change but it did not generate any valid rules.


##########################
# 3. ASSOCIATION RULE MINING  --------------------------
#
# Workers Rights Specific Rules 
##########################

## Worker Specific Rules
worker_rules <- arules::apriori(data=basket_data,
                                 parameter = list(support=.01,conf=.01,minlen=2,maxlen=5),
                                 appearance= list(lhs="workers"),
                                 control = list(verbose=TRUE))

inspect(worker_rules)

worker_rules_sorted <- sort(worker_rules,by='lift',
                             decreasing=TRUE)

inspect(worker_rules_sorted[0:10])

## Saving the rules to a CSV
write(worker_rules_sorted,
      file='Worker Association Rules.csv',
      sep= ',',
      quote=TRUE,
      row.names = FALSE)

## Visualizing the rules
plot(worker_rules_sorted,method='graph',shading='confidence',)


##########################
# 3. ASSOCIATION RULE MINING  --------------------------
#
# International Workers Day Specific Rules 
##########################

## Worker Specific Rules
iwd_rules <- arules::apriori(data=basket_data,
                                parameter = list(support=.01,conf=.01,minlen=2,maxlen=5),
                                appearance= list(lhs="international"),
                                control = list(verbose=TRUE))

inspect(iwd_rules)

iwd_rules_sorted <- sort(iwd_rules,by='lift',
                            decreasing=TRUE)

inspect(iwd_rules_sorted[0:10])

## Saving the rules to a CSV
write(iwd_rules_sorted,
      file='International Workers Day Association Rules.csv',
      sep= ',',
      quote=TRUE,
      row.names = FALSE)

## Visualizing the rules
plot(iwd_rules_sorted,method='graph',shading='confidence',)

##########################
# 3. ASSOCIATION RULE MINING  --------------------------
#
# Labor Specific Rules 
##########################

## Worker Specific Rules
labor_rules <- arules::apriori(data=basket_data,
                             parameter = list(support=.01,conf=.01,minlen=2,maxlen=5),
                             appearance= list(lhs="labor"),
                             control = list(verbose=TRUE))

inspect(labor_rules)

labor_rules_sorted <- sort(labor_rules,by='lift',
                         decreasing=TRUE)

inspect(labor_rules_sorted[0:10])

## Saving the rules to a CSV
write(labor_rules_sorted,
      file='Labor Association Rules.csv',
      sep= ',',
      quote=TRUE,
      row.names = FALSE)

## Visualizing the rules
plot(labor_rules_sorted,method='graph',shading='confidence',)

