library(datasets)
library(arules)
library(arulesViz)
library(RColorBrewer)
data(Groceries)
View(Groceries)
class(Groceries)

setwd("C:/Users/barun/Desktop/GreatLakes/Marketing & Retail")
# data = read.csv("groceries.csv",header=FALSE)
# View(data)
# dim(data)
# trans1 = as(data[,-1]>0,"transactions")
# View(trans1)
# data$transactions = seq(1,15296)
# 
# itemFrequencyPlot(Groceries,topN=20,type="absolute")
# itemFrequencyPlot(data$V1)
# 
# as(Groceries,"list")

#Read the data
trans = read.transactions("groceries.csv", format = "basket", sep = ",")

#Finding the top selling items
itemFrequencyPlot(trans,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),main="Absolute Item Frequency Plot")

#Setting the support to 0.001 and confidence to 80%
rules = apriori(trans,parameter=list(supp=0.001,confidence=0.8))

#First five rules
inspect(rules[1:5])

#Sort by confidence
rules=sort(rules, by="confidence", decreasing=TRUE)
first_five = head(rules,n=10,by="confidence")
inspect(rules[1:5])

#Remvoving redundant rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#What do customers buy after buying milk
rules<-apriori(data=trans, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


#What do customers buy before buying milk
  rules<-apriori(data=trans, parameter=list(supp=0.001,conf = 0.15), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
ord_rules = head(rules,n=20,by="lift")
inspect(rules[1:5])


plot(rules,method = "graph",engine='interactive')
plot(rules,method = "graph",engine='htmlwidget')
plot(first_five,method = "paracoord")
plot(rules)
plotly_arules(rules)
