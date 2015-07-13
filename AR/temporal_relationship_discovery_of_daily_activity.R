library(Ckmeans.1d.dp) #this package used for one-dimensional clustering
library(data.table)
library(arules)

#extract all action_begin from data frame "times"
action_begin = dplyr::select(times, c(action, timestamp))
action_begin = action_begin[grep("*begin", action_begin$action),]
action_begin = data.table(action_begin)

#apply one-dimensional clustering algorithm
cluster_info = Ckmeans.1d.dp(action_begin$timestamp)
action_begin[, cluster_info := cluster_info$cluster] #add clustering information
action_begin[, successor := action_begin$action[2:120]]

#remove timestmap
action_relationship = action_begin[, -2, with = F]
rules = apriori(action_relationship)

#as factor
for(x in 1:3){
  action_relationship[[x]] = as.factor(action_relationship[[x]])
}

#apply apriori
rules = apriori(action_relationship, appearance = list(rhs = c("successor= *"), default= "lhs"))
rules_sub = subset(rules, subset = rhs %pin% "successor=") #filte rules
