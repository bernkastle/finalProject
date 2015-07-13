library("dplyr")
library("MASS") ##used to produce multivariable normal distrubution
library("mvtnorm") ##used to produce density of normal distrubution
library("rgl") ##used to plot 3D graph
library("data.table")
library(Ckmeans.1d.dp)
library(arules)
# read data from files
rm(list = ls())
data = read.table(file.choose(), header = F, sep = "\t", fill = T, col.names = c("date", "time", "sensor", "state", "action"), as.is = T)

#extract begin and end information
times = grep(".", data$action)
times = data[times,]
times = dplyr::select(times, c(date, time, action))
times$date = times$date[[1]]

#create timestamp
times$timestamp = paste(times$date, times$time, sep = " ")
times$timestamp = as.POSIXct(times$timestamp)

#get start time and duration
action_info = grep("* begin", times$action, ignore.case = T)
action_info = dplyr::select(times[action_info,], c(action, timestamp))

#sepreate action for individuals
action_r1 = grep("R2", action_info$action, ignore.case = T)
action_r1 = action_info[-action_r1,] ##r1 and both
action_r2 = grep("R1", action_info$action, ignore.case = T)
action_r2 = action_info[-action_r2,] ##r1 and both
action_r1 = data.table(action_r1)  ##change data frame to data.table
action_r2 = data.table(action_r2)

#calcualte clusters
cluster_info_r1 = Ckmeans.1d.dp(action_r1$timestamp)
# plot(x=action_r1$timestamp, y= 1:334, col = cluster_info_r1$cluster)
cluster_info_r2 = Ckmeans.1d.dp(action_r2$timestamp)
# plot(x=action_r2$timestamp, y= 1:318, col = cluster_info_r2$cluster)

### add cluster information to action
action_r1 = action_r1[, cluster_info := cluster_info_r1$cluster]
action_r2 = action_r2[, cluster_info := cluster_info_r2$cluster]

### add successer information
action_r1 = action_r1[, successor := action_r1$action[2:334]]
action_r2 = action_r2[, successor := action_r2$action[2:318]]

## remove timestamp
action_relationship_r1 = action_r1[, -2, with = F]
action_relationship_r2 = action_r2[, -2, with = F]

## as.factor
for(x in 1:3){
  action_relationship_r1[[x]] = as.factor(action_relationship_r1[[x]])
  action_relationship_r2[[x]] = as.factor(action_relationship_r2[[x]])
}
## apply Apriori
rules_r1 = apriori(action_relationship_r1, parameter = list(supp = 0.01, conf = 0.3))
rules_sub_r1 = subset(rules_r1, subset = rhs %pin% "successor=") #filte rules
rules_r2 = apriori(action_relationship_r2, parameter = list(supp = 0.01, conf = 0.3))
rules_sub_r2 = subset(rules_r2, subset = rhs %pin% "successor=") #filte rules