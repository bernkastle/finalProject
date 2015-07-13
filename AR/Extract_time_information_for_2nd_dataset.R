library("dplyr")
library("MASS") ##used to produce multivariable normal distrubution
library("mvtnorm") ##used to produce density of normal distrubution
library("rgl") ##used to plot 3D graph
# read data from files
data = read.table(file.choose(), header = F, sep = " ", fill = T, col.names = c("date", "time", "sensor", "state", "action"), as.is = T)

#extract begin and end information
times = grep(".", data$action)
times = data[times,]
times = dplyr::select(times, c(date, time, action))
times$date = times$date[[1]]

#create timestamp
times$timestamp = paste(times$date, times$time, sep = " ")
times$timestamp = as.POSIXct(times$timestamp)

#calculate duration of each action and begin by seconds
for(x in seq(1, 4300, 2)){
  temp = difftime(times$timestamp[[x+1]], times$timestamp[[x]], units = "secs")
  times$dur[[x]] = temp
  times$dur[[x+1]] = 0
  temp = difftime(times$timestamp[[x]], "2010-11-04 00:00:00", units = "mins")
  times$beginBySeconds[[x]] = temp
}
rm(x)

#get start time and duration
sta_dur = grep("* begin", times$action, ignore.case = T)
sta_dur = dplyr::select(times[sta_dur,], c(action, timestamp, dur, beginBySeconds))

#each action
phoneCall = sta_dur[grep("phone*", sta_dur$action, ignore.case = T),]
washHand = sta_dur[grep("wash*", sta_dur$action, ignore.case = T),]
cook = sta_dur[grep("cook*", sta_dur$action, ignore.case = T),]
eat = sta_dur[grep("eat *", sta_dur$action, ignore.case = T),]
clean = sta_dur[grep("phone*", sta_dur$action, ignore.case = T),]

#produce cluster
k_eat = kmeans(eat$beginBySeconds, eat$dur, centers = 3)
plot(eat$beginBySeconds, eat$dur, col = k_eat$cluster, pch = 19)
points(k_eat$centers, col = 1:3, pch = 3)

k_phoneCall = kmeans(phoneCall$beginBySeconds, phoneCall$dur, centers = 3)
plot(phoneCall$beginBySeconds, phoneCall$dur, col = k_phoneCall$cluster, pch = 19)
points(k_phoneCall$centers, col = 1:3, pch = 3)

#mean and cov matrix
mean_eat_dur = mean(sta_dur$dur)
mean_eat_begin = mean(sta_dur$beginBySeconds)
temp = dplyr::select(sta_dur, c(dur, beginBySeconds))
covMatrix_eat = cov(temp)

# #plot 3D graph
# density_eat = matrix(nrow = 700, ncol = 1000)
# col = 0
# for(y in seq(from = 30001, to = 70000, length.out = 1000)){
#   col = col + 1
#   for(x in 1:700){
#     rm(temp)
#     temp = dmvnorm(x = c(x, y), mean = c(mean_eat_dur, mean_eat_begin), sigma = covMatrix_eat)
#     density_eat[x, col] = temp
#   }
# }

