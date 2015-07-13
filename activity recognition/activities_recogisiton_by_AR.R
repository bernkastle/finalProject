##activities recogisiton by AR
library(arules)

data = read.table(file.choose(), header = F, sep = "\t", fill = T, as.is = F, col.names = c("date", "time", "sensor", "state", "activity"))
## define global constant
sensor_l = levels(data$sensor)
lockBinding("sensot_l", globalenv())

data = activity_classify(data)
sparse_matrix = sparse_matrix(data)