##activities recogisiton by AR
library(arules)

data = read.table(file.choose(), header = F, sep = "\t", fill = T, as.is = F, col.names = c("date", "time", "sensor", "state", "activity"))

## define global constant
sensor_num = length(levels(data$sensor))
lockBinding("sensor_num", globalenv())

data = activity_classify(data)
sparse_matrix = sparse_matrix(data)