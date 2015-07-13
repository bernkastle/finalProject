##activities recogisiton by AR
library(arules)

data = read.table(file.choose(), header = F, sep = "\t", fill = T, as.is = F, col.names = c("date", "time", "sensor", "state", "activity"))

## define global constant
sensor_l = levels(data$sensor)
sensor_num = length(sensro_l)
lockBinding(c("sensor_num", "sensro_l"), globalenv())

data = activity_classify(data)
sparse_matrix = create_sparse_matrix(data)
sparse_matrix = fill_matrix(data, sparse_matrix)
