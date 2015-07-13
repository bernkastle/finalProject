## add action number to different action
activity_classify = function(dataframe){
  dataframe$activity_label = 0
  action_l = levels(dataframe$activity)
  action_l = action_l[grep(".", action_l)]
  for(x in seq(from = 1, to = length(action_l), by = 2)){
    temp_s = grep(action_l[[x]], dataframe$activity)
    temp_e = grep(action_l[[x+1]], dataframe$activity)
    for (temp in 1:length(temp_e)) {
      dataframe$activity_label[temp_s[[temp]]:temp_e[[temp]]] = (x+1)/2
    }
  }
  return(dataframe)
}
## create empty sparse matrix
sparse_matrix = function(dataframe){
  total_actions = length(grep("* begin", dataframe$activity))
  sensor_l = levels(dataframe$sensor)
  total_sensors = length(sensor_l)
  sparse_matrix = matrix(nrow = total_actions, ncol = total_sensors+1)
  rownames(sparse_matrix) = rownames(sparse_matrix, do.NULL = FALSE, prefix = "Activity.")
  colnames(sparse_matrix) = c("activity_num", sensor_l[1:total_sensors])
  return(sparse_matrix)
}
## fill empty sparse matrix
fill_matrix = function(dataframe, matrix){
  
}