## add action number to different action
activity_classify = function(dataframe){
  dataframe$activity_label = 0 ## init activity label
  action_l = levels(dataframe$activity) ## find all level of activity label
  action_l = action_l[grep(".", action_l)] ## throw away empty one
  
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
create_sparse_matrix = function(dataframe){
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
  root_start = 1 ## set root number
  root_label = dataframe$activity_label[root_start] ## set root label
  matrix[1, 1] = root_label
  row_count = 1 ## set sparse matrix row count
  
  ## scan each row one by one
  for(x in 1:length(dataframe$activity_label)){
    ## if they are same label do nothing
    if(dataframe$activity_label[x] == root_label){
    }
    ## if they are not same, extract sensor information and fill it to matrix
    else{
      root_end = x-1 ## update root end to the end of first activity
      temp_factor = dataframe$sensor[root_start:root_end]
      temp_factor = factor(temp_factor)
      temp_factor_l = levels(temp_factor)
      for(y in 1:sensor_num){
        count = grep(sensor_l[y], temp_factor_l)
        count = length(count)
        if(count >= 1){
          matrix[row_count, y+1] = 1
        } else{
          matrix[row_count, y+1] = 0
        }
      }
      row_count = row_count + 1 ## move to next row
      matrix[row_count,1] = dataframe$activity_label[x]
      root_start = x
      root_label = dataframe$activity_label[root_start] ## reset root label
    }
  }
  ## add last row information manually
  root_end = length(dataframe$activity_label)
  temp_factor = dataframe$sensor[root_start:root_end]
  temp_factor = factor(temp_factor)
  temp_factor_l = levels(temp_factor)
  for(y in 1:sensor_num){
    count = grep(sensor_l[y], temp_factor_l)
    count = length(count)
    if(count >= 1){
      matrix[row_count, y+1] = 1
    } else{
      matrix[row_count, y+1] = 0
    }
  }
  return(matrix)
}