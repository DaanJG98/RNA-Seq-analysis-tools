# Load cancer data and combine different cancer types together. 
LoadCancerData <- function(file){
  f <- load(paste(data_dir, file, sep=''))
  n_patients <- ncol(get(f))
  if(n_patients >= MIN_N_PATIENTS){
    df <- as.data.frame(matrix(unlist(get(f)), nrow = nrow(get(f)), ncol = ncol(get(f))))
    rownames(df) = rownames(get(f))
    colnames(df) = colnames(get(f))
    df[is.na(df)] <- 0
    if (nrow(cancer_data) > 0){
      cancer_data <- cbind(cancer_data, df)
    } else {
      cancer_data <- df
    }
  }
  return(cancer_data)
}

# Filter out data below a given fold change.
FoldChangeFilter <- function(data, fc = FOLD_CHANGE){
  selection <- apply(data, MARGIN = 1, FUN = function(x) all(abs(x)>fc))
  result <- data[selection,]
  return(result)
}

# Append labels to dataframe
AppendLabels <- function(df, label_list){
  temp <- c(mode='character', length = nrow(df))
  index = 1
  for (patient in rownames(df)){
    cancer_type <- tail(strsplit(x = patient, split = "-")[[1]], 1)
    # APPEND label TO temo
    
    temp[index] = label_list[[cancer_type]]
    #df[patient,]$label = label_list$cancer_type
    index = index+1
  }
  df$label = temp
  return(df)
}
