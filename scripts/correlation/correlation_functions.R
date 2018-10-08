GetNTranscripts <- function(file_list){
  file_nrows <- vector(mode='integer', length=length(file_list))
  n_transcripts = 0
  for (i in 1:length(file_list)){
    f <- load(paste(data_dir, file_list[i], sep=''))
    file_nrows[i] = nrow(get(f))
  }
  if (all(file_nrows == file_nrows[1])){
    n_transcripts = file_nrows[1]
  } else {
    n_transcripts = max(file_nrows)
  }
  return(n_transcripts)
}

InitializeMeansDF <- function(n_row){
  df <- data.frame(matrix(ncol = 0, nrow = n_row))
  return(df)
}

CalculatePopulationMeans <- function(file, min_population = NULL){
  cancer_type = str_replace(file, '.Rdata', '')
  f <- load(paste(data_dir, file, sep=''))
  df <- as.data.frame(matrix(unlist(get(f)), nrow = nrow(get(f)), ncol = ncol(get(f))))
  rownames(df) = rownames(get(f))
  colnames(df) = colnames(get(f))
  df[is.na(df)] <- 0
  df$avg = apply(df, 1, mean, na.rm = FALSE)
  result <- subset(df, select = 'avg')
  names(result)[names(result) == 'avg'] <- cancer_type
  return(result)
}