LoadCancerData <- function(file){
  
}

# for (file in file_list){
#   f <- load(paste(data_dir, file, sep=''))
#   n_patients <- ncol(get(f))
#   if(n_patients < MIN_N_PATIENTS){
#     break()
#   }
#   df <- as.data.frame(matrix(unlist(get(f)), nrow = nrow(get(f)), ncol = ncol(get(f))))
#   rownames(df) = rownames(get(f))
#   colnames(df) = colnames(get(f))
#   df[is.na(df)] <- 0
#   if (nrow(d.cancer) > 0){
#     d.cancer <- cbind(d.cancer, df)
#   } else {
#     d.cancer <- df
#   }
# }