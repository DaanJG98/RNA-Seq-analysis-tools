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
  selection <- apply(data, MARGIN = 1, FUN = function(x) any(abs(x)>fc))
  result <- data[selection,]
  return(result)
}

# Get best scoring clustering methods from clValid class.
GetOptimalMethods <- function(evaluation){
  optimals <- optimalScores(evaluation)
  methods <- list()
  for(o in 1:length(optimals)){
    method <- as.character(optimals[o, 2])
    n <- as.integer(as.character(optimals[o, 3]))
    combined <- c(method, n)
    methods[[o]] <- combined
  }
  methods <- unique(methods)
  return(methods)
}

# Run and plot hierarchical clustering.
HierarchicalClustering <- function(data, n, cex = 1, ...){
  res <- hcut(data, k = n, stand = TRUE)
  fviz_dend(res, rect = TRUE, cex = cex)
}

# Run and plot K-means clustering.
KMeansClustering <- function(data, n, ...){
  res <- kmeans(data, n, nstart = 25)
  fviz_cluster(res, data = patient_data, ellipse.type = 'norm', labelsize = 8)
}

# Run and plot partioning clustering.
PartioningClustering <- function(data, n, ...){
  res <- pam(data, n)
  fviz_cluster(res, labelsize = 8)
}

# Run clustering methods derived from GetOptimalMethods
RunOptimalMethods <- function(methods, data){
  for(m in methods){
    method <- m[1]
    n <- as.integer(m[2])
    
    # TO-DO: Display plots OR return which functions and parameters to call alternatively.
    
    # if(method == 'hierarchical'){
    #   HierarchicalClustering(data, n)
    # } else if(method == 'kmeans'){
    #   KMeansClustering(data, n)
    # } else if(method == 'pam'){
    #   PartioningClustering(data, n)
    # }
  }
}
