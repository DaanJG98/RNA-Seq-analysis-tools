#####################################################################

# LOAD PACKAGES

library(cluster)
library(factoextra)
library(data.table)
library(corrplot)
library(clValid)
library(tidyverse)
library(miceadds)

####################################################################

# LOAD DATAFRAMES

base_dir <- "C:\\Users\\daan\\Documents\\week6_8\\DATA\\"
file_list <- c("BLCA.Rdata", "BRCA.Rdata")
# file_list <- list.files(base_dir)

prepDataframe <- function(file){
  x <- load(paste(base_dir,file, sep=""))
  df <- as.data.frame(matrix(unlist(get(x)), nrow = nrow(get(x))), col.names=names(get(x)), row.names = rownames(get(x)))
  df[is.na(df)] <- 0
  name = replace(file, ".Rdata", "")
  df$avg = apply(df,1,mean,na.rm=FALSE)
  colnames(df)[colnames(df) == "avg"] <- name
  print(head(df))
  return (df[, ncol(df)])
}


for (file in file_list){
  
  
  temp_avg <- prepDataframe(file)
  # print(head(temp_avg))
  # load.Rdata(paste(base_dir,file, sep=""), file)
}

data_array <- c(data.BLCA, data.BRCA, data.CESC, data.CHOL, data.COAD, data.ESCA, data.HNSC, data.KICH, data.KIRC, data.KIRP, data.LIHC, data.LUAD, data.LUSC, data.PAAD, data.PCPG, data.PRAD, data.READ, data.STAD, data.THCA, data.UCEC)

rowMeans(data.BLCA, na.rm = FALSE, dims = 1)

data.BLCA %>% mutate(BLCA = rowMeans(.))


df <- as.data.frame(matrix(unclass(data.BLCA), nrow = nrow(data.BLCA)), col.names=names(data.BLCA), row.names = rownames(data.BLCA))
df[is.na(df)] <- 0
df$avg = apply(df,1,mean,na.rm=FALSE)




