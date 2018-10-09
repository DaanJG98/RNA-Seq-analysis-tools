#####################################################################

# INSTALL AND LOAD PACKAGES
packages <- c('cluster', 'factoextra', 'data.table', 'corrplot', 'clValid')
for(package in packages){
  if(is.element(package, installed.packages()[,1])==FALSE){
    install.packages(package)
  }
}
library(cluster)
library(factoextra)
library(data.table)
library(corrplot)
library(clValid)

# LOAD LOCATIONS

# base_dir <- ''
base_dir <- 'C://Users//Koen//Documents//RNA-Seq-analysis-tools'
  # Set base_dir to the specific folder where this repository is stored on your computer!
script_dir <- paste(base_dir, '//scripts//cluster', sep='')
data_dir <- paste(base_dir, '//example_data//', sep='')

# LOAD FUNCTIONS

setwd(script_dir)
source('cluster_functions.r')

# LOAD DATAFRAMES

# file_list <- list.files(data_dir)
file_list <- c('KIRC.Rdata', 'KIRP.Rdata', 'LUAD.Rdata', 'LUSC.Rdata', 'COAD.Rdata', 'THCA.Rdata')

# SET GLOBAL VARIABLES

MIN_N_PATIENTS = 10
cancer_data <- data.frame()
patient_data <- data.frame()
# d.cancer <- data.frame()

####################################################################

# LOAD AND COMBINE CANCER DATA


for (file in file_list){
  LoadCancerData(file)
}

# Apply fold change filter
fc <- 1.5 # GLOBAL VAR
sel <- apply(d.cancer, MARGIN = 1, FUN = function(x) any(abs(x)>fc))
d.cancer <- d.cancer[sel,]




# TRANSPOSE DATA
patient_data <- t(cancer_data)
# d.patient <- t(d.cancer)

evaluation <- clValid(d.patient, nClust = 2:4, 
                      clMethods = c("hierarchical","kmeans","pam"),
                      validation = "internal")
top_evaluation <- optimalScores(evaluation)
top_evaluation <- top_evaluation[which.max(top_evaluation$Score),]
print(top_evaluation)

# Summary
summary(intern)
# oS <- optimalScores(intern)
# oS[which.max(oS$Score),]
# 2 = method, 3 = n clusters





### k-means clustering
km.res <- kmeans(d.patient, 2, nstart = 25)
fviz_cluster(km.res, data = d.patient, ellipse.type = "norm", labelsize = 8)


### Partioning clustering
pam.res <- pam(d.patient, 2)
fviz_cluster(pam.res)


### Hierarchical clustering - 1
d <- dist(d.patient, method = "euclidean")
res.hc <- hclust(d, method = "ward.D2" )

ct.n <- cutree(res.hc, k = 2)

plot(res.hc, cex=0.5) 
rect.hclust(res.hc, k = 2) 

### Hierarchical clustering - 2
res <- hcut(d.patient, k = 5, stand = TRUE)
fviz_dend(res, rect = TRUE, cex = 0.1)


###################################
#
# Step 8. Check cluster results
#

intern <- clValid(d.patient, nClust = 2:4, 
                  clMethods = c("hierarchical","kmeans","pam"),
                  validation = "internal")
# Summary
summary(intern)
# oS <- optimalScores(intern)
# oS[which.max(oS$Score),]
# 2 = method, 3 = n clusters
