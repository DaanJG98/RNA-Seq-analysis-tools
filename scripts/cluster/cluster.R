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

base_dir <- 'C://Users//Koen//Documents//RNA-Seq-analysis-tools-feature-classifier'
# Set base_dir to the specific folder where this repository is stored on your computer!
script_dir <- paste(base_dir, '//scripts//cluster', sep='')
data_dir <- paste(base_dir, '//example_data//', sep='')

# LOAD FUNCTIONS

setwd(script_dir)
source('cluster_functions.r')

# SET GLOBAL VARIABLES

MIN_N_PATIENTS = 10
MAX_CLUSTER_MULTIPLIER = 4
FOLD_CHANGE <- 1.5
CLUSTERING_METHODS <- c('hierarchical', 'kmeans', 'pam')
cancer_data <- data.frame()
patient_data <- data.frame()

# LOAD DATAFRAMES

file_list <- c('COAD.Rdata', 'READ.Rdata')
  # Set file_list to a custom selection of files or to all using 'list.files(data_dir)' (not advised due to size)!
  
  ####################################################################

# LOAD AND COMBINE CANCER DATA

for (file in file_list){
  cancer_data <- LoadCancerData(file)
}

# APPLY FOLD CHANGE FILTER

cancer_data <- FoldChangeFilter(cancer_data)

# TRANSPOSE DATA

patient_data <- t(cancer_data)

# EVALUATE BEST CLUSTERING METHOD(S) (AVOID WITH LARGE NUMBER OF DATA SETS)

evaluation <- clValid(patient_data, nClust = 2:(length(file_list)*MAX_CLUSTER_MULTIPLIER), 
                      clMethods = CLUSTERING_METHODS,
                      validation = 'internal')
optimal_methods <- GetOptimalMethods(evaluation)
RunOptimalMethods(optimal_methods, patient_data)

#####################################################################
