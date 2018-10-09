#####################################################################

# INSTALL AND LOAD PACKAGES

packages <- c('cluster', 'factoextra', 'data.table', 'corrplot', 'clValid', 'tidyverse')
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
library(tidyverse)

# LOAD LOCATIONS

base_dir <- 'LOCATION//RNA-Seq-analysis-tools-develop'
  # Set base_dir to the specific folder where this repository is stored on your computer!
script_dir <- paste(base_dir, '//scripts//correlation', sep='')
data_dir <- paste(base_dir, '//example_data//', sep='')

# LOAD FUNCTIONS

setwd(script_dir)
source('correlation_functions.r')

# LOAD DATAFRAMES

file_list <- list.files(data_dir)

# SET GLOBAL VARIABLES

N_TRANSCRIPTS = GetNTranscripts(file_list)
population_means <- InitializeMeansDF(N_TRANSCRIPTS)

####################################################################

# CALCULATE POPULATION MEANS

for (file in file_list){
  file_population_mean <- CalculatePopulationMeans(file)
  population_means <- cbind(population_means, file_population_mean)
}

# PERFORM AND PLOT PCA FOR ALL CANCER TYPES

cancer_types_pca <- prcomp(population_means, center = TRUE, scale. = TRUE) 
plot(cancer_types_pca, type = 'l')
fviz_eig(cancer_types_pca)

fviz_pca_var(cancer_types_pca,
             col.var = 'contrib',
             geom = c('point', 'text'),
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
             pointsize = 3,
             repel = TRUE
)

####################################################################
