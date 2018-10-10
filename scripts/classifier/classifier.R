packages <- c('gbm', 'naivebayes', 'tree', 'rpart', 'caret', 'e1071', 'rpart.plot')
for(package in packages){
  if(is.element(package, installed.packages()[,1])==FALSE){
    install.packages(package)
  }
}
library(gbm)
library(naivebayes)
library(tree)
library(rpart)
library(caret)
library(e1071)
library(rpart.plot)

base_dir <- 'C://Users//daan//Documents//GitHub//RNA-Seq-analysis-tools'
# Set base_dir to the specific folder where this repository is stored on your computer!
script_dir <- paste(base_dir, '//scripts//classifier', sep='')
data_dir <- paste(base_dir, '//example_data//', sep='')

# LOAD FUNCTIONS

setwd(script_dir)
source('classifier_functions.r')

# LOAD DATAFRAMES

file_list <- c("COAD.Rdata", "LUSC.Rdata", "THCA.Rdata")

cancer_types <- list("COAD","LUSC","THCA")
labels <- list("Colorectal","Lung","Thyroid")
names(labels) <- cancer_types

cancer_data <- data.frame()
patient_data <- data.frame()
MIN_N_PATIENTS = 10
FOLD_CHANGE <- 0.05


####################################################################

# LOAD AND COMBINE CANCER DATA

for (file in file_list){
  cancer_data <- LoadCancerData(file)
}

# APPLY FOLD CHANGE FILTER

cancer_data <- FoldChangeFilter(cancer_data)

# TRANSPOSE DATA

patient_data <- t(cancer_data)

# ADD COLUMN WITH LABELS TO NEW DATAFRAME

patient_data_with_labels <- AppendLabels(patient_data, labels)


####################################################################


# DEFINE PATIENT LABELS AS FACTOR
y <- as.factor(patient_data_with_labels$label)

# CREATE BASIC NAIVEBAYES MODEL WITHOUT TEST SET
model.nb <- naiveBayes(patient_data, y)
table(predict(model.nb, patient_data), y)

# cREATE NAIVE BAYES MODEL WITH CROSS VALIDATION

model = train(x=patient_data,y=y,method='naive_bayes',trControl=trainControl(method='cv',number=10),metric="Accuracy")

# CREATE RPART DECISION TREE WITH CROSS VALIDATION

model = train(x=patient_data,y=y,method='rpart',trControl=trainControl(method='cv',number=10),metric="Accuracy")
