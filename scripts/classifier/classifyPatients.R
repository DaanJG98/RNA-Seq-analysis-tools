
library(caret)
library(gbm)
library(naivebayes)
library(tree)
library(rpart)
library(caret)
library(e1071)

###################################################################
#
#         Data retrieval and preprocessing
#
###################################################################

###################################
# 
# Step 1. Read TCGA datasets
#
load("/home/devel/TCGA/data/READ.Rdata")
load("/home/devel/TCGA/data/CHOL.Rdata")

###################################
#
# Step 2. Merge datasets into a single dataset for further analysis. 
# 
d.cancer <- cbind(data.READ, data.CHOL)

###################################
#
# Step 3. Select rows that are complete by removing missing values
#
d.cancer <- d.cancer[complete.cases(d.cancer),]

###################################
#
# Step 4. Select genes above a given threshold (fc)
#

fc <- 1.0
sel <- apply(d.cancer, MARGIN = 1, FUN = function(x) all(abs(x)>fc))
d.cancer <- d.cancer[sel,]

###################################
#
# Step 5. Transpose matrix
#
d.patient <- t(d.cancer)

####################################################################
##
##         Classification
##
####################################################################

###################################
# 
# Step 6. Define input data
#

x <- as.data.frame( d.patient )
y <- as.factor(grepl("READ", rownames(d.patient), fixed=TRUE))


###################################
# 
# Step 7. Classify by NB method
#

model.nb <- naiveBayes(x, y)
table(predict(model.nb, x), y)


###################################
# 
# Step 8. Classify by NB method from caret package
#

model = train(x=x,y=y,method='naive_bayes',trControl=trainControl(method='cv',number=10),metric="Accuracy")

