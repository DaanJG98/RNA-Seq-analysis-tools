
library(cluster)
library(factoextra)
library(data.table)
library(corrplot)
library(clValid)

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

###################################################################
#
#         Correlations
#
###################################################################

###################################
#
# Step 5. Get correlation between patients
#

### correlation
corr.cancer <- cor( d.cancer )

### test significance of correlation
test.cancer <- cor.mtest( d.cancer, conf.level=0.95 )

### plot as corrplot
corrplot( corr.cancer, p.mat=test.cancer$p, sig.level=0.05, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex=0.6)

### plot as heatmap
heatmap( corr.cancer )

###################################################################
#
#         Principle component analysis
#
###################################################################

###################################
#
# Step 6. Get principle components of patient data
#

d.patient <- t(d.cancer)

cancer.pca <- prcomp(d.cancer, center = TRUE, scale. = TRUE) 
plot(cancer.pca , type = "l")
fviz_eig(cancer.pca)

patient.pca <- prcomp(d.patient, center = TRUE, scale. = TRUE) 
plot(patient.pca , type = "l")
fviz_eig(patient.pca)

fviz_pca_ind(patient.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(cancer.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

