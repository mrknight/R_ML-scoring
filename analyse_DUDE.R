actives <- unique(read.table("/Users/knight/Dropbox/data/DUD-E/ADA_actives.txt")[,1])


#poseList = read.csv(paste("/Users/knight/Dropbox/data/", "cs-confgen_all_SP.csv", sep=""))
poseList = read.csv(paste("/Users/knight/Dropbox/data/DUD-E/", "DUD-E_SIFt_c12b0_ADA.csv", sep=""))
len = length(colnames(poseList))
#print(strsplit(toString(poseList[,1]), '_'))
post = regexpr('_', poseList[, len]) - 1
print(unique(substr(poseList[, len],1,post)))

{
export CLASSPATH=/home/dat/WORK/dev/weka/weka.jar:$CLASSPATH
library(ROCR);

#load ligands and decoys
lig <- unique(read.table("ligands.txt")[,1]);
dec <- unique(read.table("decoys.txt")[,1]);

#load data file from docking
uniqRes <- read.table("dataforR_uq.txt",header=T);

#change colnames
colnames(uniqRes)[1]="LigandName";

#add column with ligand/decoy info
uniqRes$IsActive <- as.numeric(uniqRes$LigandName %in% lig)

#define ROC parameters 
#here INTER is selected to compare between ligands using rDock SCORE.INTER
#this could be changed for also running with other programs
predINTERuq <- prediction(uniqRes$INTER*-1, uniqRes$IsActive)
perfINTERuq <- performance(predINTERuq, 'tpr','fpr')

#plot in jpg format with a grey line with theoretical random results
jpeg("hivpr_Rinter_ROC.jpg")
plot(perfINTERuq,main="hivpr - ROC Curves",col="blue")
abline(0,1,col="grey")
dev.off()
#AUC (area under the curve)
auc_rdock <- performance(predINTERuq, "auc")
auc.area_rdock <- slot(auc_rdock, "y.values")[[1]]
cat("AUC: \n")
cat(auc.area_rdock)
cat("\n\n")

AUC: 
  0.7700965

#Enrichment Factors
EF_rdock <- perfINTERuq@y.values[[1]]/perfINTERuq@x.values[[1]]
EF_rdock_1 <- EF_rdock[which(perfINTERuq@x.values[[1]] > 0.01)[1]]
EF_rdock_20 <- EF_rdock[which(perfINTERuq@x.values[[1]] > 0.2)[1]]
cat("Enrichment Factor top1%:\n")
cat(EF_rdock_1)
cat("\n\n")

Enrichment Factor top1%:
  11.11817


cat("Enrichment Factor top20%:\n")
cat(EF_rdock_20)
cat("\n\n")

}