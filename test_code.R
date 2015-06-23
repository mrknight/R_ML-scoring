#lig <- unique(read.table("/home/dat/Downloads/ligands.txt")[,1]);
#uniqRes <- read.table("/home/dat/Downloads/dataforR_uq.txt",header=T);
#colnames(uniqRes)[1]="LigandName";
#uniqRes$IsActive <- as.numeric(uniqRes$LigandName %in% lig)
#predINTERuq <- prediction(uniqRes$INTER*-1, uniqRes$IsActive)

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


#data1 = read.csv("/home/dat/WORK/DB/DESCRIPTORS/DIG10.2/confgen/DIG10.2_elementsv2_c12b0_SP.csv", na.strings=c(".", "NA", "", "?"))
#data2 = read.csv("/home/dat/WORK/DB/DESCRIPTORS/DIG10.2/confgen/DIG10.2_SIFt_c12b0_SP.csv", na.strings=c(".", "NA", "", "?"))
#data1 = read.csv("/home/dat/WORK/DB/DESCRIPTORS/CASF/CASF14_refined_SIFt_c12b0.csv", na.strings=c(".", "NA", "", "?"))
#data2 = read.csv("/home/dat/WORK/DB/DESCRIPTORS/CASF/CASF14_refined_elementsv2_c12b0.csv", na.strings=c(".", "NA", "", "?"))
#nameIndex1 = length(data1[1,])
#nameIndex2 = length(data2[1,])
#score = read.csv("/home/dat/WORK/DB/DESCRIPTORS/DIG10.2/cs-confgen_all_SP.csv", na.strings=c(".", "NA", "", "?"))
#score = read.csv("/home/dat/WORK/DB/DESCRIPTORS/CASF/CASF14_refined_xscore.csv", na.strings=c(".", "NA", "", "?"))
#mergeData = merge(mergeData12, score, by.x = 1, by.y = 1)