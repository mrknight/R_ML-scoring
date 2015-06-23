source("lib/configuration.R")
###########################################################################
# DESC: read the classify result from weka output
###########################################################################
readWekaResult <- function(resultPath, fileName)  {
  result = read.csv(paste(resultPath, fileName, sep=""), skip = 4)
  return (result[,2:3])  
}
###########################################################################
# DESC: read the classify result from weka output, same as read weka, but with other params
###########################################################################
readClassifyResult <- function(resultPath, data, evalSet, method, desc, cutoff = 12, binsize = 0)  {
  result = read.csv(paste(resultPath, data,"_", evalSet,"_", method,"_", concatCutoffAndBinsize(desc, cutoff, binsize), ".csv", sep=""), skip = 4)
  return (result[,2:3])  
}
###########################################################################
# add ligand/protein ID to result of weka
###########################################################################
addID <- function(result, IDPath, IDFile, IDIndex = 1) {
  IDdata        = read.csv(paste(IDPath, IDFile, sep=""), na.strings=c(".", "NA", "", "?"))  
  if (IDIndex == "last") {
    IDIndex = length(colnames(IDdata))
  }
  result["ID"]  = IDdata[,IDIndex]    
  return (result)
}

###########################################################################
# preprocess the scorelist by sorting it by the absolute value
###########################################################################
preprocess_scorelist <- function(scorelist, scoreIndex = 2, IDindex, splitChar = '_', decreasing = TRUE) {
  # be sure that the scores are in absolute value
  scorelist[,scoreIndex] = abs(scorelist[,scoreIndex])
  # sorting the list
  if (decreasing) {
    scorelist = scorelist[ order(-scorelist[,scoreIndex]), ]
  }
  else {
    scorelist = scorelist[ order(scorelist[,scoreIndex]), ]
  }
  # only getting the cs-ligand name
  if (!missing(IDindex)) {
    charPos = regexpr(splitChar, scorelist[, IDindex]) - 1
    scorelist[,IDindex] = substr(scorelist[,IDindex], 1, charPos)
  }
  return (scorelist)
}

###########################################################################
# ranking based on best pose, redundant with list_top_bestpose with numberOfPoses = 1
# \TODO: necessary? remove later!
###########################################################################
rank_bestpose <- function(scorelist, scoreIndex = 2, IDindex) {
  scorelist = preprocess_scorelist(scorelist, scoreIndex, IDindex = IDindex)
  return (unique(scorelist[,IDindex]))
}

###########################################################################
# list top numberOfPoses (standard = 3) best poses
###########################################################################
list_top_bestpose <- function(scorelist, numberOfPoses = 1, IDindex, decreasing = TRUE, preprocess = TRUE) {
  poselist = preprocess_scorelist(scorelist, IDindex = IDindex, decreasing = decreasing)
  library(hash)
  keep_pose = hash(unique(poselist[,IDindex]), numberOfPoses) 
  index = 0
  for (pose in poselist[,IDindex]) {
    index = index + 1 # index of current pose in poselist
    if (keep_pose[[pose]] > 0) {
      keep_pose[[pose]] = keep_pose[[pose]] - 1
    }
    else {
      poselist = poselist[-c(index),]     
      index = index - 1
    }
  }
  return (poselist)
} 


###########################################################################
# 
###########################################################################
calc_average_bestpose <- function(ranklist, scoreIndex = 2, IDindex) {
  library(hash)
  numberOfPoselist = hash(unique(ranklist[,IDindex]), 0) 
  totalScoreList = hash(unique(ranklist[,IDindex]), 0) 
  index = 0
  for (pose in ranklist[,IDindex]) {
    index = index + 1    
    numberOfPoselist[[pose]] = numberOfPoselist[[pose]] + 1
    totalScoreList[[pose]] = totalScoreList[[pose]] + ranklist[index, scoreIndex]
  }  
  temp = data.frame(data.frame(cbind(Name = NA, score = 1:length(keys(totalScoreList)))))
  index = 0
  for (pose in keys(totalScoreList)) {
    index = index + 1    
    totalScoreList[[pose]] = totalScoreList[[pose]] / numberOfPoselist[[pose]]
    temp[index,] = c(pose, totalScoreList[[pose]] )
  }
  # convert to float
  temp[,2] = as.numeric(temp[,2])
  # sort in the descending order
  #temp = temp[ order(-temp[,2]), ] 
  colnames(temp) = c("ID", "predicted")
  return (temp)
}

###########################################################################
# DESC: create heatmaps based on given metric
###########################################################################
createHeatmap <- function(metric = "r2.pearson", dataSets, methods, resultPath, evalSet, desc) {
  heatmap = matrix(NA, nrow = length(dataSets), ncol = length(methods))  
  rownames(heatmap) = dataSets
  colnames(heatmap) = methods 
  for (i in seq(length(dataSets)))
    # for (desc in descSets)
    for (j in seq(length(methods))) {
      data    = dataSets[i]
      method  = methods[j]      
      result = readClassifyResult(resultPath, data, evalSet, method, desc)
      heatmap[i,j] = sqrt(as.numeric(calcValidationMetric(result[,1], result[,2])[metric]))
    }
  if (metric == "r2.pearson") {
    metricName = "R_pearson"
  }
  if (metric == "r2m") {
    metricName = "R_m"
  }  
  if (metric == "rse") {
    metricName = "RMSE"
  }
  
  heatmap = heatmap[nrow(heatmap):1,]
  
  mainLabel = paste(metricName, "on", evalSet, "set")
  image(1:length(methods), 1:length(dataSets), z = t(heatmap), axes = FALSE, xlab = paste("Descriptor:", desc), ylab = "", main = mainLabel, cex.lab=2, cex.main = 2)
  axis(1, 1:length(methods), colnames(heatmap))
  axis(2, 1:length(dataSets), rownames(heatmap))
  for (x in 1:ncol(heatmap))
    for (y in 1:nrow(heatmap))
      text(x, y, round(heatmap[y,x], digits = 3), cex = 2)
  
  #xval <- formatC(heatmap, format="f", digits=3)
  #pal <- colorRampPalette(c(rgb(0.96,0.96,1), rgb(0.1,0.1,0.9)), space = "rgb")
  #col=pal,
  #x_hm <- heatmap.2(heatmap, Rowv=FALSE, Colv=FALSE, dendrogram="none", main=mainLabel, xlab= paste("Descriptor:", desc), ylab="", 
  #                  key=FALSE,  tracecol="#303030", trace="none", cellnote=xval, notecol="black", 
  #                  cexRow=0.7, cexCol=1.2, srtRow = 0, srtCol = 0) 
}