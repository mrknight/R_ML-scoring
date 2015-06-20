TRUE_RANK_CSAR13 = c(7, 8, 6, 4, 3, 5, 2, 1, 10, 9)

###########################################################################
# DESC: read the classify result from weka output
###########################################################################
readClassifyResult <- function(resultPath, fileName)  {
  result = read.csv(paste(resultPath, fileName, sep=""), skip = 4)
  return (result[,2:3])  
}

# add ligand/protein ID to result of weka
addID <- function(result, IDpath, IDfile) {
  IDdata        = read.csv(paste(IDPath, IDFile, sep=""), na.strings=c(".", "NA", "", "?"))  
  result["ID"]  = IDdata[,1]
  return (result)
}

#resultPath = "/Users/knight/Dropbox/data/"
resultPath = "/home/dat/WORK/output/results/2015-06-23/"
fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0-xscore_XP.csv"
#fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0_asp.csv"

test = readClassifyResult(resultPath, fileName)

IDPath = "/home/dat/WORK/output/results/2015-06-23/"
IDPath = "/home/dat/WORK/DB/DESCRIPTORS/June-2015/process/"

IDFile = "DIG10.2_elementsv2-SIFt_c12b0-xscore_XP_process.csv"
#IDFile = "DIG10.2_elementsv2-SIFt_c12b0_asp_process.csv"

test = addID(test, IDPath, IDFile)

# preprocess the scorelist by sorting it by the absolute value
preprocess_scorelist <- function(scorelist, scoreIndex = 2, IDindex) {
  # be sure that the scores are in absolute value
  scorelist[,scoreIndex] = abs(scorelist[,scoreIndex])
  # sorting the list
  scorelist = scorelist[ order(-scorelist[,scoreIndex]), ]
  # only getting the cs-ligand name
  if (!missing(IDindex)) {
    scorelist[,IDindex] = substr(scorelist[,IDindex],1,5)
  }
  return (scorelist)
}

rank_bestpose <- function(scorelist, IDindex) {
  scorelist = preprocess_scorelist(scorelist, IDindex = IDindex)
  return (unique(scorelist[,IDindex]))
}

rank_average_bestpose <- function(scorelist, numberOfPoses = 3, IDindex) {
  poselist = preprocess_scorelist(scorelist, IDindex = IDindex)
  library(hash)
  keep_pose = hash(unique(poselist[,IDindex]), numberOfPoses) 
  index = 0
  for (pose in poselist[,IDindex]) {
    index = index + 1
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
  return (temp)
}

print(rank_bestpose(test, IDindex = 3))
print(rank_average_bestpose(test, IDindex = 3))
rankpose = calc_average_bestpose(rank_average_bestpose(test, IDindex = 3), IDindex = 3)

print(rank(-rankpose[,2]))
print(cor(TRUE_RANK_CSAR13, rank(-rankpose[,2]), method="spearman"))
