###########################################################################
# DESC: read the classify result from weka output
###########################################################################
readClassifyResult <- function(resultPath, fileName)  {
  result = read.csv(paste(resultPath, fileName, sep=""), skip = 4)
  return (result[,2:3])  
}

TRUE_RANK_CSAR2013 = c(7, 8, 6, 4, 3, 5, 2, 1, 10, 9)

resultPath = "/Users/knight/Dropbox/data/"
fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0_SP.csv"
fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0_asp.csv"

test = readClassifyResult(resultPath, fileName)

inputFile = "DIG10.2_elementsv2-SIFt_c12b0-xscore_SP_process.csv"
inputFile = "DIG10.2_elementsv2-SIFt_c12b0_asp_process.csv"

data    = read.csv(paste(resultPath, inputFile, sep=""), na.strings=c(".", "NA", "", "?"))

test["ID"] = data[,1]

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

test = preprocess_scorelist(test)
#round(data[1,2],3) == test[1,1]