#source("lib/lib.validation_extern.R")
source("lib/lib.process_data.R")
source("lib/configuration.R")

runCombiData <- function() {
  # create combination datas from combining 2 types of descriptors (elementsv2 + SIFt/SIFtv2)
  for (pre in c("CASF12_core_", "CASF13_core_", "CASF14_core_", "CASF12_refined_", "CASF13_refined_", "CASF14_refined_") ) {
    print(pre)
#    createCombiData(path = path2Name, prefix = pre, descName = c("elementsv2", "SIFt"))
#    createCombiData(path = path2Name, prefix = pre, descName = c("elementsv2", "SIFtv2"))  
  }

  #for (pre in c("3PDQ_new_", "T36_JMJ_Xray_") ) {
  for (pre in c("DIG10.2_") ) {
    print(pre)  
    for (post in c("_XP", "_SP")) {      
    #for (post in c("_actives", "_inactives", "_test_gold")) {
      print(post)
      createCombiData(path = path2Name, prefix = pre, descName = c("elementsv2", "SIFt"), postfix = post)
      #createCombiData(path = path2Name, prefix = pre, descName = c("elementsv2", "SIFtv2"), postfix = post)
      processTestData(path = path2Name, prefix = pre, descName = "elementsv2-SIFt", postfix = post)
      #processTestData(path = path2Name, prefix = pre, descName = "elementsv2-SIFtv2", postfix = post)
    }
  }
}

# create only-metallo data for training purpose
createMetallo <- function(path, prefix, desc, cutoff = 12, binsize = 0) {
  dataPath = paste(path,prefix,"refined_",concatCutoffAndBinsize(desc, cutoff, binsize),".csv", sep="")
  listPath = paste(path,"ID_",prefix,"metallo_",concatCutoffAndBinsize(desc, cutoff, binsize),".csv", sep="")
  output   = paste(path,prefix,"metallo_",concatCutoffAndBinsize(desc, cutoff, binsize),".csv", sep="")
  
  filterDataRow(dataPath, listPath, output, nameIndex=1)  
}

runCombiData()

# create training and test set by checking the common rows of two sets and remove it from the training set
for (pre in dataSets ) {
  print(pre)
  #path = "/home/dat/WORK/DB/DESCRIPTORS/New/"
  #  createTrainingTestData(path, prefix= pre, desc = "elementsv2-SIFt", nameIndex = 1)
  #  createTrainingTestData(path, prefix= pre, desc = "elementsv2-SIFtv2", nameIndex = 1)
}

path = "/home/dat/WORK/DB/DESCRIPTORS/New/Processed/"
for (prefix in dataSets ) {
  print(prefix)
  for (desc in c("elementsv2-SIFt", "elementsv2-SIFtv2") ) {
    #createMetallo(path, prefix, desc)
  }
}