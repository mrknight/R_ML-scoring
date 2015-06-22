source("lib/configuration.R")
source("lib/lib.analysis.R")

resultPath = "/Users/knight/Dropbox/data/"
#resultPath = "/home/dat/WORK/output/results/2015-06-23/"

IDPath = "/Users/knight/Dropbox/data/"
#IDPath = "/home/dat/WORK/DB/DESCRIPTORS/June-2015/process/"


testing <- function() {

IDFile = "DIG10.2_elementsv2-SIFt_c12b0-xscore_XP_process.csv"
#IDFile = "DIG10.2_elementsv2-SIFt_c12b0_asp_process.csv"

fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0-xscore_XP.csv"
#fileName = "CASF12_refined_DIG10.2_RoF-REP_elementsv2-SIFt_c12b0_asp.csv"

test = readWekaResult(resultPath, fileName)
test = addID(test, IDPath, IDFile)

#print(rank_bestpose(test, IDindex = 3))
print(list_top_bestpose(test, IDindex = 3))
rankpose = calc_average_bestpose(list_top_bestpose(test, IDindex = 3), IDindex = 3)
print(rankpose)
print(rank(-rankpose[,2]))
print(cor(TRUE_RANK_CSAR13, rank(-rankpose[,2]), method="spearman"))

#test = read.csv(paste(IDPath, "cs-confgen_all_goldscore.csv", sep=""))
#print(rank_bestpose(test, IDindex = 1))
#print(list_top_bestpose(test, numberOfPoses=1, IDindex = 1))
#rankpose = calc_average_bestpose(list_top_bestpose(test, numberOfPoses = 3, IDindex = 1), IDindex = 1)
#print(rankpose)
#print(rankpose[ order(-rankpose[,2]), ]) 
#print(rank(-rankpose[,2]))
#print(cor(TRUE_RANK_CSAR13, rank(-rankpose[,2]), method="spearman"))
}

testing()
#
analyse_result_MLscoring <- function(numberOfPoses, target) {
  for (training in dataSets) {
    for (classify in methods) {
      for (docking in dockingMethods) {
        
      }
    }
  }
  for (method in dockingMethods) {
    print(method)
    poseList = read.csv(paste(IDPath, "cs-confgen_all_", method, ".csv", sep=""))
    topPoses = calc_average_bestpose(list_top_bestpose(poseList, numberOfPoses, IDindex = 1), IDindex = 1) 
    print(topPoses)
    rankTopPoses = rank(-topPoses[,2])
    print(rankTopPoses)
    print(cor(TRUE_RANK_CSAR13, rankTopPoses, method="spearman"))
  }
}
  
# ranking based on average of numberOfPoses best poses
analyse_result_original_poses <- function(numberOfPoses) {
  for (method in dockingMethods) {
    print(method)
    poseList = read.csv(paste(IDPath, "cs-confgen_all_", method, ".csv", sep=""))
    topPoses = calc_average_bestpose(list_top_bestpose(poseList, numberOfPoses, IDindex = 1), IDindex = 1) 
    print(topPoses)
    rankTopPoses = rank(-topPoses[,2])
    print(rankTopPoses)
    print(cor(TRUE_RANK_CSAR13, rankTopPoses, method="spearman"))
  }
}

analyse_result_original_poses(1)