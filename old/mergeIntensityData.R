mergeIntensityData <- function(dataPath, intensityPath, output) {
	data = read.csv(dataPath)
	pIC50 = read.csv(intensityPath,sep = "\t")
	nameIndex = length(data[1,])
	mergeData = merge(data, pIC50, by.x = nameIndex, by.y = 2)
	write.table(mergeData, file = output, sep = ",", row.names = FALSE)
}
createResult <- function(resultPath = "/home/dat/WORK/dev/data/Inna_davis72_RoF_RandCom_RandT_elements_b2.csv"
			, fullDataPath = "/home/dat/WORK/dev/data/Inna/hm_2esm_davis_all72_elements_c12b2.csv"
			, output = "/home/dat/WORK/dev/data/Inna_hm_2esm_davis72_RoF_RandCom_RandT_elements_b2_full.csv") {
	result = read.csv(resultPath)
	fullData = read.csv(fullDataPath)
	result["name"] = fullData[,1]
	data = result[,c(2,3,5)]
	data = data[order(-data[,2]),]
	write.table(data, file = output, sep = ",", row.names = FALSE)
}

#mergeIntensityData(dataPath = "/home/dat/WORK/dev/rfscore/bin/hm_2esm_all329_elements_c12b2_non.csv", intensityPath = "/home/dat/WORK/dev/data/Inna_all329_pIC50.csv", output = "/home/dat/WORK/dev/data/Inna/hm_2esm_all329_elements_c12b2.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/dev/rfscore/bin/hm_2esm_davis_all72_elements_c12b2_non.csv", intensityPath = "/home/dat/WORK/dev/data/Inna_davis72_pKd.csv", output = "/home/dat/WORK/dev/data/Inna/hm_2esm_davis_all72_elements_c12b2.csv")

#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_elements_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_elements_c12b12_non_all.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_sybyl_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_sybyl_c12b12_non_all.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_credo_c12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_credo_c12_non_all.csv")

#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_inactives_JMJD2A_elements_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_inactives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_inactives_elements_c12b12_non_all_gold.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_inactives_JMJD2A_sybyl_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_inactives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_inactives_sybyl_c12b12_non_all_gold.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_inactives_JMJD2A_credo_c12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_inactives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_inactives_credo_c12_non_all_gold.csv")

#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_elements_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_elements_c12b12_non_all_gold.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_sybyl_c12b12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_sybyl_c12b12_non_all_gold.csv")
#mergeIntensityData(dataPath = "/home/dat/WORK/DB/JMJD/desc/nowater/3PDQ_actives_JMJD2A_credo_c12_non.csv", intensityPath = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_dock_actives_gold.csv", output = "/home/dat/WORK/DB/DESCRIPTORS/JMJD/JMJD2A_actives_credo_c12_non_all_gold.csv")

#createResult(resultPath = "/home/dat/WORK/dev/data/Inna_all329_RoF_RandCom_RandT_elements_b2.csv"
#		, fullDataPath = "/home/dat/WORK/dev/data/Inna/hm_2esm_all329_elements_c12b2.csv"
#		, output = "/home/dat/WORK/dev/data/Inna_hm_2esm_all329_RoF_RandCom_RandT_elements_b2_full.csv")


