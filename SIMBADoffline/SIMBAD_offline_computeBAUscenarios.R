#
# SIMBAD Offline application
#

library(raster)

source("functions.R")
source("global.R")

outputPath <- "../data/BAU_SCENARIO/"

# Reading configBAU.csv file 
print(paste0(Sys.time()," - Reading configBAU.csv file ..."))

scenarioList <- read.csv2("SIMBADoffline/configBAU.csv")

for (scen in 1:nrow(scenarioList)) {
	
  scenario <- scenarioList$nameScenario[scen]
  type <- scenarioList$Type[scen]
  year <- scenarioList$year[scen]
  dailyComputation <- scenarioList$dailyComputation[scen]
  
	print(paste0("Processing scenario: ", scenario))
	
	scenDir <- file.path(outputPath,scenarioList$nameScenario[scen])

	dir.create(scenDir)

	if (type == 1) {
		
	  # DA FARE: calcolo con livelli di attività e fattori emissivi #####################
		
		
	} else {
		
	  # EMISSIONS
	  print(paste0(Sys.time(), " - Calcolo Emissioni"))
	  listBAU <- computeBAUScenario(year,dailyComputation,FALSE,0,0)
	  emissionsStack <- listBAU[[1]]
	  save(emissionsStack, file = paste0(scenDir,"/","emiRaster.rda"))
	  # writeRaster(emissionsStack, filename = paste0(emiDir,"/asc/",names(emissionsStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	  
	  # EMISSION VARIATIONS
	  redPercREF <- listBAU[[3]]
	  redPercREF <- cbind(PRECURSORS,redPercREF)
	  colnames(redPercREF) <- c("PREC",SECTORS[1:(length(SECTORS))])
	  write.csv2(redPercREF, file = paste0(scenDir,"/","emiReductionREF.csv"), row.names = F, quote = F)
	  
	  redPercP <- listBAU[[4]]
	  redPercP <- cbind(PRECURSORS,redPercP)
	  colnames(redPercP) <- c("PREC",SECTORS[1:(length(SECTORS))])
	  write.csv2(redPercP, file = paste0(scenDir,"/","emiReductionP.csv"), row.names = F, quote = F)

	  redPercQ <- listBAU[[5]]
	  redPercQ <- cbind(PRECURSORS,redPercQ)
	  colnames(redPercQ) <- c("PREC",SECTORS[1:(length(SECTORS))])
	  write.csv2(redPercQ, file = paste0(scenDir,"/","emiReductionQ.csv"), row.names = F, quote = F)

	  # CONCENTRATIONS
	  concentrationsStack <- listBAU[[2]]
	  if (dailyComputation) {
	    save(concentrationsStack, file = paste0(scenDir,"/","concRaster_daily.rda"))
	  } else {
	    save(concentrationsStack, file = paste0(scenDir,"/","concRaster.rda"))
	  }
	  # writeRaster(concentrationsStack, filename = paste0(concDir,"/asc/",names(concentrationsStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	  
	  # HIA
	  # print(paste0(Sys.time(), " - Calcolo Impatto Salute"))
	  # hiaStack <- computeHIA(scenario,concentrationsStack,FALSE,0,0)
	  # save(hiaStack, file = paste0(scenDir,"/","hiaRaster.rda"))
	  # writeRaster(hiaStack, filename = paste0(hiaDir,"/asc/",names(hiaStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	   

	}
	
	
	
	

	
}

