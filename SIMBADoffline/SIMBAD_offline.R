#
# SIMBAD Offline application
#

library(raster)

source("functions.R")
source("global.R")

outputPath <- "SIMBADoffline/output"

# Reading config.csv file 
print(paste0(Sys.time()," - Reading config.csv file ..."))

scenarioList <- read.csv2("SIMBADoffline/config.csv")

for (scen in 1:nrow(scenarioList)) {
	
  scenario <- scenarioList$nameScenario[scen]
  year <- scenarioList$year[scen]
  type <- scenarioList$Type[scen]
  reductionsFile <- scenarioList$reductionsFile[scen]
  dailyComputation <- scenarioList$dailyComputation[scen]
  updateBAU <- scenarioList$updateBAU[scen]
  reductionsBAUFile <- scenarioList$reductionsBAUFile[scen]
  
	print(paste0("Processing scenario: ", scenario))
	
	if (type == 1) {
		##### DA AGGIORNARE #########
		redPerc <- computeEmissionReduction(reductionsFile)
		
	} else {
		
		redPercEmi <- read.csv2(reductionsFile, sep = ";", dec = ".")
		redPerc <- redPercEmi[,2:(length(SECTORS))]
		
	}
	
	if (updateBAU) {
	  
	  if (typeBAU == 1) {
	  
	   ##### DA AGGIORNARE #########
	    
	  } else {
	    
	    redPercEmi <- read.csv2(reductionsBAUFile, sep = ";", dec = ".")
	    redPercBAU <- redPercEmi[,2:(length(SECTORS))]
	    
	  }
	  
	} else {
	  
	  redPercBAU <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
	  
	}
	
	scenDir <- paste0(outputPath,"/",scenarioList$nameScenario[scen])
	emiDir <- paste0(scenDir,"/","EMI")
	concDir <- paste0(scenDir,"/","CONC")
	hiaDir <- paste0(scenDir,"/","HIA")
	
	dir.create(scenDir)
	dir.create(emiDir)
	dir.create(paste0(emiDir,"/","asc"))
	dir.create(paste0(emiDir,"/","shp"))
	dir.create(concDir)
	dir.create(paste0(concDir,"/","asc"))
	dir.create(paste0(concDir,"/","shp"))
	dir.create(hiaDir)
	dir.create(paste0(hiaDir,"/","asc"))
	dir.create(paste0(hiaDir,"/","shp"))
	
	
	# EMISSIONS
	print(paste0(Sys.time(), " - Calcolo Emissioni"))
	listEMI <- computeEmissions(scenario,year,redPerc,updateBAU,redPercBAU,"offline",FALSE,0,0)
	
	# emissionsStackBAU <- listEMI[[1]]
	# save(emissionsStackBAU, file = paste0(scenDir,"/","emiRasterBAU.rda"))
	# writeRaster(emissionsStack, filename = paste0(emiDir,"/asc/",names(emissionsStackBAU),"_BAU.asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	
	emissionsStack <- listEMI[[2]]
	save(emissionsStack, file = paste0(scenDir,"/","emiRaster.rda"))
	writeRaster(emissionsStack, filename = paste0(emiDir,"/asc/",names(emissionsStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	
	print(paste0(Sys.time(), " - Disaggregazione spaziale Emissioni"))
	shpEmixExtraction <- interpolateRaster(scenario,emissionsStack,"sum","EMIX",FALSE,0,0)
	save(shpEmixExtraction, file = paste0(scenDir,"/","emiShp.rda"))
	writeSHP(shpEmixExtraction, "EMIX", path = paste0(emiDir,"/shp/"))
	
	# CONCENTRATIONS 
	print(paste0(Sys.time(), " - Calcolo Concentrazioni"))
	listCONC <- computeScenario(scenario,year,dailyComputation,redPerc,updateBAU,redPercBAU,"offline",FALSE,0,0)
	
	concentrationsStackBAU <- listCONC[[1]]
	save(concentrationsStackBAU, file = paste0(scenDir,"/","concRasterBAU.rda"))
	writeRaster(concentrationsStackBAU, filename = paste0(concDir,"/asc/",names(concentrationsStackBAU),"_BAU.asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)
	
	concentrationsStack <- listCONC[[2]]
	save(concentrationsStack, file = paste0(scenDir,"/","concRaster.rda"))
	writeRaster(concentrationsStack, filename = paste0(concDir,"/asc/",names(concentrationsStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)

	print(paste0(Sys.time(), " - Disaggregazione spaziale Concentrazioni"))
	shpConcExtraction <- interpolateRaster(scenario,concentrationsStack,"mean","CONC",FALSE,0,0)
	save(shpConcExtraction, file = paste0(scenDir,"/","concShp.rda"))
	writeSHP(shpConcExtraction, "CONC", path = paste0(concDir,"/shp/"))

	# HIA
	print(paste0(Sys.time(), " - Calcolo Impatto Salute"))
	hiaStack <- computeHIA(scenario,concentrationsStack,FALSE,0,0)
	save(hiaStack, file = paste0(scenDir,"/","hiaRaster.rda"))
	writeRaster(hiaStack, filename = paste0(hiaDir,"/asc/",names(hiaStack),".asc"), format = "ascii", overwrite = T, prj= T, bylayer = T)

	print(Sys.time(), " - Disaggregazione spaziale Impatto Salute")
	shpHiaExtraction <- interpolateRaster(scenario,hiaStack,"sum","HIA",FALSE,0,0)
	save(shpHiaExtraction, file = paste0(scenDir,"/","hiaShp.rda"))
	writeSHP(shpHiaExtraction, "HIA", path = paste0(hiaDir,"/shp/"))
	
}

