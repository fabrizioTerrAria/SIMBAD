
updateDB <- function(scenarioDF) {
	
	scenarioDF <- scenarioDF[,c("scenarioName", "directory", "year", "defaultBAU", "dailyComputation", "description", "dateCreation", "userName", "progress", "status", "isPublic")]
	write.csv2(scenarioDF, paste0(SCENDIR,"/userScenario.csv"), row.names = FALSE, quote = TRUE)
	
}


initializeDB <- function() {
	
  print(paste0(Sys.time()," - Inizializzo file degli scenari"))
  
	if (!file.exists(paste0(SCENDIR,"/userScenario.csv"))) {
		
		userScenario <- data.frame(scenarioName = character(0),
															 directory = character(0),
															 year = integer(0),
															 defaultBAU = logical(0), 
															 dailyComputation = logical(0),
															 description = character(0),
															 dateCreation = character(0),
															 userName = character(0),
															 progress = character(0),
															 status = character(0),
															 isPublic = logical(0))
		
		updateDB(userScenario)
		
	}
	
}

readScenarioFromDB <- function(userName, userGroup) {
	
	if (missing(userName) & missing(userGroup)) {
		
		#leggo tutti gli scenari
		userScenario <- read.csv2(paste0(SCENDIR,"/userScenario.csv"))
		
	} else {
		
		if (userGroup == "ADMIN") {
			
			#leggo tutti gli scenari
			userScenario <- read.csv2(paste0(SCENDIR,"/userScenario.csv"))
			
			# inserisco gli scenari dell'utente nelle prime posizioni
			userScenario <- rbind(userScenario[userScenario$userName %in% c(userName),],
														userScenario[!userScenario$userName %in% c(userName),])
			
			
		} else {
			
			# leggo solo gli scenari dell'utente oppure quelli pubblicati
			userScenario <- read.csv2(paste0(SCENDIR,"/userScenario.csv"))
			
			userScenario <- userScenario[userScenario$userName %in% c(userName) | userScenario$isPublic,]
			
			# maschero gli utenti
			userScenario$userName[!userScenario$userName %in% c(userName)] <- "Other user"
			
			# blocco le funzionalità non utilizzabili
			userScenario$isPublic[!userScenario$userName %in% c(userName)] <- NA
			userScenario$UpdateBtn[!userScenario$userName %in% c(userName)] <- NA
			userScenario$DeleteBtn[!userScenario$userName %in% c(userName)] <- NA
			
			# inserisco gli scenari dell'utente nelle prime posizioni
			userScenario <- rbind(userScenario[userScenario$userName %in% c(userName),],
														userScenario[!userScenario$userName %in% c(userName),])
			
		}
	  
	  if (nrow(userScenario)>0) {
	    
	    userScenario$UpdateBtn <- TRUE
	    userScenario$DeleteBtn <- TRUE
	    
	    
	  }
		
	}
	
	return(userScenario)
	
}

scenarioListfromDB <- function(userName, userGroup) {
	
	scenarioDF <- readScenarioFromDB(userName, userGroup)
	scenarioDF <- scenarioDF[scenarioDF$status == "Ok",]
	return(scenarioDF$scenarioName)
	
}

scenarioListAllfromDB <- function(userName, userGroup) {
  
  scenarioDF <- readScenarioFromDB(userName, userGroup)
  return(scenarioDF$scenarioName)
  
}

getScenarioDirectoryInDB <- function(scenarioNameIN) {
  
  print(paste0(Sys.time()," - Recupero la directory dello scenario: ", scenarioNameIN))
  scenarioDF <- readScenarioFromDB()
  desc <- scenarioDF$directory[scenarioDF$scenarioName %in% scenarioNameIN]
  
  return(desc)
  
}

getScenarioYearInDB <- function(scenarioNameIN) {
  
  print(paste0(Sys.time()," - Recupero anno dello scenario: ", scenarioNameIN))
  scenarioDF <- readScenarioFromDB()
  year <- scenarioDF$year[scenarioDF$scenarioName == scenarioNameIN]
  
  return(year)
  
}

isDefaultBAUScenarioInDB <- function(scenarioNameIN) {
  
  print(paste0(Sys.time()," - Recupero se scenario BAU di default: ", scenarioNameIN))
  scenarioDF <- readScenarioFromDB()
  check <- scenarioDF$defaultBAU[scenarioDF$scenarioName == scenarioNameIN]
  
  return(check)
  
}

isDailyComputationScenarioInDB <- function(scenarioNameIN) {
  
  print(paste0(Sys.time()," - Recupero se scenario BAU di default: ", scenarioNameIN))
  scenarioDF <- readScenarioFromDB()
  check <- scenarioDF$dailyComputation[scenarioDF$scenarioName == scenarioNameIN]
  
  return(check)
  
}

getScenarioDescriptionInDB <- function(scenarioNameIN) {
	
  print(paste0(Sys.time()," - Recupero descrizione dello scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	desc <- scenarioDF$description[scenarioDF$scenarioName == scenarioNameIN]
	
	return(desc)
	
}

getScenarioPublicFromDB <- function(userName, userGroup) {
	
	scenarioDF <- readScenarioFromDB(userName, userGroup)
	res <- ifelse(scenarioDF$userName %!in% c("System",userName),"public","")
	
	return(res)
	
}


getScenarioProgressInDB <- function(scenarioNameIN) {
  
  scenarioDF <- readScenarioFromDB()
  
  if (missing(scenarioNameIN)) {
    
    return(scenarioDF$progress)
    
  } else {
    
    progr <- scenarioDF$progress[scenarioDF$scenarioName == scenarioNameIN]  
    return(progr)
    
  }
  
}

updateProgressScenario <- function() { 
  
  scenarioDF <- readScenarioFromDB()
  
  progressFiles <- dir(SCENDIR, recursive=TRUE, full.names=TRUE, pattern="progress.txt")
  
  scenList <- list.dirs(path = SCENDIR, full.names = FALSE, recursive = FALSE)
  progressList <- as.numeric(sapply(progressFiles, function(f) read.table(f, col.names = FALSE)))
  
  scenarioDF$progress[na.omit(match(scenList,scenarioDF$directory))] <- progressList
  
  updateDB(scenarioDF)
  
}

updateStatusScenario <- function() { 
  
  scenarioDF <- readScenarioFromDB()
  
  statusFiles <- dir(SCENDIR, recursive=TRUE, full.names=TRUE, pattern="status.txt")
  
  scenList <- list.dirs(path = SCENDIR, full.names = FALSE, recursive = FALSE)
  statusList <- as.character(sapply(statusFiles, function(f) read.table(f, col.names = FALSE)))
  
  scenarioDF$status[na.omit(match(scenList,scenarioDF$directory))] <- statusList
  
  updateDB(scenarioDF)
  
}

updateScenarioInfoInDB <- function(scenarioNameIN,scenarioNameOUT,scenarioDescOUT) {
	
	scenarioDF <- readScenarioFromDB()
	scenarioDF[scenarioDF$scenarioName == scenarioNameIN,1:2] <- list(scenarioNameOUT, scenarioDescOUT)
	
	updateDB(scenarioDF)
	
}

updateScenarioPublicInDB <- function(scenarioNameIN, status) {
	
  print(paste0(Sys.time()," - Aggiorno info dello scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	scenarioDF[scenarioDF$scenarioName == scenarioNameIN,"isPublic"] <- status
	
	updateDB(scenarioDF)
	
}

deleteScenarioInDB <- function(scenarioNameIN) {
	
  print(paste0(Sys.time()," - Elimino lo scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	scenarioDF <- scenarioDF[!(scenarioDF$scenarioName == scenarioNameIN),] 
	
	updateDB(scenarioDF)
	
}

addScenarioInDB <- function(scenarioName, directory, year, defaultBAU, dailyComputation, scenarioDescription, userName) {
  
  print(paste0(Sys.time()," - Aggiungo lo scenario: ", scenarioName))
	dateCreation = strftime(Sys.time(),format = "%d %b %Y")
	scenarioDF <- readScenarioFromDB()
	progress <- 0
	status <- "Inizio calcolo"
	isPublic <- FALSE
	
	# aggiungo nuovo scenario
	scenarioDF[nrow(scenarioDF) + 1,] <- list(scenarioName, directory, year, defaultBAU, dailyComputation, scenarioDescription, dateCreation, userName, progress, status, isPublic)
	updateDB(scenarioDF)
	
}

deleteUserInDB <- function(userName) {
	
	scenarioDF <- readScenarioFromDB()
	scenarioDF <- scenarioDF[!(scenarioDF$userName == userName),] 
	
	updateDB(scenarioDF)
	
}








