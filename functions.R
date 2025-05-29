library(ncdf4)
library(sp)
library(raster)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(highcharter)
library(jsonlite)


`%!in%` <- Negate(`%in%`)

createSpecieNames <- function(withDailyStats) {
  
  listNames <- c()
  for (i in 1:length(SPECIES_DESC)) {
    
    if (withDailyStats) {
      
      for (j in 1:length(SPECIES_STATS[[i]])) {
        varName <- paste(SPECIES_DESC[i], SPECIES_STATS[[i]][j])
        listNames <- c(listNames, varName) 
      }
      
    } else {
      
      # tengo solo la media annua
      varName <- paste(SPECIES_DESC[i], SPECIES_STATS[[i]][1])
      listNames <- c(listNames, varName) 
      
    }
    
     
    
  }
  
  return(listNames)
  
}

isAbsolute <- function(choice) {
  
  # check on DELTA_ABS_CHOICHES
  return(choice == 1 | choice == 2)
  
}

isPercentual <- function(choice) {
  
  # check on DELTA_ABS_CHOICHES
  return(choice == 4)
  
}

isDensity <- function(choice) {
  
  # check on EMI_DENS_CHOICHES
  return(choice == 2)
  
}

getSpecie <- function(specieName) {
  
  varName <- SPECIES[match(unlist(str_split(specieName, " "))[1], SPECIES_DESC)]
  return(varName)
  
}

getStat <- function(specieName) {
  
  stat <- gsub(paste0(unlist(str_split(specieName, " "))[1], " "), "", specieName)
  return(stat)
  
}

getSpecieUnit <- function(specieName) {
  
  if (getStat(specieName) == "# daily exceed") {
    unit <- "# of days exceed"
  } else {
    unit <- "[\u00b5g/m\u00b3]"
  }
  
  # unit <- unlist(SPECIES_UNIT)[match(specieName, createSpecieNames(withDailyStats))]
  return(unit)
  
}

nearestPastYear <- function(year) {
  
  nRanges <- length(YEAR_STEPS) - 1
  
  for (i in 1:nRanges) {
    
    range <- seq(YEAR_STEPS[i], YEAR_STEPS[i+1])
    
    if (year %in% range) {
      return(min(range))
    }
    
  }
  
  return(NA)
  
}

nearestFutureYear <- function(year) {
  
  nRanges <- length(YEAR_STEPS) - 1
  
  for (i in 1:nRanges) {
    
    range <- seq(YEAR_STEPS[i], YEAR_STEPS[i+1])
    
    if (year %in% range) {
      return(max(range))
    }
    
  }
  
  return(NA)
  
}


computeBAUScenario <- function(year,dailyComputation,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeBAUScenario per lo scenario'anno: ", year))
  
  A <- nearestPastYear(year)
  B <- nearestFutureYear(year)
  
  tau <- (year - A)/(B - A)
  
  redPerc <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
  scenario <- paste0("BAU_",year)
  
  emiBaseE <- computeBaseYearEmissions(scenario,A,redPerc,FALSE,0,0)
  emiBaseF <- computeBaseYearEmissions(scenario,B,redPerc,FALSE,0,0)
  
  if (A != YEAR_STEPS[1]) {
    emiBaseREF <- computeBaseYearEmissions(scenario,YEAR_STEPS[1],redPerc,FALSE,0,0)
  } else {
    emiBaseREF <- emiBaseE
  }
  
  k <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente usato per la proiezione delle emissioni associate al settore,precursore i,j  dal primo caso di riferimento al secondo  

  p <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal primo anno di riferimento all’anno di interesse, tale per cui G_j=p_j E_j
  pREF <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal 2017 all’anno di interesse, tale per cui G_j=p_j E_j
  q <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal secondo anno di riferimento all’anno di interesse, tale per cui G_j=q_j F_j
  
  emiBaseP <- stack()
  emiBaseQ <- stack()
  emiScen <- stack()
  final <- stack()
  delta <- stack()
  deltaP <- stack()
  
  # concScen <- stack()
  
  for (i in 1:length(SECTORS_ID)) {
    
    # print(paste0(Sys.time()," - Computing emissions for sector: ", SECTORS[i]))
    
    for (j in 1:length(PRECURSORS)) {
      
      var.name <- paste(PRECURSORS[j],SECTORS_ID[i], sep = "_")
      
      k[j,i] <- sum(values(emiBaseF[[var.name]]), na.rm = TRUE) / sum(values(emiBaseE[[var.name]]), na.rm = TRUE)
      
      if (is.na(k[j,i])) k[j,i] <- 0
      
      p[j,i] <- 1 - tau * (1 - k[j,i])
      q[j,i] <- p[j,i]/k[j,i]
      
      
      if (k[j,i] == 0) {
        
        emiBaseP <- addLayer(emiBaseP, p[j,i] * emiBaseE[[var.name]])
        totEmi <- emiBaseP[[var.name]]
        
      } else {
        
        emiBaseP <- addLayer(emiBaseP, p[j,i] * emiBaseE[[var.name]])
        emiBaseQ <- addLayer(emiBaseQ, q[j,i] * emiBaseF[[var.name]])
        
        totEmi <- (emiBaseP[[var.name]] + emiBaseQ[[var.name]]) / 2
        
      }
      
      # variazioni rispetto al 2017
      pREF[j,i] <- sum(values(totEmi), na.rm = TRUE) / sum(values(emiBaseREF[[var.name]]), na.rm = TRUE)
      
      deltaEmi <- totEmi - emiBaseREF[[var.name]]
      deltaPerc <- deltaEmi / (totEmi - deltaEmi) * 100
      
      final <- addLayer(final, totEmi)
      delta <- addLayer(delta, deltaEmi)
      deltaP <- addLayer(deltaP, deltaPerc)
      
    }
    
  }
  
  emiScen <- addLayer(final, delta, deltaP)
  emiScen <- round(emiScen, digits = 3)
  
  combNames <- paste(PRECURSORS, rep(SECTORS_ID, each = length(PRECURSORS)), sep = "_")
  names(emiScen) <- paste0(rep(c("","delta_","deltaP_"), each = length(combNames)), combNames)
  
  # CALCOLO LE CONCENTRAZIONI
  
  # nel caso di k = 0, q diventa Inf
  q[is.infinite(q)] <- 0
  
  redPercREF <- (pREF - 1) * 100 # riduzioni con il segno meno
  redPercP <- (p - 1) * 100 # riduzioni con il segno meno
  redPercQ <- (q - 1) * 100 # riduzioni con il segno meno
  
  redPercREF[is.na(redPercREF)] <- 0
  redPercP[is.na(redPercP)] <- 0
  redPercQ[is.na(redPercQ)] <- 0
  
  concScen <- stack()
  
  if (dailyComputation) {
    
    concBaseP <- computeBaseYearConcentrationsDaily(scenario,A,redPercP,FALSE,0,0)
    concBaseQ <- computeBaseYearConcentrationsDaily(scenario,B,redPercQ,FALSE,0,0)
    
    # redPerc <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
    
    # if (A != YEAR_STEPS[1]) {
    #   concBaseREF <- computeBaseYearConcentrationsDaily(scenario,YEAR_STEPS[1],redPerc,FALSE,0,0)
    # } else {
    #   concBaseREF <- concBaseP
    # }
    
    for (n in 1:length(SPECIES)) {
      
      var.name <- paste0(SPECIES[n],"_day",1:365)
      
      finalConc <- (1 - tau) * concBaseP[[var.name]] + tau * concBaseQ[[var.name]]
      finalConc <- round(finalConc, digits = 3)
      names(finalConc) <- paste0(SPECIES[n],"_day",1:nlayers(finalConc))
      
      concScen <- addLayer(concScen, finalConc)

    }

  } else {
    
    concBaseP <- computeBaseYearConcentrations(scenario,A,redPercP,FALSE,0,0)
    concBaseQ <- computeBaseYearConcentrations(scenario,B,redPercQ,FALSE,0,0)
    
    # redPerc <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
    
    # if (A != YEAR_STEPS[1]) {
    #   concBaseREF <- computeBaseYearConcentrations(scenario,YEAR_STEPS[1],redPerc,FALSE,0,0)
    # } else {
    #   concBaseREF <- concBaseP
    # }
    
    for (n in 1:length(SPECIES)) {
      
      var.name <- SPECIES[n]
      
      finalConc <- (1 - tau) * concBaseP[[var.name]] + tau * concBaseQ[[var.name]]
      # deltaConc <- -(concBaseREF[[var.name]] - finalConc)
      # deltaPerc <- deltaConc / concBaseREF[[var.name]] * 100
      # deltaPerc[deltaPerc == Inf] <- 0 
      # names(finalConc) <- paste0(SPECIES[[n]],"_day",rep(1:nlayers(finalConc)))
      finalConc <- round(finalConc, digits = 3)
      concScen <- addLayer(concScen, finalConc)
      # delta <- addLayer(delta, deltaConc)
      # deltaP <- addLayer(deltaP, deltaConc)
      
      
    }
    
    names(concScen) <- SPECIES
    
  }

  # concScen <- addLayer(final, delta, deltaP)
  # concScen <- final
  # concScen <- round(concScen, digits = 3)
  
  # names(concScen) <- c(SPECIES,paste0("delta_",SPECIES),paste0("deltaP_",SPECIES))
  # names(concScen) <- SPECIES
  
  return(list(emiScen, concScen, redPercREF, redPercP, redPercQ))
  
}


computeScenario <- function(scenario,year,dailyComputation,redPerc,updateBAU,redPercBAU,runType,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeScenario per lo scenario: ", scenario, " - anno: ", year))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute concentrations ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
    nSteps <- ifelse(updateBAU,5,4)
    
  }
  
  A <- nearestPastYear(year)
  B <- nearestFutureYear(year)
  
  tau <- (year - A)/(B - A)

  emiBaseE <- getBAUEmissions(A)
  emiBaseF <- getBAUEmissions(B)
  emiSCEN <- getEmissions(scenario,runType)
  
  if (A != YEAR_STEPS[1]) {
    emiBaseREF <- getBAUEmissions(YEAR_STEPS[1])
  } else {
    emiBaseREF <- emiBaseE
  }
  
  p <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal primo anno di riferimento all’anno di interesse, tale per cui G_j=p_j E_j
  pREF <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal 2017 all’anno di interesse, tale per cui G_j=p_j E_j
  q <- array(0, c(length(PRECURSORS),length(SECTORS_ID))) # Coefficiente di proiezione delle emissioni associate al settore,precursore i,j dal secondo anno di riferimento all’anno di interesse, tale per cui G_j=q_j F_j

  concScen <- stack()
  
  for (i in 1:length(SECTORS_ID)) {
    
    # print(paste0(Sys.time()," - Computing emissions for sector: ", SECTORS[i]))
    
    for (j in 1:length(PRECURSORS)) {
      
      var.name <- paste(PRECURSORS[j],SECTORS_ID[i], sep = "_")
      
      pREF[j,i] <- sum(values(emiSCEN[[var.name]]), na.rm = TRUE) / sum(values(emiBaseREF[[var.name]]), na.rm = TRUE)
      p[j,i] <- sum(values(emiSCEN[[var.name]]), na.rm = TRUE) / sum(values(emiBaseE[[var.name]]), na.rm = TRUE)
      q[j,i] <- sum(values(emiSCEN[[var.name]]), na.rm = TRUE) / sum(values(emiBaseF[[var.name]]), na.rm = TRUE)

    }
    
  }
  
  if (setProgress) {
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }

  # CALCOLO LE CONCENTRAZIONI
  p[is.infinite(p)] <- 0
  q[is.infinite(q)] <- 0
  
  redPercREF <- (pREF - 1) * 100 # riduzioni con il segno meno
  redPercP <- (p - 1) * 100 # riduzioni con il segno meno
  redPercQ <- (q - 1) * 100 # riduzioni con il segno meno
  
  redPercREF[is.na(redPercREF)] <- 0
  redPercP[is.na(redPercP)] <- 0
  redPercQ[is.na(redPercQ)] <- 0
  
  
  final <- stack()
  finalBAU <- stack()
  delta <- stack()
  deltaP <- stack()
  
  if (dailyComputation) {
    
    concBaseP <- computeBaseYearConcentrationsDaily(scenario,A,redPercP,FALSE,0,0)
    
    if (setProgress) {
      
      progressInc <- progressInc + (progressEnd - progressStart) / nSteps
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
      
    }
    
    concBaseQ <- computeBaseYearConcentrationsDaily(scenario,B,redPercQ,FALSE,0,0)
    
    if (setProgress) {
      
      progressInc <- progressInc + (progressEnd - progressStart) / nSteps
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
      
    }
    
    # concBaseREF <- getBAUConcentrations(YEAR_STEPS[1],dailyComputation)
    
    if (updateBAU) {
      
      concBaseBAU <- computeBaseYearConcentrationsDaily(scenario,YEAR_STEPS[1],redPercBAU,FALSE,0,0)
      
      if (setProgress) {
        
        progressInc <- progressInc + (progressEnd - progressStart) / nSteps
        write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
        
      }
      
    } else {
      
      concBaseBAU <- getBAUConcentrations(year,dailyComputation)
      
    }
    
    
    
    for (n in 1:length(SPECIES)) {
      
      print(paste0(Sys.time(), " - Calcolo concentrazioni finali per: ", SPECIES[n]))
      
      var.name <- paste0(SPECIES[n],"_day",1:365)
      
      finalConc <- (1 - tau) * concBaseP[[var.name]] + tau * concBaseQ[[var.name]]
      finalConcBAU <- concBaseBAU[[var.name]]
      
      if ("yearly avg" %in% SPECIES_STATS[[n]]) {

        # calcolo la media annua
        finalConcYearlyAvg <- calc(finalConc, mean, na.rm = T)
        finalConcBAUYearlyAvg <- calc(finalConcBAU, mean, na.rm = T)
        
        deltaConc <- -(finalConcBAUYearlyAvg - finalConcYearlyAvg)
        deltaPerc <- deltaConc / finalConcBAUYearlyAvg * 100
        deltaPerc[deltaPerc == Inf] <- 0
        
        final <- addToStack(finalConcYearlyAvg, final)
        names(final)[length(names(final))] <- SPECIES[n]
        
        finalBAU <- addToStack(finalConcBAUYearlyAvg, finalBAU)
        names(finalBAU)[length(names(finalBAU))] <- paste0("bau_",SPECIES[n])

        delta <- addToStack(deltaConc, delta)
        names(delta)[length(names(delta))] <- paste0("delta_",SPECIES[n])

        deltaP <- addToStack(deltaPerc, deltaP)
        names(deltaP)[length(names(deltaP))] <- paste0("deltaP_",SPECIES[n])

      }
      
      if ("# daily exceed" %in% SPECIES_STATS[[n]]) {

        # calcolo il numero di superamenti giornalieri
        limit <- SPECIES_DLY_LIMITS[n]
        finalConcDailyExceed <- calc(finalConc, function(x) sum(x > limit, na.rm = TRUE))

        final <- addToStack(finalConcDailyExceed, final)
        names(final)[length(names(final))] <- paste0(SPECIES[n],"_dylExceed")
        
        finalBAUConcDailyExceed <- calc(finalConcBAU, function(x) sum(x > limit, na.rm = TRUE))
        
        finalBAU <- addToStack(finalBAUConcDailyExceed, finalBAU)
        names(finalBAU)[length(names(finalBAU))] <- paste0("bau_",SPECIES[n],"_dylExceed")

      }
      
    }
    
    
  } else {
    
    concBaseP <- computeBaseYearConcentrations(scenario,A,redPercP,FALSE,0,0)
    
    if (setProgress) {
      
      progressInc <- progressInc + (progressEnd - progressStart) / nSteps
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
      
    }
    
    concBaseQ <- computeBaseYearConcentrations(scenario,B,redPercQ,FALSE,0,0)
    
    if (setProgress) {
      
      progressInc <- progressInc + (progressEnd - progressStart) / nSteps
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
      
    }
    
    # concBaseREF <- getBAUConcentrations(YEAR_STEPS[1],dailyComputation)
    
    if (updateBAU) {
      
      # riduzioni del BAU rispetto al 2017
      concBaseBAU <- computeBaseYearConcentrations(scenario,YEAR_STEPS[1],redPercBAU,FALSE,0,0)
      
      if (setProgress) {
        
        progressInc <- progressInc + (progressEnd - progressStart) / nSteps
        write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
        
      }
      
    } else {
      
      concBaseBAU <- getBAUConcentrations(year,dailyComputation)
      
    }
    
    
    for (n in 1:length(SPECIES)) {
      
      var.name <- SPECIES[n]
      
      # riduzioni dello scenario rispetto al BAU
      finalConc <- (1 - tau) * concBaseP[[var.name]] + tau * concBaseQ[[var.name]]
      finalConcBAU <- concBaseBAU[[var.name]]
      deltaConc <- -(finalConcBAU - finalConc)
      deltaPerc <- deltaConc / concBaseBAU[[var.name]] * 100
      deltaPerc[deltaPerc == Inf] <- 0 
      
      final <- addLayer(final, finalConc)
      finalBAU <- addLayer(finalBAU, finalConcBAU)
      delta <- addLayer(delta, deltaConc)
      deltaP <- addLayer(deltaP, deltaConc)
      
      names(final)[length(names(final))] <- paste0(SPECIES[n])
      names(finalBAU)[length(names(finalBAU))] <- paste0("bau_", SPECIES[n])
      names(delta)[length(names(delta))] <- paste0("delta_", SPECIES[n])
      names(deltaP)[length(names(deltaP))] <- paste0("deltaP_",SPECIES[n])
      
      
    }
    
    
  }

  concScen <- addLayer(final, finalBAU, delta, deltaP)
  concScen <- round(concScen, digits = 3)
  
  # names(concScen) <- c(SPECIES,paste0("bau_",SPECIES),paste0("delta_",SPECIES),paste0("deltaP_",SPECIES))
  
  if (setProgress) {
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }
  
  return(list(concBaseBAU, concScen, redPercBAU, redPercREF, redPercP, redPercQ))
  
}


readCAMXfile <- function(CAMXname,varName) {
	
	nc <- nc_open(CAMXname)
	# resX  <- ncatt_get(nc,0,"XCELL")$value
	# resY  <- ncatt_get(nc,0,"YCELL")$value
	# xMin  <- ncatt_get(nc,0,"XORIG")$value
	# yMin  <- ncatt_get(nc,0,"YORIG")$value
	nCols <- ncatt_get(nc,0,"NCOLS")$value
	nRows <- ncatt_get(nc,0,"NROWS")$value
	# xCent <- ncatt_get(nc,0,"XCENT")$value
	# yCent <- ncatt_get(nc,0,"YCENT")$value
	# lat1  <- ncatt_get(nc,0,"P_ALP")$value
	# lat2  <- ncatt_get(nc,0,"P_BET")$value
	
	varExists <- varName %in% names(nc$var)
	
	# crs.lcc <- CRS(paste0("+proj=lcc +lat_1=",lat1," +lat_2=",lat2," +lat_0=",round(yCent,digits = 3)," +lon_0=",round(xCent, digits = 3)," +datum=WGS84"))
	# 
	# xMax <- xMin + resX * nCols
	# yMax <- yMin + resY * nRows
	
	if (varExists) {
		
		# r <- raster::raster(CAMXname, var = varName)
		# extent(r) <- extent(xMin, xMax, yMin, yMax)
		data <- ncvar_get(nc,var=varName)
		
	} else {
		
	  print(paste0(Sys.time(), " - Variable: ", varName, " not found in file: ", CAMXname))
		# r <- raster(nrows=nRows, ncols=nCols, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax, vals=0)
		data <- array(0,dim=c(nCols,nRows))
		
	}
	
	# projection(r) <- crs.lcc
	
	nc_close(nc)
	return(data)
	
}

readCAMXfileDaily <- function(CAMXname,varName) {
  
	nc <- nc_open(CAMXname)
	# resX  <- ncatt_get(nc,0,"XCELL")$value
	# resY  <- ncatt_get(nc,0,"YCELL")$value
	# xMin  <- ncatt_get(nc,0,"XORIG")$value
	# yMin  <- ncatt_get(nc,0,"YORIG")$value
	nCols <- ncatt_get(nc,0,"NCOLS")$value
	nRows <- ncatt_get(nc,0,"NROWS")$value
	# xCent <- ncatt_get(nc,0,"XCENT")$value
	# yCent <- ncatt_get(nc,0,"YCENT")$value
	# lat1  <- ncatt_get(nc,0,"P_ALP")$value
	# lat2  <- ncatt_get(nc,0,"P_BET")$value
	
	varExists <- varName %in% names(nc$var)
	# 
	# crs.lcc <- CRS(paste0("+proj=lcc +lat_1=",lat1," +lat_2=",lat2," +lat_0=",round(yCent,digits = 3)," +lon_0=",round(xCent, digits = 3)," +datum=WGS84"))
	# 
	# xMax <- xMin + resX * nCols
	# yMax <- yMin + resY * nRows
	
	if (varExists) {
		
		data <- ncvar_get(nc,var=varName)
		
	} else {
		
	  print(paste0(Sys.time(), " - Variable: ", varName, " not found in file: ", CAMXname))
		data <- array(0,dim=c(nCols,nRows,365))
		
	}
	
	nc_close(nc)
	return(data)
  
}


readEMIXptfile <- function(CAMXname,varName) {
	
	nc <- nc_open(CAMXname)
	resX  <- ncatt_get(nc,0,"XCELL")$value
	resY  <- ncatt_get(nc,0,"YCELL")$value
	xMin  <- ncatt_get(nc,0,"XORIG")$value
	yMin  <- ncatt_get(nc,0,"YORIG")$value
	nCols <- ncatt_get(nc,0,"NCOLS")$value
	nRows <- ncatt_get(nc,0,"NROWS")$value
	xCent <- ncatt_get(nc,0,"XCENT")$value
	yCent <- ncatt_get(nc,0,"YCENT")$value
	lat1  <- ncatt_get(nc,0,"P_ALP")$value
	lat2  <- ncatt_get(nc,0,"P_BET")$value
	
	varExists <- varName %in% names(nc$var)
	
	crs.lcc <- CRS(paste0("+proj=lcc +lat_1=",lat1," +lat_2=",lat2," +lat_0=",round(yCent,digits = 3)," +lon_0=",round(xCent, digits = 3)," +datum=WGS84"))
	
	xMax <- xMin + resX * nCols
	yMax <- yMin + resY * nRows
	
	if (varExists) {
		
		xcoord <- ncvar_get(nc,"xcoord")
		ycoord <- ncvar_get(nc,"ycoord")
		var <- ncvar_get(nc,varName)
		xy <- cbind(xcoord,ycoord)
		
		r <- raster(nrows=nRows, ncols=nCols, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax, vals=0)
		r <- rasterize(xy, r, var, fun=sum)
		
	} else {
		
	  print(paste0(Sys.time(), " - Variable: ", varName, " not found in file: ", CAMXname))
		r <- raster(nrows=nRows, ncols=nCols, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax, vals=0)
		
	}
	
	projection(r) <- crs.lcc
	nc_close(nc)
	
	r <- raster2matrix(r)
	return(r)
	
}


addToStack <- function(rasterName,stackName) {
	# funzione che aggiunge un layer ad un raster stack
	# se il raster stack non esiste, lo crea
	
	if (nlayers(stackName) == 0) {
		stackName <- stack(rasterName)
	} else {
		stackName <- addLayer(stackName, rasterName)
	}
	
	return(stackName)
	
}


computeEmissionReduction <- function(redPercAct) {
	
	redPercAct <- read.csv2(redPercAct, sep = ";", dec = ".")
	emiFactors <- read.csv2(emissFactors.filename, sep = ";", dec = ".")
	
	data <- left_join(emiFactors,redPercAct, by = c("Settore","Tipo","Combustibile"), suffix = c("", ".y"))
	
	dataBC <- data %>% 
		mutate(across(PRECURSORS, ~ .x * Basecase)) %>% 
		group_by(Settore) %>% 
		summarise_at(PRECURSORS, sum)
	
	dataRed <- data %>% 
		mutate(across(PRECURSORS, ~ .x * Basecase * ( 100 - RiduzionePerc) / 100 )) %>% 
		group_by(Settore) %>% 
		summarise_at(PRECURSORS, sum)
	
	redPerc <- (1 - (dataRed[,PRECURSORS] / dataBC[,PRECURSORS])) * 100
  redPerc[is.na(redPerc)] <- 0
  redPerc <- round(redPerc, digits = 3)
	redPerc <- cbind(dataBC[,1], redPerc)
	redPerc <- redPerc[match(SECTORS[1:4], redPerc$Settore),]
	
	return(t(redPerc[,PRECURSORS]))
	
}

getEmissions <- function(scenario,runType) {
  
  if (runType == "offline") {
    
    directory <- file.path("SIMBADoffline/output",scenario)
    filename <- file.path(directory,"emiRaster.rda")
    
  } else {
    
    directory <- getScenarioDirectoryInDB(scenario)
    filename <- file.path(SCENDIR,directory,"emiRaster.rda")
    
  }
  
  emissionsStack <- base::get(load(filename))
  
  return(emissionsStack)
  
}

getBAUScenarioDirectory <- function(year) {
  
  dir <- file.path(BAUSCENDIR,paste0(year,"_BAU"))
  return(dir)
  
}

getBAUEmissions <- function(year) {
  
  directory <- getBAUScenarioDirectory(year)
  
  filename <- file.path(directory,"emiRaster.rda")
  emissionsStack <- get(load(filename))

  return(emissionsStack)
  
}

getBAUConcentrations <- function(year,dailyComputation) {
  
  directory <- getBAUScenarioDirectory(year)
  
  if (dailyComputation) {
    filename <- file.path(directory,"concRaster_daily.rda")
  } else {
    filename <- file.path(directory,"concRaster.rda")
  }
  
  concentrationsStack <- get(load(filename))
  
  return(concentrationsStack)
  
}

applyRedPercEmissions <- function(emissionsStack, redPerc, type) {
  
  print(paste0(Sys.time(), " - Lancio applyRedPercEmissions... "))
  
  final <- stack()
  finalBAU <- stack()
  delta <- stack()
  deltaP <- stack()
  
  # adding new column in the reductions matrix for Others sectors
  redPerc <- cbind(redPerc, 0)
  
  
  for (i in 1:length(SECTORS_ID)) {
    
    for (j in 1:length(PRECURSORS)) {
      
      var.name <- paste0(PRECURSORS[j], "_", SECTORS_ID[i])
      totEmiBAU <- emissionsStack[[var.name]]
      totEmi <- totEmiBAU * (100 + redPerc[j,i]) / 100  # Riduzioni con il segno negativo
      deltaEmi <- emissionsStack[[var.name]] * redPerc[j,i] / 100
      deltaPerc <- deltaEmi / (totEmi - deltaEmi) * 100
      
      final <- addToStack(totEmi, final)
      finalBAU <- addToStack(totEmiBAU, finalBAU)
      delta <- addToStack(deltaEmi, delta)
      deltaP <- addToStack(deltaPerc, deltaP)
      
    }
    
  }
  
  combNames <- paste(PRECURSORS, rep(SECTORS_ID, each = length(PRECURSORS)), sep = "_")
  
  if (type == "BAU") {
    final <- addLayer(final, delta, deltaP)
    names(final) <- paste0(rep(c("", "delta_", "deltaP_"), each = length(combNames)), combNames)
  } else {
    final <- addLayer(final, finalBAU, delta, deltaP)
    names(final) <- paste0(rep(c("", "bau_", "delta_", "deltaP_"), each = length(combNames)), combNames)
  }

  return(final)
  
}


computeBaseYearEmissions <- function(scenario,baseYear,redPerc,setProgress,progressStart,progressEnd) {
	
  print(paste0(Sys.time(), " - Lancio computeBaseYearEmissions per lo scenario: ", scenario, " - anno: ", baseYear))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute concentrations ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }

	final <- stack()
	delta <- stack()
	deltaP <- stack()
	
	# adding new column in the reductions matrix for Others sectors
	redPerc <- cbind(redPerc, 0)
	
	emiIndex <- match(baseYear,YEAR_STEPS)
	
	# creo una matrice vuota delle dimensioni corrette
	xx <- readCAMXfile(emibase.ar.filenames[[1]][1], "NO2")
	xx[] <- 0
	
	for (i in 1:length(SECTORS_ID)) {
		
		print(paste0(Sys.time()," - Computing emissions for sector: ", SECTORS[i]))
		
		for (j in 1:length(PRECURSORS)) {
			
			finalEmi <- stack()
			print(paste0(Sys.time(), " - Precursor: ", PRECURSORS[j]))
			
			for (sp in 1:length(PRECURSORS_SPEC[[j]])) {
				
				var.name <- PRECURSORS_SPEC[[j]][sp]
				print(paste0(Sys.time(), "   + Variable name: ", var.name))
				
				# Reading areal emissions
				if (is.na(emibase.ar.filenames[[emiIndex]][i])) {
				  ar <- xx
				} else {
					ar <- readCAMXfile(emibase.ar.filenames[[emiIndex]][i], var.name)
				}
				
				# Reading point sources emissions
				if (is.na(emibase.pt.filenames[[emiIndex]][i])) {
					pt <- xx
				} else {
					pt <- readEMIXptfile(emibase.pt.filenames[[emiIndex]][i], var.name)
					pt[is.na(pt)] <- 0   # MPC Per non perdere i dati areali sulle celle senza dati puntuali
				}
				
				tot <- (ar + pt) * PRECURSORS_MOLWT[[j]][sp] / 10^6
				
				# converto la matrice in raster
				tot <- matrix2raster(tot)
				
				finalEmi <- addToStack(tot, finalEmi)
				
			}
			
			totEmi <- calc(finalEmi, sum) * (100 + redPerc[j,i]) / 100  # Riduzioni con il segno negativo
			deltaEmi <- (calc(finalEmi, sum) * redPerc[j,i] / 100)  
			deltaPerc <- deltaEmi / (totEmi - deltaEmi) * 100
	
			final <- addToStack(totEmi, final)
			delta <- addToStack(deltaEmi, delta)
			deltaP <- addToStack(deltaPerc, deltaP)
			
			if (setProgress) {
			  
			  progressInc <- progressInc + (progressEnd - progressStart) / (length(SECTORS_ID) * length(PRECURSORS))
			  write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
			  
			}
			
			
		}
		
	}
	
	final <- addLayer(final, delta, deltaP)
	# final <- round(final, digits = 3)
	
	combNames <- paste(PRECURSORS, rep(SECTORS_ID, each = length(PRECURSORS)), sep = "_")
	names(final) <- paste0(rep(c("","delta_","deltaP_"), each = length(combNames)), combNames)
	
	return(final)

}


computeEmissions <- function(scenario,year,redPerc,updateBAU,redPercBAU,runType,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeEmissions per lo scenario: ", scenario, " - anno: ", year))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute emissions ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
    nSteps <- 2
    
  }
  
  if (updateBAU) {
    
    emissionStackREF <- getBAUEmissions(2017)
    emissionStackBAU <- applyRedPercEmissions(emissionStackREF, redPercBAU, "BAU")
    
  } else {
    
    emissionStackBAU <- getBAUEmissions(year)
    
  }
  
  if (setProgress) {
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }
  
  # riduzioni dello scenario rispetto al BAU
  emissionStackSCEN <- applyRedPercEmissions(emissionStackBAU, redPerc, "SCEN")
  
  if (setProgress) {
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }
  
  return(list(emissionStackBAU,emissionStackSCEN))
  
}



computeBaseYearConcentrations <- function(scenario,baseYear,redPerc,setProgress,progressStart,progressEnd) {
	
	# redPerc <- rep(20,length(PRECURSORS)*length(SECTORS))
	# red <- 100 - matrix(redPerc, nrow = length(PRECURSORS), ncol = length(SECTORS_ID)) # matrice delle riduzioni%
  
  print(paste0(Sys.time(), " - Lancio computeBaseYearConcentrations per lo scenario: ", scenario))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute concentrations ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    
  }
	
	red <- 100 + redPerc # Riduzioni con il segno negativo
	
	lambda <- (red - 100) / 50
	
	omega100 <- 1 + lambda
	omega100[red > 100] <- 1 # se ho incremento delle emissioni
	omega100[red < 50] <- 0 # se ho riduzioni emissive maggiori del 50%
	
	omega50 <- -lambda
	omega50[red > 100] <- 0
	omega50[red < 50] <- 1
	
	# Per il settore 4
	# lambda[,4] <- (red[,4] - 100) / 100
	# omega100[,4] <- 1
	# omega50[,4] <- 0

	final <- stack()
	# delta <- stack()
	# deltaP <- stack()
	
	concIndex <- match(baseYear,YEAR_STEPS)
	
	if (concIndex > 1) {
	  
	  # Per 2030 e 2050 considero solo c100
	  omega100[] <- 1
	  omega50[] <- 0
	  
	}
	
	for (n in 1:length(SPECIES)) {
		
	  print(paste0(Sys.time(), " - Computing concentrations for: ", SPECIES[n]))
		
	  # leggo il file base 100
	  var.name <- SPECIES[n]
	  base100 <- readCAMXfile(base100.year.fileNames[[concIndex]], var.name)
	  
	  # leggo il file scenario al 50%
	  scen50 <- readCAMXfile(scen50.year.fileNames[[1]], var.name)
	  
		Nsec <- length(SECTORS_ID)-1
		Npre <- length(SPECIES_PREC[[n]])
		
		s <- base100
		omega100_avg <- base100
		omega50_avg <- base100
		var100 <- base100
		var50 <- base100
		
		s[] <- 0
		omega100_avg[] <- 0
		omega50_avg[] <- 0
		var100[] <- 0
		var50[] <- 0
		
		for (i in match(SPECIES_PREC[[n]], PRECURSORS)) {
			
		  print(paste0(Sys.time(), " - Precursor: ", PRECURSORS[i]))
			
			for (j in 1:Nsec) {
				
			  print(paste0(Sys.time(), "  + Sector: ", SECTORS_ID[j]))
			  
			  var.name <- paste(SPECIES[n],SECTORS_ID[j],"ITA",PRECURSORS[i], sep = "_")
			  
			  # leggo il ddm al 50% del file base 100
			  base100.ddm50 <- readCAMXfile(base100.ddm50.year.fileNames[[concIndex]][j], var.name)
			  
			  # leggo il ddm al 50% dello scenario al 50%
		    scen50.ddm50 <- readCAMXfile(scen50.ddm50.year.fileNames[[1]][j], var.name)  

        if (concIndex == 1) { 
          s_curr <- (abs(base100.ddm50) + abs(scen50.ddm50)) / 2 
        } else {
          s_curr <- abs(base100.ddm50) 
        }
				
				omega100_curr = omega100[i,j] * s_curr
				omega50_curr = omega50[i,j] * s_curr
				var100_curr <- lambda[i,j] * base100.ddm50
				var50_curr <- (1 + lambda[i,j]) * scen50.ddm50

				s <- s_curr + s
				omega100_avg <- omega100_curr + omega100_avg
				omega50_avg <- omega50_curr + omega50_avg
				var100 <- var100_curr + var100
				var50 <- var50_curr + var50
				
				if (setProgress) {
				  
				  progressInc <- progressInc + (progressEnd - progressStart) / ((length(SECTORS_ID) - 1) * sum(lengths(SPECIES_PREC)))
					progress$set(progressInc/100, detail = paste(round(progressInc,1),"%"))	
					
				}
				
			}
			
		}
		
		omega100_avg <- omega100_avg / s
		omega50_avg <- omega50_avg / s
		
		finalConc <- omega100_avg * (base100 + var100) + omega50_avg * (scen50 + var50)
		
		# correzione per quelle celle in cui s=0 e si ottengono NA in omega100 e omega50
		finalConc[which(s == 0)] <- base100[which(s == 0)]
		
		
		# elimino valori estremi o negativi per problema DDM
		finalConc[finalConc<0] <- NA
		finalConc[finalConc>1e+6] <- NA
		
		deltaConc <- -(base100 - finalConc) # Riduzioni con il segno negativo
		
		finalConc <- SPECIES_CONV[n] * finalConc
		# deltaConc <- SPECIES_CONV[n] * deltaConc
		
		# deltaPerc <- deltaConc / base100 * 100
		# deltaPerc[deltaPerc == Inf] <- 0 
		
		finalConc <- round(finalConc, digits = 2)
		# deltaConc <- round(deltaConc, digits = 2)
		# deltaPerc <- round(deltaPerc, digits = 2)
		
		# converto la matrice in raster
		finalConc <- matrix2raster(finalConc)
		# deltaConc <- matrix2raster(deltaConc)
		# deltaPerc <- matrix2raster(deltaPerc)

		final <- addToStack(finalConc, final)
		# delta <- addToStack(deltaConc, delta)
		# deltaP <- addToStack(deltaPerc, deltaP)
		
	}
	
	# final <- addLayer(final, delta, deltaP)
	final <- round(final, digits = 2)
	names(final) <- SPECIES
	
	# names(final) <- c(SPECIES,paste0("delta_",SPECIES),paste0("deltaP_",SPECIES))
	return(final)
	
}


computeBaseYearConcentrationsDaily <- function(scenario,baseYear,redPerc,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeBaseYearConcentrationsDaily per lo scenario: ", scenario))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute concentrations ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  }
  
  red <- 100 + redPerc # Riduzioni con il segno negativo
  
  lambda <- (red - 100) / 50
  
  omega100 <- 1 + lambda
  omega100[red > 100] <- 1 # se ho incremento delle emissioni
  omega100[red < 50] <- 0 # se ho riduzioni emissive maggiori del 50%
  
  omega50 <- -lambda
  omega50[red > 100] <- 0
  omega50[red < 50] <- 1
  
  # Per il settore 4
  # lambda[,4] <- (red[,4] - 100) / 100
  # omega100[,4] <- 1
  # omega50[,4] <- 0
  
  final <- stack()
  # delta <- stack()
  # deltaP <- stack()
  
  concIndex <- match(baseYear,YEAR_STEPS)
  
  if (concIndex > 1) {
    
    # Per 2030 e 2050 considero solo c100
    omega100[] <- 1
    omega50[] <- 0
    
  }
  
  for (n in 1:length(SPECIES)) {
    
    print(paste0(Sys.time(), " - Computing concentrations for: ", SPECIES[n]))
    
    # leggo il file base 100
    var.name <- SPECIES[n]
    base100 <- readCAMXfileDaily(base100.dly.fileNames[[concIndex]], var.name)
    
    # leggo il file scenario al 50%
    scen50 <- readCAMXfileDaily(scen50.dly.fileNames[[1]], var.name)
    
    Nsec <- length(SECTORS_ID)-1
    Npre <- length(SPECIES_PREC[[n]])
    
    s <- base100
    omega100_avg <- base100
    omega50_avg <- base100
    var100 <- base100
    var50 <- base100
    
    s[] <- 0
    omega100_avg[] <- 0
    omega50_avg[] <- 0
    var100[] <- 0
    var50[] <- 0
    
    for (i in match(SPECIES_PREC[[n]], PRECURSORS)) {
      
      print(paste0(Sys.time(), " - Precursor: ", PRECURSORS[i]))
      
      for (j in 1:Nsec) {
        
        print(paste0(Sys.time(), "  + Sector: ", SECTORS_ID[j]))
        
        var.name <- paste(SPECIES[n],SECTORS_ID[j],"ITA",PRECURSORS[i], sep = "_")
        
        # leggo il ddm al 50% del file base 100
        base100.ddm50 <- readCAMXfileDaily(base100.ddm50.dly.fileNames[[concIndex]][j], var.name)
        
        # leggo il ddm al 50% dello scenario al 50%
        scen50.ddm50 <- readCAMXfileDaily(scen50.ddm50.dly.fileNames[[1]][j], var.name)  

        if ((i == 1) & (j == 1)) {
        	
        	# controllo che nelle celle per l'Italia il ddm non sia maggiore delle concetrazioni di base
        	# altrimenti elimino il giorno
        	dayOff100 <- which(apply(base100.ddm50-base100, 3, function(x) any(italyMask(x[,])>0)) == TRUE)
        	if (length(dayOff100)>0) base100.ddm50[,,dayOff100] <- NA
        	 
        	dayOff50 <- which(apply(scen50.ddm50-scen50, 3, function(x) any(italyMask(x[,])>0)) == TRUE)
        	if (length(dayOff50)>0) scen50.ddm50[,,dayOff50] <- NA
        	
        } else {
        	
        	if (length(dayOff100)>0) base100.ddm50[,,dayOff100] <- NA
        	if (length(dayOff50)>0) scen50.ddm50[,,dayOff50] <- NA
        	
        }
        
        if (concIndex == 1) { 
          s_curr <- (abs(base100.ddm50) + abs(scen50.ddm50)) / 2 
        } else {
          s_curr <- abs(base100.ddm50) 
        }
        
        omega100_curr = omega100[i,j] * s_curr
        omega50_curr = omega50[i,j] * s_curr
        var100_curr <- lambda[i,j] * base100.ddm50
        var50_curr <- (1 + lambda[i,j]) * scen50.ddm50
        
        s <- s_curr + s
        omega100_avg <- omega100_curr + omega100_avg
        omega50_avg <- omega50_curr + omega50_avg
        var100 <- var100_curr + var100
        var50 <- var50_curr + var50
        
        if (setProgress) {
          
          progressInc <- progressInc + (progressEnd - progressStart) / ((length(SECTORS_ID) - 1) * sum(lengths(SPECIES_PREC)))
          write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
          
        }
        
      }
      
    }
    
    omega100_avg <- omega100_avg / s
    omega50_avg <- omega50_avg / s

    finalConc <- omega100_avg * (base100 + var100) + omega50_avg * (scen50 + var50)
    
    # correzione per quelle celle in cui s=0 e si ottengono NA in omega100 e omega50
    finalConc[which(s == 0)] <- base100[which(s == 0)]
    
    
    # elimino valori estremi o negativi per problema DDM
    finalConc[finalConc<0] <- NA
    finalConc[finalConc>1e+6] <- NA
    
    # deltaConc <- -(base100 - finalConc) # Riduzioni con il segno negativo
    
    finalConc <- SPECIES_CONV[n] * finalConc
    # deltaConc <- SPECIES_CONV[n] * deltaConc
    
    # deltaPerc <- deltaConc / base100 * 100
    # deltaPerc[deltaPerc == Inf] <- 0 
    
    finalConc <- round(finalConc, digits = 2)
    # deltaConc <- round(deltaConc, digits = 2)
    # deltaPerc <- round(deltaPerc, digits = 2)

    # save(finalConc, file = paste0(outputDir,"/",SPECIES[n],"_daily_finalConc.Rdata"))
    # save(deltaConc, file = paste0(outputDir,"/",SPECIES[n],"_daily_deltaConc.Rdata"))
    # save(deltaPerc, file = paste0(outputDir,"/",SPECIES[n],"_daily_deltaPerc.Rdata"))
    
    # if ("yearly avg" %in% SPECIES_STATS[[n]]) {
    #   
    #   # calcolo la media annua
    #   finalConcYearlyAvg <- rowMeans(finalConc, dims = 2, na.rm = T)
    #   deltaConcYearlyAvg <- rowMeans(deltaConc, dims = 2, na.rm = T)
    #   deltaPercYearlyAvg <- rowMeans(deltaPerc, dims = 2, na.rm = T)
    #   
    #   # converto la matrice in raster
    #   finalConcYearlyAvg <- matrix2raster(finalConcYearlyAvg)
    #   deltaConcYearlyAvg <- matrix2raster(deltaConcYearlyAvg)
    #   deltaPercYearlyAvg <- matrix2raster(deltaPercYearlyAvg)
    #   
    #   final <- addToStack(finalConcYearlyAvg, final)
    #   names(final)[length(names(final))] <- SPECIES[n]
    #   
    #   delta <- addToStack(deltaConcYearlyAvg, delta)
    #   names(delta)[length(names(delta))] <- paste0("delta_",SPECIES[n])
    #   
    #   deltaP <- addToStack(deltaPercYearlyAvg, deltaP)
    #   names(deltaP)[length(names(deltaP))] <- paste0("deltaP_",SPECIES[n])
    #   
    # }
    
    # if ("# daily exceed" %in% SPECIES_STATS[[n]]) {
    #   
    #   # calcolo il numero di superamenti giornalieri
    #   limit <- SPECIES_DLY_LIMITS[n]
    #   finalConcDailyExceed <- apply(finalConc, 1:2, function(x) sum(x > limit, na.rm = TRUE))
    #   
    #   # converto la matrice in raster
    #   finalConcDailyExceed <- matrix2raster(finalConcDailyExceed)
    # 
    #   final <- addToStack(finalConcDailyExceed, final)
    #   names(final)[length(names(final))] <- paste0(SPECIES[n],"_dylExceed")
    #   
    # }
    
    finalConc <- round(finalConc, digits = 2)
    finalConc <- matrix2raster(finalConc)
    names(finalConc) <- paste0(SPECIES[n],"_day",1:nlayers(finalConc))
    
    final <- addToStack(finalConc, final)

  }
  
  # final <- addLayer(final, delta, deltaP)
  # final <- round(finalConc, digits = 2)
  
  
  # names(final) <- c(SPECIES,paste0("delta_",SPECIES),paste0("deltaP_",SPECIES))
  return(final)
  
}

raster2matrix <- function(raster) {
	
	r <- t(as.matrix(flip(raster)))
	
	return(r)
	
}

matrix2raster <- function(matrix) {
	
	xMin <- -580000
	xMax <- 556000
	yMin <- -736000
	yMax <- 712000
	
	crs.lcc <- CRS("+proj=lcc +lat_0=41.9 +lon_0=12.6 +lat_1=30 +lat_2=60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
	
	if (length(dim(matrix)) == 2) {
	  
	  r <- flip(raster(t(matrix)))
	
  	extent(r) <- extent(xMin, xMax, yMin, yMax)
  	projection(r) <- crs.lcc
	
	  return(r)
  
	} else if (length(dim(matrix)) == 3) {
	  
	  # caso giornaliero
	  layers <- dim(matrix)[3]
	  stack_list <- vector("list", layers)
	  
	  for (i in 1:layers) {
	    
	    r <- flip(raster(t(matrix[,,i])))
	    
	    extent(r) <- extent(xMin, xMax, yMin, yMax)
	    projection(r) <- crs.lcc
	    
	    stack_list[[i]] <- r
	    
	  }
	  
	  return(stack(stack_list))
	  
	}
	
}

italyMask <- function(matrix) {
	
	raster <- matrix2raster(matrix)
	values(raster) <- 1
	
	shpIta <- st_transform(shp[[3]], crs(raster))
	raster <- mask(raster, shpIta)
	
	r <- raster2matrix(raster) * matrix
	
	return(r)
	
}

createEmiPlotData <- function(rasterStack, ScenarioName) {
	
	shpIta <- st_transform(shp[[3]], crs(rasterStack))
	rasterStack <- mask(rasterStack, shpIta)
	
	# nLayers <- length(PRECURSORS) * length(SECTORS)
	emiNames <- paste(PRECURSORS, rep(SECTORS_ID, each = length(PRECURSORS)), sep = "_")
	rs <- rasterStack[[emiNames]]
	
	x <- data.frame(raster::values(rs)) %>%
		summarise_all(sum, na.rm = TRUE) %>% 
		pivot_longer(everything(), names_to = "varName", values_to = "value")
	
	x$varName <- sapply(strsplit(x$varName, "_"), "[", 1)
	
	x <- x %>% 
		group_by(varName)%>%
		summarise_all(sum, na.rm = TRUE)
	
	x <- cbind(category = ScenarioName, x)
	x$value <- round(x$value, digits = 1)
	
	dat1 <- x %>% 
		group_by(category) %>%  
		do(data = list_parse2(data.frame(.$varName, .$value))) %>%
		ungroup() %>%
		rename(name = category)
	
	dat1$absDel <- "absolute"
	
	# rs <- rasterStack[[(nLayers+1):(nLayers*2)]]
	rs <- rasterStack[[paste0("delta_",emiNames)]]
	
	x <- data.frame(raster::values(rs)) %>%
		summarise_all(sum, na.rm = TRUE) %>% 
		pivot_longer(everything(), names_to = "varName", values_to = "value")
	
	x$varName <-  sapply(strsplit(x$varName, "_"), "[", 2)
	
	x <- x %>% 
		group_by(varName)%>%
		summarise_all(sum, na.rm = TRUE)
	
	x <- cbind(category = ScenarioName, x)
	x$value <- round(x$value, digits = 1)
	
	dat2 <- x %>% 
		group_by(category) %>%  
		do(data = list_parse2(data.frame(.$varName, .$value))) %>%
		ungroup() %>%
		rename(name = category)
	
	dat2$absDel <- "delta"

	dat <- rbind(dat1,dat2)
	dat$type <- "column"
	# dat$value <- round(dat$value, digits = 1)
	
	return(dat)
	
}

createHIAPlotData <- function(rasterStack, ScenarioName) {
	
	shpIta <- st_transform(shp[[3]], crs(rasterStack))
	rasterStack <- mask(rasterStack, shpIta)
	
	x <- data.frame(raster::values(rasterStack)) %>%
		summarise_all(sum, na.rm = TRUE) %>% 
		pivot_longer(everything(), names_to = "varName", values_to = "value")
	
	x <- cbind(category = ScenarioName, x)
	x$varName <- c(HIA_VARS,"Health Costs")
	x$value[x$varName == "Health Costs"] <- (x$value[x$varName == "Avoided YOLL"] * VOLY) / 10^6
	x$value <- round(x$value, digits = 0)
	
	dat <- x %>% 
		group_by(category, varName) %>%  
		do(data = list_parse2(data.frame(.$varName, .$value))) %>%
		ungroup() %>%
		rename(name = category)
	
	return(dat)
	
}


createBoxPlotData <- function(rasterStack, ScenarioName) {
	
	shpIta <- st_transform(shp[[3]], crs(rasterStack))
	rasterStack <- mask(rasterStack, shpIta)
	
	x <- data.frame(raster::values(rasterStack)) %>%
		pivot_longer(all_of(SPECIES) ,names_to = "varName", values_to = "value") %>% 
		dplyr::select(c("varName","value")) 
	x <- x[!is.na(x$value),]
	
	dat1 <- data_to_boxplot(x, value, varName, name = ScenarioName)
	dat1$absDel <- "absolute"
	
	x <- data.frame(raster::values(rasterStack)) %>%
		pivot_longer(all_of(paste0("delta_",SPECIES)) ,names_to = "varName", values_to = "value") %>% 
		dplyr::select(c("varName","value"))
	x <- x[!is.na(x$value),]
	x$varName <- str_replace(x$varName,"delta_","")
	
	dat2 <- data_to_boxplot(x, value, varName, name = ScenarioName)
	dat2$absDel <- "delta"
	
	dat <- rbind(dat1,dat2)
		
	return(dat)
	
}


interpolateRaster <- function(scenario, rasterStack, fun, type, setProgress, progressStart, progressEnd) {
	
	export <- list()
	
	if (setProgress) {
	  
	  directory <- getScenarioDirectoryInDB(scenario)
	  progressInc <- progressStart
	  progressFile <- file.path(SCENDIR,directory,"progress.txt")
	  
	  statusFile <- file.path(SCENDIR,directory,"status.txt")
	  write.table("Spatial interpolation ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
	  
	  nSteps <- 3
	  
	}
	

	if (type == "EMIX") {
		
		varNames <- paste(PRECURSORS, rep(SECTORS_ID, each = length(PRECURSORS)), sep = "_")
		deltaVarNames <- paste0("delta_",varNames)
		deltaPVarNames <- paste0("deltaP_",varNames)
		computePerc <- TRUE
		dailyExceedVarNames <- NA
		
	} else if (type == "CONC") {
		
		varNames <- SPECIES
		deltaVarNames <- paste0("delta_",SPECIES)
		deltaPVarNames <- paste0("deltaP_",SPECIES)
		
		dailyExceedVarNames <- c(paste0(SPECIES,"_dylExceed"),paste0("bau_",SPECIES,"_dylExceed"))
		# tengo solo quelli effettivamente presenti nel rasterStack
		dailyExceedVarNames <- dailyExceedVarNames[dailyExceedVarNames %in% names(rasterStack)]
		
		computePerc <- TRUE
		
	} else {
		
	  dailyExceedVarNames <- NA
		computePerc <- FALSE
		
	}
	
	export[[1]] <- exact_extract(rasterStack, shp[[1]], fun, progress = FALSE, append_cols = c('COMUNE','POP_2011'))  %>%
		rename('NAME' = 'COMUNE') %>% 
		rename_with(~ gsub(paste0(fun,"."), "", .x, fixed = TRUE)) %>% 
		mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
	
	
	if (setProgress) {
	  
	  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
	  write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
	  
	}
		
	export[[2]] <- exact_extract(rasterStack, shp[[2]], fun, progress = FALSE, append_cols = c('PROVINCIA','POP_2011')) %>%
		rename('NAME' = 'PROVINCIA') %>% 
		rename_with(~ gsub(paste0(fun,"."), "", .x, fixed = TRUE)) %>% 
		mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
	

	if (setProgress) {
	  
	  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
	  write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
	  
	}
	
	export[[3]] <- exact_extract(rasterStack, shp[[3]], fun, progress = FALSE, append_cols = c('REGIONE','POP_2011')) %>%
		rename('NAME' = 'REGIONE') %>% 
		rename_with(~ gsub(paste0(fun,"."), "", .x, fixed = TRUE)) %>% 
		mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
	
	if (computePerc) {
		
		export[[1]][,deltaPVarNames] <- export[[1]][,deltaVarNames] / (export[[1]][,varNames] - export[[1]][,deltaVarNames]) * 100
		export[[2]][,deltaPVarNames] <- export[[2]][,deltaVarNames] / (export[[2]][,varNames] - export[[2]][,deltaVarNames]) * 100
		export[[3]][,deltaPVarNames] <- export[[3]][,deltaVarNames] / (export[[3]][,varNames] - export[[3]][,deltaVarNames]) * 100
		
	} 
	
	if (type == "CONC" & length(dailyExceedVarNames) > 0) {
	  # calcolo il massimo dei superamenti 
	  export[[1]][,dailyExceedVarNames] <- exact_extract(rasterStack[[dailyExceedVarNames]], shp[[1]], "max", progress = FALSE)
	  export[[2]][,dailyExceedVarNames] <- exact_extract(rasterStack[[dailyExceedVarNames]], shp[[2]], "max", progress = FALSE)
	  export[[3]][,dailyExceedVarNames] <- exact_extract(rasterStack[[dailyExceedVarNames]], shp[[3]], "max", progress = FALSE)
	}
	
	
	if (setProgress) {
	  
	  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
	  write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
	  
	}

	return(export)
	
}

mia_pm25lt <- function(pop,inc,age,pmc,pmb,zcf) {
	plus <- function(x) {
		(x>0)*x
	}
	beta_m <- log(1.062)/10
	beta_u <- log(1.083)/10
	beta_l <- log(1.040)/10
	hi_m <- 0*pop
	hi_u <- 0*pop
	hi_l <- 0*pop
	if (age>=30) {
		hi_m <- pop*inc*(1-exp(beta_m*plus(pmc-zcf)-beta_m*plus(pmb-zcf)))
		hi_u <- pop*inc*(1-exp(beta_u*plus(pmc-zcf)-beta_u*plus(pmb-zcf)))
		hi_l <- pop*inc*(1-exp(beta_l*plus(pmc-zcf)-beta_l*plus(pmb-zcf)))
	}
	result <- list(hi_m,hi_u,hi_l)
	return(result)
}

mia_no2lt <- function(pop,inc,age,pmc,pmb,zcf) {
	plus <- function(x) {
		(x>0)*x
	}
	beta_m <- log(1.0076)/10
	beta_u <- log(1.013)/10
	beta_l <- log(1.006)/10
	# hi_m <- 0*pop
	# hi_u <- 0*pop
	# hi_l <- 0*pop
	
	hi_m <- pop*inc*(1-exp(beta_m*plus(pmc-zcf)-beta_m*plus(pmb-zcf)))
	hi_u <- pop*inc*(1-exp(beta_u*plus(pmc-zcf)-beta_u*plus(pmb-zcf)))
	hi_l <- pop*inc*(1-exp(beta_l*plus(pmc-zcf)-beta_l*plus(pmb-zcf)))
	
	result <- list(hi_m,hi_u,hi_l)
	return(result)
}

computeHIA <- function(scenario,rasterStack,setProgress,progressStart,progressEnd) {
	
  print(paste0(Sys.time(), " - Lancio computeHIA per lo scenario: ", scenario))
  
  if (setProgress) {
    
    directory <- getScenarioDirectoryInDB(scenario)
    progressInc <- progressStart
    progressFile <- file.path(SCENDIR,directory,"progress.txt")
    
    statusFile <- file.path(SCENDIR,directory,"status.txt")
    write.table("Compute HIA ...",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)

  }
 
	PM_avrg_c <- rasterStack[[which(SPECIES=="PM25")]]
	PM_avrg_c_grd <- raster::values(PM_avrg_c) 
	
	NO2_avrg_c <- rasterStack[[which(SPECIES=="NO2")]] 
	NO2_avrg_c_grd <- raster::values(NO2_avrg_c) 

	mi_pm <- list(0,0,0)
	yll_pm <- list(0,0,0)
	
	mi_no2 <- list(0,0,0)
	yll_no2 <- list(0,0,0)
	
	ptot <- 0
	base_mort <- 0 
	cells_idx <- data.frame(inc_interp)[,1]
	ptot <- 0

	#progressInc <- progressStart
	
	for (i in 1:length(eta_l_p)) {
		#AGE-SPECIFIC DATA PREP
		age <- eta_l_p[i]
		p <- data.frame(pop_interp)[,i+1]
		inc <- data.frame(inc_interp)[,1+findInterval(age,eta_l_i)]
		le <- data.frame(l_interp)[,1+findInterval(age,eta_l_l)]
		print(paste("AGE: ",age," POP_AGE",colnames(pop_interp)[i+1]," INC_AGE: ",
								colnames(inc_interp)[1+findInterval(age,eta_l_i)]," LE_AGE: ",
								colnames(l_interp)[1+findInterval(age,eta_l_l)]," HA_AGE: ",
								colnames(h_interp[[1]])[1+findInterval(age,eta_l_h)],sep=""))
		base_mort <- base_mort + inc*p
		ptot <- p + ptot
		
		#CALCULATIONS (MORTALITY)
		tpm <- mia_pm25lt(p,inc,age,PM_avrg_c_grd[cells_idx],PM_avrg_b_grd[cells_idx],as.numeric(zcf_pm))
		mi_pm <- lapply(c(1,2,3),function(v) tpm[[v]]+mi_pm[[v]])
		yll_pm <- lapply(c(1,2,3),function(v) yll_pm[[v]]+tpm[[v]]*le)
		
		#CALCULATIONS (MORTALITY)
		tno2 <- mia_no2lt(p,inc,age,NO2_avrg_c_grd[cells_idx],NO2_avrg_b_grd[cells_idx],as.numeric(zcf_no2))
		mi_no2 <- lapply(c(1,2,3),function(v) tno2[[v]]+mi_no2[[v]])
		yll_no2 <- lapply(c(1,2,3),function(v) yll_no2[[v]]+tno2[[v]]*le)
		
		if (setProgress) {
		  
		  progressInc <- progressInc + (progressEnd - progressStart) / length(eta_l_p)
		  write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
		  
		}
			
	}
	
	#totals
	mort <- base_mort - mi_pm[[1]] - mi_no2[[1]]

	# mi_grd2 <- st_sf(cbind(ptot,base_mort/ptot*10^5,mi_pm[[1]],yll_pm[[1]],st_centroid(grd[cells_idx,])))
	mi_grd2 <- st_sf(cbind(mort,mi_pm[[1]]+mi_no2[[1]],yll_pm[[1]]+yll_no2[[1]],ptot,st_centroid(grd[cells_idx,])))
	t <- st_transform(mi_grd2, crs = crs(PM_avrg_c)) 
	t[is.na(t)] <- 0
	
	finalList <- lapply(1:4, function(v) rasterize(t, PM_avrg_c, field = names(t)[v]))
	final <- stack(finalList)
	final <- round(final, digits = 0)
	
	return(final)
	
}

