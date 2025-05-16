library(jsonlite)

SCENARIO_DEFINITION_CHOICES <- c("Upload a file with activity indicators",
																 "Upload a file with emission reductions",
																 "Set manually emission reductions by interface")

COMPUTATION_CHOICES <- c("Yearly average",
                         "Daily average")

SPATIAL_AGGREGATION_CHOICES <- c("Regions","Provinces","Municipalities","Gridded")
SPATIAL_AGGREGATION_SHP <- c("Municipalities","Provinces","Regions")

DELTA_ABS_CHOICHES <- c("Absolute (SCEN)","Absolute (BAU)", "Delta (SCEN vs BAU)", "Delta % (SCEN vs BAU)")
EMI_DENS_CHOICHES <- c("Total [ton]", "Density [ton/km\u00b2]")
HIA_DENS_CHOICHES <- c("Total [inhabitants]", "Density [per 100.000 inh]")

# EMISSIONS
PRECURSORS <- c("NOX","VOC","SO2","NH3","PP25","PPCO")
PRECURSORS_DESC <- c("NO\u2093","VOC","SO\u2082","NH\u2083","PM2.5","PM coarse")
PRECURSORS_UNIT <- c("kmol","kmol","kmol","kmol","ton","ton")
PRECURSORS_SPEC <- list(c("NO","NO2"),
						c("NASN","PAR","OLE","TOL","XYL","FORM","ALD2","ETH","ISOP","MEOH","ETOH","ETHA","CH4","IOLE","ALDX","TERP","NR"),
						c("SO2"),
						c("NH3"),
						c("PSO4","PNO3","PNH4","NA","PCL","PEC","POA","FCRS","FPRM"),
						c("CCRS","CPRM"))
PRECURSORS_MOLWT <- list(c(30,46.0),
						 c(128,16,32,112,128,16,32,32,80,16,32,32,16,64,32,160,16),
						 64.0,
						 7.0,
						 c(1,1,1,1,1,1,1,1,1),
						 c(1,1))

# CONCENTRATIONS
SPECIES <- c("NO2","SO2","NH3","PEC","PM25","PM10")
SPECIES_PREC <- list(c("NOX","VOC","SO2","NH3"),
					 c("NOX","VOC","SO2","NH3"),
					 c("NOX","VOC","SO2","NH3"),
					 c("PP25"),
					 c("NOX","VOC","SO2","NH3","PP25"),
					 c("NOX","VOC","SO2","NH3","PP25","PPCO"))

SPECIES_DESC <- c("NO\u2082","SO\u2082","NH\u2083","PEC","PM2.5","PM10")
SPECIES_UNIT <- c("[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]")

SPECIES_STATS <- list(c("yearly avg"),
                      c("yearly avg"),
                      c("yearly avg"),
                      c("yearly avg"),
                      c("yearly avg"),
                      c("yearly avg","# daily exeed"))

SPECIES_DLY_LIMITS <- c(NA,125,NA,NA,NA,50)

#conversion from ppbV to ug/m3 with 25°C and 1 atm
SPECIES_CONV <- c(1.91, 2.62, 0.7, 1, 1, 1)

# SECTORS
SECTORS <- c("MS02Bio","MS02noBio","MS07","MS01","MS03.04","MS10","Others")
SECTORS_ID <- c("RHB","RHN","TRA","ENE","IND","AGR","OTH")
SECTORS_DESC <- c("MS02 - Residential combustion with biomass",
                  "MS02 - Residential combustion (Other fuels)",
                  "MS07 - Mobile Sources",
                  "MS01 - Energy Production",
                  "MS03.04 - Industrial sources",
                  "MS10 - Agriculture",
                  "Others")


# HEALTH
HIA_VARS <- c("Mortality","Avoided deaths","Avoided YOLL")
VOLY <- 70000 # Value Of a Life Year lost [euro]

if (Sys.info()['sysname']=="Linux") {
	DATAPATH <- "/data"
} else {
	DATAPATH <- "../data"
}

SCENDIR <- file.path(DATAPATH,"SCENARIO")
BAUSCENDIR <- file.path(DATAPATH,"BAU_SCENARIO")
CAMXDIR <- file.path(DATAPATH,"CAMX")
EMIXDIR <- file.path(DATAPATH,"EMIX")
HIADIR <- file.path(DATAPATH,"CODE_HIA")
TEMPLATESDIR <- file.path(DATAPATH,"TEMPLATES")
USERGUIDEDIR <- file.path(DATAPATH,"USERGUIDE")


YEAR_STEPS <- c(2017,2030,2050)

## Daily avg CAMX files

base100.dly.fileNames <- list(
  ## 2017
  file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.avrg.year.grd01.dayavg.nc"),
  ## 2030
  file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.avrg.year.grd01.dayavg.nc"),
  ## 2050
  ##################################### DA INSERIRE 2050
  file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[3],NA)
)
  

base100.ddm50.dly.fileNames <- list(
  ## 2017
  c(file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.RHB.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.RHN.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.TRA.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.ENE.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.IND.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.AGR.nc")
    ),
  ## 2030
  c(
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.RHB.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.RHN.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.TRA.nc"),  
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.ENE.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.IND.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.AGR.nc")
  ),
  ## 2050
  c(
  ##################################### DA INSERIRE 2050 
  )
)

scen50.dly.fileNames <- list(
  ## 2017
  file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.avrg.year.grd01.dayavg.nc")
)

scen50.ddm50.dly.fileNames <- list(
  ## 2017
  c(file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.RHB.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.RHN.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.TRA.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.grd01.dayavg.ENE.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.IND.nc"),
    file.path(CAMXDIR,"DAYAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.grd01.dayavg.AGR.nc")
    )
)


## Yearly avg CAMX files

base100.year.fileNames <- list(
  ## 2017
  file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.avrg.year.avg.grd01.nc"),
  ## 2030
  file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.avrg.year.avg.grd01.nc"),
  ## 2050
  file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_37_2050_base_ddm_01e02b07t_p50.avrg.year.avg.grd01.nc")
)
  

base100.ddm50.year.fileNames <- list(
  ## 2017
  c(
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.RHB.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.RHN.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.TRA.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_31_2017_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.ENE.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.IND.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_32_2017_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.AGR.nc")
    ),
  ## 2030
  c(
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.RHB.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.RHN.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.TRA.nc"),  
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_35_2030_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.ENE.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.IND.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[2],"modea_36_2030_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.AGR.nc")
  ),
  ## 2050
  c(
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_37_2050_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.RHB.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_38_2050_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.RHN.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_37_2050_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.TRA.nc"),  
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_37_2050_base_ddm_01e02b07t_p50.ddm.year.avg.grd01.ENE.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_38_2050_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.IND.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[3],"modea_38_2050_base_ddm_02n0304i10a_p50.ddm.year.avg.grd01.AGR.nc")
  )
)

scen50.year.fileNames <- list(
  ## 2017
  file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.avrg.year.avg.grd01.nc")
)

scen50.ddm50.year.fileNames <- list(
  ## 2017
  c(
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.avg.grd01.RHB.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.avg.grd01.RHN.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.avg.grd01.TRA.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_33_2017_sce50_01e02b07t_ddm_01e02b07t_p50.ddm.year.avg.grd01.ENE.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.avg.grd01.IND.nc"),
    file.path(CAMXDIR,"YEARAVG",YEAR_STEPS[1],"modea_34_2017_sce50_02n0304i10a_ddm_02n0304i10a_p50.ddm.year.avg.grd01.AGR.nc")
  )
)






emibase.ar.tot.filename <- paste0(EMIXDIR,"/2017.ar.ita.4km.ttl.nc")

# Emission files. One file per sector
emibase.ar.filenames <- list(
  ## 2017
  c(
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.RHB.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.RHN.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.TRA.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.IND.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.AGR.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"areal.2017.1.4km.ita.OTH.ttl.nc")
  ),
  ## 2030
  c(
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.RHB.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.RHN.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.TRA.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.IND.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.AGR.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"areal.2030.1.4km.ita.OTH.ttl.nc")
  ),
  ## 2050
  c(
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.RHB.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.RHN.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.TRA.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.IND.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.AGR.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"areal.2050.1.4km.ita.OTH.ttl.nc")
  )
)

emibase.pt.filenames <- list(
  ## 2017
  c(
    NA,
    NA,
    NA,
    file.path(EMIXDIR,YEAR_STEPS[1],"point.2017.1.ita.4km.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[1],"point.2017.1.ita.4km.IND.ttl.nc"),
    NA,
    file.path(EMIXDIR,YEAR_STEPS[1],"point.2017.1.ita.4km.OTH.ttl.nc")
  ),
  ## 2030
  c(
    NA,
    NA,
    NA,
    file.path(EMIXDIR,YEAR_STEPS[2],"point.2030.1.ita.4km.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[2],"point.2030.1.ita.4km.IND.ttl.nc"),
    NA,
    file.path(EMIXDIR,YEAR_STEPS[2],"point.2030.1.ita.4km.OTH.ttl.nc")
  ),
  ## 2050
  c(
    NA,
    NA,
    NA,
    file.path(EMIXDIR,YEAR_STEPS[3],"point.2050.1.ita.4km.ELE.ttl.nc"),
    file.path(EMIXDIR,YEAR_STEPS[3],"point.2050.1.ita.4km.IND.ttl.nc"),
    NA,
    file.path(EMIXDIR,YEAR_STEPS[3],"point.2050.1.ita.4km.OTH.ttl.nc")
  )
)


emissFactors.filename <- file.path(EMIXDIR,"emissionFactors.csv")

load(paste0(HIADIR,"/PREPROC-DATA/prepdata.RData"))



