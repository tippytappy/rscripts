source("C:/Users/dylan/Documents/R/scripts/useful_functions.R")
#### MAINS REPAIRS ####
importMR <- function() {
  # imports and tidies the SAP mains repairs
  if(!require(readxl)) {require(readxl)}
  if(!require(dplyr)) {require(dplyr)}
  
  cat("importing the mains repairs\n")
  mr <- read_excel("C:/Users/dylan/Documents/work/data/Mains Repair Reporting Latest.xlsx", 
                   sheet = "SAP Base Data")
  mrNames <- c("notification", "order", "operation", "raisedCode", "raisedType", 
               "actualCode", "actualType", "latestCode", "latestType", 
               "dateCreated", "dateCompleted", "funcloc", "dma", 
               "buildnum", "street", "city", "postcode", 
               "gisid", "x", "y", "maintActType", "matFirstType", 
               "workCentre", "vendor", "formCreator", "length", "workCentreArea", 
               "material", "sizeMixed", "sizeUnits", "leakageType", "cutOutLength", 
               "title", "monthYear", "mainsRepair", "postcodeBreak", "shortPostcode", 
               "region", "completedWE", "completedWeek", "raisedWE", "raisedWeek", 
               "reportingPeriod", "sizeLetter", "calculatedRegion", "sizeInches", "sizeBand")
  cat("tidying the data\n")
  names(mr) <- mrNames
  mr$dateCompleted <- as.POSIXct(mr$dateCompleted)
  mr$dateCreated <- as.POSIXct(mr$dateCreated)
  mr$x <- as.numeric(mr$x)
  mr$y <- as.numeric(mr$y)
  mr$postcode <- substr(mr$postcode, 3, 12)
  mr$buildnum[mr$buildnum == "0"] <- ""
  mr$buildnum <- gsub("Not assigned", "", mr$buildnum, ignore.case = TRUE)
  mr$address <- paste0(mr$buildnum, " ", mr$street, ",", mr$city, ", ", mr$postcode)
  mr$address <- trimws(mr$address)
  mr$mainsRepair <- factor(mr$mainsRepair)
  mr$sizeBand <- factor(mr$sizeBand, levels = c("<6", "6-9", "9-12", "12-18", "18+"), ordered = TRUE)
  
  mrSub <<- mr %>% select(order, operation, x, y, dateCreated, dateCompleted, address, sizeInches, material)
  mr <<- mr
  cat("done\n")
}
#### PERMIT TO WORK FRONT PAGES ####
importPTW <- function() {
  library(readr, quietly = TRUE)
  cat("importing the PTW data\n")  
  ptwp1ColSpec <-cols(
    ptw = col_double(),
    jobref = col_character(),
    assetNum = col_character(),
    assettype = col_character(),
    jobType = col_character(),
    startDate = col_character(),
    endDate = col_character(),
    houseNum = col_character(),
    street = col_character(),
    town = col_character(),
    postcode = col_character(),
    x = col_double(),
    y = col_double(),
    workstreamSubject = col_character(),
    workstreamActivity = col_character(),
    description = col_character(),
    jobID = col_character(),
    caseID = col_character(),
    pipeSize = col_double()
  )
  
  suppressWarnings(ptwp1 <- read_delim("F:/2/ptwp1.txt", "\t", 
                                       escape_double = FALSE, trim_ws = TRUE, col_types = ptwp1ColSpec))
  cat("done")
  ptwp1 <<- ptwp1
}
#### PERMIT TO WORK LOGS ####
importPTWLog <- function() {
  if(!require(readr)) {require(readr)}
  source("C:/Users/dylan/Documents/R/scripts/useful_functions.R")
  ptwLog <- readr::read_delim("F:/2/logAll.txt", delim = "\t", progress = TRUE, col_types = cols(
    col_character(), col_datetime(format = "%d/%m/%Y %H:%M:%S"), col_character(), col_character(), col_character(), col_character(), 
    col_datetime(format = "%d/%m/%Y %H:%M:%S"), col_character()))
  # ptwLog$comment <- cleanFreeText(ptwLog$comment)
  ptwLog <<- ptwLog
}
#### SUPPLY INTERRUPIONS ####
importSI <- function() {
  #### SET UP ####
  library(dplyr)
  library(stringr)
  #### IMPORT THE DATA ####
  cat("Importing the data\n")
  siFilePath <- file.path("C:", "Users", "dylan", "Documents", "work", "data", "si.xlsx")
  colSpec <- readxl::read_xlsx(path = siFilePath, sheet = "column_spec")
  si <- readxl::read_xlsx(path = siFilePath, sheet = "si_data", progress = TRUE)
  cat("the data has been imported\n")
  
  #### TIDY POSTCODE ####
  # full postcode pattern = "^[A-Z]{1,2}[0-9]{1,2}\\s{1,2}[0-9]{1,2}[A-Z]{1,2}$"
  # partial postcode pattern = "^[A-Z]{1,2}[0-9]{1,2}\\s{0,2}$"
  si$PostCode <- toupper(si$PostCode)
  si$postcodeTidy <- str_extract(si$PostCode, "^[A-Z]{1,2}[0-9]{1,2}\\s{1,2}[0-9]{1,2}[A-Z]{1,2}$|^[A-Z]{1,2}[0-9]{1,2}\\s{0,2}$")
  cat("the postcode field has been tidied\n")
  #### TIDY CASE ID ####
  # extract the 6 numbers, only from this field
  si$CaseIDTidy <- str_extract(si$CaseID, "\\d{6}")
  cat("the case id field has been tidied\n")
  #### TIDY PTW ####
  si$ptwTidy <- str_extract(si$PTW, "\\d{6}")
  cat("the ptw field has been tidied\n")
  #### PIPE SIZE ####
  si$SizeMainUser <- as.double(si$SizeMainUser)
  si$sizeInches <- ifelse(si$SizeMainUser > 78, ceiling(si$SizeMainUser / 25.4), si$SizeMainUser)
  si$sizeInches[si$sizeInches > 200] <- 0
  
  siSub <<- si %>% select(id, ptwTidy, CaseIDTidy, Location, postcodeTidy, sizeInches)
  si <<- si
}

#### STREET WORKS ####
importSW <- function() {
  
  if(!require(readxl)) {require(readxl)}
  if(!require(stringr)) {require(stringr)}
  
  cat("importing the streetworks data\n")
  sw <- read_excel("~/work/data/streetworks.xlsx")
  
  cat("tidying\n")
  # change the promo_ref so it can be matched to other jobs data
  sw$jobRef <- str_extract(sw$PROMO_REF, "\\d{2}[A-Z]{4}|\\d{8}-\\d{3}")
  sw$jobRef <- sub("-", "/", sw$jobRef)
  sw$jobRef[grepl("\\d{8}/\\d{3}", sw$jobRef)] <- paste0(sw$jobRef[grepl("\\d{8}/\\d{3}", sw$jobRef)], 0)
  sw$jobRef[grepl("\\d{8}/\\d{4}", sw$jobRef)] <- paste0(substr(sw$jobRef[grepl("\\d{8}/\\d{4}", sw$jobRef)], 1, 9), 
                                                         as.integer(substr(sw$jobRef[grepl("\\d{8}/\\d{4}", sw$jobRef)], 10, 13)))
  
  # dates
  sw$PLANNED_START_DATE <- as.POSIXct(sw$PLANNED_START_DATE)
  sw$ACTUAL_START_DATE <- as.POSIXct(sw$ACTUAL_START_DATE)
  sw$WORKS_END_DATE <- as.POSIXct(sw$WORKS_END_DATE)
  
  # factors
  sw$PHASE_TYPE <- factor(sw$PHASE_TYPE)
  sw$HA <- factor(sw$HA)
  sw$WORKS_TYPE <- factor(sw$WORKS_TYPE)
  sw$TOWN <- factor(sw$TOWN)
  sw$roadSize <- factor(sw$roadSize)
  cat("done")
  sw <<- sw
}
#### TRUNK MAINS DATABASE ####
importTMD <- function(){
  source("C:/Users/dylan/Documents/R/scripts/useful_functions.R")
  tmdb <- RODBC::sqlQuery(tmdCon, "select * from TrunkMainDatabase.dbo.Leak")
  tmdb$jobRef <- ex_job(tmdb$VistecRef)
  tmdb <<- tmdb
}
#### AIM TRUNK MAINS ####
importAIMTM <- function() {
  #### import ####
  aimTM <- read_delim("C:/Users/Dylan/Documents/work/Factbook/data/Trunk Mains_AIM.txt", 
                      "\t", escape_double = FALSE, 
                      col_types = cols(
                        X = col_skip(),
                        METRICCALCULATEDDIAMETER = col_skip(),
                        Y = col_skip(),
                        IMPERIALDIAMETERBAND = col_skip(),
                        METRICDIAMETERBAND = col_skip(),
                        HYDRAULICFAMILYTYPE = col_skip(),
                        FMZCODE = col_skip(),
                        REGION = col_skip(),
                        PRESSURE = col_skip(),
                        MAINLENGTH = col_skip(),
                        SIMULATEDBURSTLENGTH = col_skip(),
                        FREQUENCY = col_skip(),
                        MAINNAME = col_skip(),
                        BURST_SPREAD_SCORE = col_skip(),
                        ICS_BURST_FIT_ANNUAL_TUNED = col_skip(),
                        TWUL_BURST_PROB = col_skip(),
                        VCM_PROP_COUNT_ISOLATED = col_skip(),
                        VCM_PROP_COUNT_LOW_PRESS = col_skip(),
                        VCM_PROP_COUNT_SRC = col_skip(),
                        VIZ_IMPACT_PROPS = col_skip(),
                        VIZ_PROPS = col_skip(),
                        ITS_PRED_PROPS = col_skip(),
                        ITS_PRED_PROPS_3M = col_skip(),
                        EXISTINGINTERVENTION = col_skip(),
                        LOW_PRES_PRED_PROPS = col_skip(),
                        DURATION_0_3_HRS = col_skip(),
                        DURATION_MEAN = col_skip(),
                        DURATION_3_4_HRS = col_skip(),
                        DURATION_4_8_HRS = col_skip(),
                        DURATION_12_24_HRS = col_skip(),
                        DURATION_8_12_HRS = col_skip(),
                        DURATION_2_7_DAYS = col_skip(),
                        DURATION_24_48_HRS = col_skip(),
                        AMBULANCESTATION = col_skip(),
                        DURATION_OVER_7_DAYS = col_skip(),
                        AQUEDUCT = col_skip(),
                        BASEMENT = col_skip(),
                        COMMERCIAL = col_skip(),
                        EDUCATION = col_skip(),
                        FIRESTATION = col_skip(),
                        GOVERNMENT = col_skip(),
                        HOSPITAL = col_skip(),
                        INLANDWATER = col_skip(),
                        ELECTRICITYSUBSTATION = col_skip(),
                        LISTEDBUILDING = col_skip(),
                        MOTORWAYAROAD = col_skip(),
                        TUNNELENTRANCE = col_skip(),
                        POLICESTATION = col_skip(),
                        SUBWAY = col_skip(),
                        RAILWAY = col_skip(),
                        RAILWAYHIGHSPEED = col_skip(),
                        RAMSAR = col_skip(),
                        REDROUTE = col_skip(),
                        RESERVOIRSERVICEEMBANKMENT = col_skip(),
                        RESERVOIRSERVICEFLOOD = col_skip(),
                        RESERVOIRSTATUTORYEMBANKMENT = col_skip(),
                        RESERVOIRSTATUTORYFLOOD = col_skip(),
                        RESERVOIRSTORAGEEMBANKMENT = col_skip(),
                        RESERVOIRSTORAGEFLOOD = col_skip(),
                        RESIDENTIALPROPERTY = col_skip(),
                        SCHEDULEDMONUMENT = col_skip(),
                        SSSI = col_skip(),
                        UNDERGROUNDSTATION = col_skip(),
                        WATERTYPE = col_skip(),
                        WORLDHERITAGESITE = col_skip(),
                        COHORTID = col_skip(),
                        SUPERSTRINGID = col_skip(),
                        MAINS_MJR_ROAD = col_skip(),
                        SUPERSTRINGLENGTH = col_skip(),
                        MAINS_MNR_ROAD = col_skip(),
                        MAINS_MOTORWAY_ROAD = col_skip(),
                        LANDTYPE = col_skip(),
                        MAINS_RAIL = col_skip(),
                        COMPLEXITY = col_skip(),
                        FLOOD_SRC = col_skip(),
                        ID = col_skip(),
                        Intervention = col_skip()), 
                      trim_ws = TRUE)
  
  # tmAim <- tmAim %>% select(-SIMULATEDBURSTID)
  # tmAim <- tmAim %>% select(-DATE_INSTALL_BIN)
  aimTM <- unique(aimTM)
  aimTM <<- aimTM
}
#### MAINS ####
importMains <- function() {
  mains <- rgdal::readOGR("F:/2", "all_mains")
  mains <- dm@data
}
#### SPATIAL FEATURES ####
importDMAs <- function() {
  library(sf, quietly = TRUE)
  dmas <<- st_read("C:/Users/dylan/Documents/work/maps/dmaFixed.shp")
}

importFMZs <- function() {
  library(sf, quietly = TRUE)
  fmzs <<- st_read("C:/Users/dylan/Documents/work/maps/fmzFixed.shp")
}

importsystem8 <- function() {
  library(sf, quietly = TRUE)
  system8s <<- st_read("C:/Users/dylan/Documents/work/maps/system8.shp")
}