#### EXCEL I/O ####
xli <- function() {
  # import clipboard contents
  read.table("clipboard", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  }

xlo <- function(df) {
  write.table(df, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)
}

#### SPATIAL ####
latLongFromXY <- function(df, x, y){
  df <- sp::SpatialPointsDataFrame(df[c(x, y)], df, proj4string = sp::CRS("+init=epsg:27700"))
  df <- sp::spTransform(df, "+init=epsg:4326")
  df <- cbind(df@data, lat = df@coords[, 2], lon = df@coords[, 1])
  zeros <- df[[x]] == 0
  df$lat[zeros] <- NA
  df$lon[zeros] <- NA
  df
}

# produces 7 digit XYs - find out why 
xyFromLatLong <- function(df, lat, lon){
  df <- sp::SpatialPointsDataFrame(df[c(lon, lat)], df, proj4string = sp::CRS("+init=epsg:4326"))
  df <- sp::spTransform(df, "+init=epsg:27700")
  df <- cbind(df@data, x = df@coords[, 1], y = df@coords[, 2])
  zeros <- df[[lat]] == 0
  df$x[zeros] <- NA
  df$y[zeros] <- NA
  df
}

sq_pointsToArea <- function(df, coord1, coord2, shp) {
  # make df spatial
  # over query
  # combine and convert to df
  df <- sp::SpatialPointsDataFrame(coords = df[c(coord1, coord2)], 
                                   data = df, proj4string = sp::CRS("+init=epsg:27700"))
  shp <- sp::spTransform(shp, CRSobj = sp::proj4string(df))
  o <- sp::over(df, shp)
  df <- cbind(df@data, o)
  df
}

#### GEOCODING ####
geocodeAddress <- function(x, pc = FALSE) {
  urlpt1 <- "http://dev.virtualearth.net/REST/v1/Locations?culture=en-GB&addressLine="
  urlpt2 <- "&userLocation=51.504360719046616,-0.12600176611298197&key="
  urlpt3 <- Sys.getenv('bing_key')
  url <- paste0(urlpt1, x, urlpt2, urlpt3)
  bingResult <- jsonlite::fromJSON(URLencode(url))
  postcode <- bingResult$resourceSets$resources[[1]]$address$postalCode[1]
  lat <- bingResult$resourceSets$resources[[1]]$point[, 2][[1]][1]
  lon <- bingResult$resourceSets$resources[[1]]$point[, 2][[1]][2]
  if(is.null(postcode)) postcode <- NA
  if(is.null(lat)) lat <- NA
  if(is.null(lon)) lon <- NA
  if(pc == TRUE) {
    result <- data.frame(postcode, lat, lon, stringsAsFactors = FALSE)
  } else {
    result <- data.frame(lon, lat, stringsAsFactors = FALSE)
  }
  result
}

geocodeCoordinates <- function(lat, lon) {
  urlpt1 <- "http://dev.virtualearth.net/REST/v1/Locations/"
  urlpt2 <- paste(lat, lon, sep = ",")
  urlpt3 <- "?key="
  urlpt4 <- Sys.getenv('bing_key')
  rgURL <- paste0(urlpt1, urlpt2, urlpt3, urlpt4)
  
  result <- jsonlite::fromJSON(rgURL)
  address <- result$resourceSets$resources[[1]]$address
  
  # these can be null which means the data frame call doesn't work
  formattedAddress <- address$formattedAddress
  street <- address$intersection$baseStreet
  locality <- address$locality
  postcode <- address$postalCode
  intersectionType <- address$intersection$intersectionType
  secondaryStreet1 <- address$intersection$secondaryStreet1
  secondaryStreet2 <- address$intersection$secondaryStreet2
  confidence <- result$resourceSets$resources[[1]]$confidence
  if(is.null(formattedAddress)) formattedAddress <- NA
  if(is.null(street)) street <- NA
  if(is.null(locality)) locality <- NA
  if(is.null(postcode)) postcode <- NA
  if(is.null(intersectionType)) intersectionType <- NA
  if(is.null(secondaryStreet1)) secondaryStreet1 <- NA
  if(is.null(secondaryStreet2)) secondaryStreet2 <- NA
  
  output <- data.frame(address = formattedAddress, street = street, locality = locality, postcode = postcode, 
                       intersectionType = intersectionType, secondaryStreet1 = secondaryStreet1, 
                       secondaryStreet2 = secondaryStreet2, confidence = confidence, stringsAsFactors = FALSE)
  output
}

geocodeCoordinatesLoop <- function(df, latCol, lonCol) {
  loopCount <- nrow(df)
  resultsList <- vector("list", loopCount)
  entry <- df[i, ]
  for(i in 1:loopCount) {
    resultsList[[i]] <- geocodeCoordinates(entry[[latCol]], entry[[lonCol]])
  }
  resultsList
}

#### COMPARISON ####
# returns the straight line distance between two sets of XY coordinates
compareCoordinates <- function(x1, y1, x2, y2) {
  sqrt(((x1 - x2)^2) + ((y1 - y2)^2))
}

# returns the difference in seconds between two dates
compareDates <- function(date1, date2, abs = TRUE) {
  if(abs == TRUE) {
    abs(date1 - date2)
  } else {
    date1 - date2
  }
}

# some method for comparing two addresses
compareAddress <- function(x, y, compMethod = "L") {
  if(compMethod == "L") {
    adist(x, y)[1]
  } else {
    stringdist::stringdist(x, y, method = "jw")
  }
  # refinr::key_collision_merge(x, y)
}
#### EXTRACTING ####
# BULLETIN BOARD / CASE ID
ex_case <- function(x){
  stringr::str_match(toupper(x), "(B.B.|BB|BB CASE|BULLETIN BOARD|BULLETIN BOARD NO)(:|;|-|,|\\s)?\\s?(\\d{6})")[, 4]
}

# JOB REF
ex_job <- function(x){
  stringr::str_extract(toupper(x), "\\d{2}[A-Z]{4}|\\d{8}")
}

## PIPE SIZE
ex_inches <- function(x, units = FALSE) {
  x <- gsub("[^[:graph:]]", " ", x)
  x <- gsub('"', '^', x)
  u <- ifelse(units == FALSE, 2, 1)
  stringr::str_match(toupper(x), "(\\d{1,2})\\s?(\\^|IN)")[, u]
}

ex_mm <- function(x, units = FALSE) {
  x <- stringr::str_replace_all(x,"[^[:graph:]]", " ")
  u <- ifelse(units == FALSE, 2, 1)
  stringr::str_match(toupper(x), "(\\d{1,3}\\.?\\d{0,2})MM")[, u]
}

ex_pipeSize <- function(x) {
  x <- gsub('"', '^', x)
  x <- gsub("\\\\", "^", x)
  x <- stringr::str_replace_all(x,"[^[:graph:]]", " ")
  r <- ifelse(!is.na(ex_mm(x, units = TRUE)), ex_mm(x, units = TRUE), ex_inches(x, units = TRUE))
  r <- ifelse(grepl("MM", r) == TRUE, as.double(stringr::str_extract(r, "\\d+")) / 25.4, as.double(stringr::str_extract(r, "\\d+")))
  round(r, 2)
}

# OLD VERSION
# ex_pipeSize <- function(x, metric = TRUE) {
#   x <- gsub('"', '^', x)
#   x <- gsub("\\\\", "^", x)
#   x <- stringr::str_replace_all(x,"[^[:graph:]]", " ")
#   if(metric == TRUE) {
#     ifelse(!is.na(ex_mm(x)), round(as.double(ex_mm(x)), 3), round(as.double(ex_inches(x)) * 25.4, 3))
#   } else {
#     ifelse(!is.na(ex_mm(x)), round(as.double(ex_mm(x)) / 25.4, 3), round(as.double(ex_inches(x)), 3))
#   }
# }

# METER REF
ex_meter <- function(x){
  str_match(toupper(x), "(DM|ZM|DI)\\s?(/|\\.|,|;|:|-)?\\s?\\d{2,6}")[, 1]
}

# VALVE REF
ex_valve <- function(x){
  str_match(toupper(x), "(CONTROL REF|DBV|DPV|PBV|PRV|NL|SL|PR|VALVE|VALVE REF|ZBV)\\s?(/|\\.|,|;|:|-)?\\s?\\d{2,8}")[, 1]
}

#### WRANGLING ####
infillValues <- function(df1id, df1missing, df2, id, values) {
  if(nchar(df1missing) > 0) {
    df1missing
  } else {
    df2[[values]][df2[[id]] == df1id][1]
  }
}

# to run this on a data frame
# mapply(infill, test1$id, test1$name, MoreArgs = list(test2, "ref", "names"))
# runs rowwise on the first two arguments line by line

netbase_sap_refs <- function(x) {
  paste0(substr(x, 5, 12), "/", as.integer(substr(x, 14, 17)))
}

remove_breaks <- function(x) {
  gsub("[\r\n\t\v\f]+", "", x)
}

remove_unicode <- function(x) {
  iconv(x, "latin1", "ASCII", sub="")
}

# "\t\n\r\v\f" # space, tab, newline, carriage return, vertical tab, form feed

charType <- function(x) {
  case_when(
    grepl("[a-z]", tolower(x)) == TRUE ~ "A",
    grepl("[0-9]", tolower(x)) == TRUE ~ "1",
    TRUE ~ x)
}

regexPattern <- function(x) {
  x <- str_split(x, "")[[1]]
  str_flatten(charType(x))
}


xldateToPosix <- function(x) {
  as.POSIXct(x * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")}

arYear <- function(x) {
  ifelse(lubridate::month(x) < 4, lubridate::year(x), lubridate::year(x) + 1)
}

# this function takes a data frame column and returns values scaled from 0 to 1
scale01 <- function(col) {
  newcol <- c()
  for (val in col) {
    newcol <- c(newcol, ((val - min(col, na.rm = TRUE)) / 
                           (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))))
  }
  newcol
}