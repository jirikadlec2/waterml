#' GetSiteInfo
#'
#' This function gets the table variables measured at a specific site from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .asmx or .wsdl,
#'  for example: http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL
#' @param siteCode The full site code, for example: default:Ru5BMMA. To get a list of
#' available site codes, see GetSites() function and use the FullSiteCode field.
#' @keywords waterml
#' @export
#' @examples
#' GetSiteInfo("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL",
#'              siteCode="default:Ru5BMMA")

GetSiteInfo <- function(server, siteCode) {

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    methodName <- "GetSiteInfoObject"

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(site=siteCode))
    response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction),
                     verbose())
    status.code <- http_status(response)$category
    WaterML <- content(response, as="text")
    SOAPdoc <- tryCatch({
      xmlRoot(xmlTreeParse(WaterML, getDTD=FALSE, useInternalNodes = TRUE))
    }, error = function(err) {
      print(paste("error fetching siteInfo from URL:", url, err))
      return(NULL)
    })
    #check soap:Header
    body <- 1
    if (xmlName(SOAPdoc[[1]]) == "Header") {
      body <- 2
    }
    #get the variablesResponse content element
    doc <- SOAPdoc[[body]][[1]][[1]]
    doc
  } else {
    #if the service is REST
    doc <- tryCatch({
      xmlRoot(xmlTreeParse(url, getDTD=FALSE, useInternalNodes = TRUE))
    }, error = function(err) {
      print(paste("error fetching siteInfo from URL:", url))
      return(NULL)
    })
  }

  #the seriesCatalog element
  sc <- doc[[2]][[2]]
  N <- xmlSize(sc)
  #define the columns for the output data frame
  df <- data.frame(SiteName=rep("",N),
                   SiteID=rep(NA,N),
                   SiteCode=rep("",N),
                   FullSiteCode=rep("",N),
                   Latitude=rep(NA,N),
                   Longitude=rep(NA,N),
                   Elevation=rep(NA,N),
                   State=rep("",N),
                   County=rep("",N),
                   Comments=rep("",N),
                   VariableCode=rep("",N),
                   FullVariableCode=rep("",N),
                   VariableName=rep("",N),
                   ValueType=rep("",N),
                   DataType=rep("",N),
                   GeneralCategory=rep("",N),
                   SampleMedium=rep("",N),
                   UnitName=rep("",N),
                   UnitType=rep("",N),
                   UnitAbbreviation=rep("",N),
                   NoDataValue=rep(NA,N),
                   IsRegular=rep("",N),
                   TimeUnitName=rep("",N),
                   TimeUnitAbbreviation=rep("",N),
                   TimeSupport=rep("",N),
                   Speciation=rep("",N),
                   methodID=rep("",N),
                   methodCode=rep("",N),
                   methodDescription=rep("",N),
                   methodLink=rep("",N),
                   sourceID=rep("",N),
                   organization=rep("",N),
                   sourceDescription=rep("",N),
                   citation=rep("",N),
                   qualityControlLevelID=rep("",N),
                   qualityControlLevelCode=rep("",N),
                   qualityControlLevelDefinition=rep("",N),
                   valueCount=rep(NA,N),
                   beginDateTime=rep(as.POSIXct(NA,"")),
                   endDateTime=rep(as.POSIXct(NA,"")),
                   stringsAsFactors=FALSE)

  #parse the site: only needed one-time
  siteInfo <- doc[[2]][[1]]
  s <- xmlToList(siteInfo)
  siteName <- s$siteName
  siteCode <- s$siteCode$text
  network <- s$siteCode$.attrs["network"]
  siteID <- ifelse(is.null(s$siteCode$.attrs["siteID"]), NA, s$siteCode$.attrs["siteID"])
  fullSiteCode <- paste(network, siteCode, sep=":")
  latitude <- as.numeric(s$geoLocation$geogLocation$latitude)
  longitude <- as.numeric(s$geoLocation$geogLocation$longitude)
  elevation <- ifelse(is.null(s$elevation_m), NA, s$elevation_m)

  comments <- NA
  state <- NA
  county <- NA

  numElements <- xmlSize(siteInfo)
  for (j in 1: numElements){
    element <- siteInfo[[j]]

    if (is.null(element)) {
      next
    }
    if (xmlName(element) != 'siteProperty') next

    attr <- xmlAttrs(element)["name"]
    if (attr == 'SiteComments') {
      comments <- xmlValue(element)
    }
    if (attr == 'Site Comments') {
      comments <- xmlValue(element)
    }
    if (attr == 'State') {
      state <- xmlValue(element)
    }
    if (attr == 'County') {
      county <- xmlValue(element)
    }
  }


  #parse the variables, methods, sources, qc levels
  i <- 1
  for(i in 1:N){

    series <- sc[[i]]
    serieList <- xmlToList(series)
    #variable-related fields
    v <- unlist(serieList$variable)

    varcode <- v["variableCode.text"]
    df$VariableCode[i] <- varcode
    df$FullVariableCode[i] <- paste(v["variableCode..attrs.vocabulary"], varcode, sep=":")
    df$VariableName[i] <- v["variableName"]
    df$ValueType[i] <- v["valueType"]
    df$DataType[i] <- v["dataType"]
    df$GeneralCategory[i] <- v["generalCategory"]
    df$SampleMedium[i] <- v["sampleMedium"]
    if (version == "1.1") {
      df$UnitName[i] <- v["unit.unitName"]
      df$UnitType[i] <- v["unit.unitType"]
      df$UnitAbbreviation[i] <- v["unit.unitAbbreviation"]
      df$IsRegular[i] <- ifelse(is.na(v["timeScale..attrs.isRegular"]), v["timeScale.isRegular"],
                                v["timeScale..attrs.isRegular"])
      df$TimeUnitName[i] <- v["timeScale.unit.unitName"]
      df$TimeUnitAbbreviation[i] <- v["timeScale.unit.unitAbbreviation"]
      df$TimeSupport[i] <- v["timeScale.timeSupport"]
      df$NoDataValue[i] <- as.numeric(v["noDataValue"])
    } else {
      df$UnitName[i] <- v["units.text"]
      df$UnitType[i] <- v["units..attrs.unitsType"]
      df$UnitAbbreviation[i] <- v["units..attrs.unitsAbbreviation"]
      df$IsRegular[i] <- ifelse(is.na(v["timeSupport..attrs.isRegular"]), v["timeSupport.isRegular"],
                                v["timeSupport..attrs.isRegular"])
      df$TimeUnitName[i] <- v["timeSupport.unit.UnitDescription"]
      df$TimeUnitAbbreviation[i] <- v["timeSupport.unit.UnitAbbreviation"]
      df$TimeSupport[i] <- v["timeSupport.timeInterval"]
      df$NoDataValue[i] <- as.numeric(v["NoDataValue"])
    }
    df$Speciation[i] <- v["speciation"]

    #method-related fields (use either code or id)
    if (version == "1.0") {
      meth <- serieList$Method
    } else {
      meth <- serieList$method
    }
    df$methodCode[i] <- ifelse(is.null(meth$methodCode), NA, meth$methodCode)
    df$methodDescription[i] <- ifelse(is.null(meth$methodDescription), NA, meth$methodDescription)
    df$methodLink[i] <- ifelse(is.null(meth$methodLink),NA,meth$methodLink)
    df$methodID[i] <- ifelse(is.null(meth$.attrs["methodID"]),NA,meth$.attrs["methodID"])
    if (is.na(df$methodID[i]) & !is.na(df$methodCode[i])) {
      df$methodID[i] <- df$methodCode[i]
    }
    if (is.na(df$methodCode[i]) & !is.na(df$methodID[i])) {
      df$methodCode[i] <- df$methodID[i]
    }
    #source-related fields
    if (version == "1.0") {
      src <- serieList$Source
      df$organization[i] <- src$Organization
      df$sourceDescription[i] <- src$SourceDescription
    } else {
      src <- serieList$source
      df$organization[i] <- src$organization
      df$sourceDescription[i] <- src$sourceDescription
    }

    df$citation[i] <- ifelse(is.null(src$citation), NA, src$citation)
    df$sourceID[i] <- ifelse(is.null(src$.attrs["sourceID"]),NA,src$.attrs["sourceID"])
    #quality control-related fields (use either code or id)
    if (version == "1.0") {
      qc <- serieList$QualityControlLevel
      df$qualityControlLevelID[i] <- ifelse(is.null(qc["QualityControlLevelID"]), NA, qc["QualityControlLevelID"])
    } else {
      qc <- serieList$qualityControlLevel
      df$qualityControlLevelID[i] <- ifelse(is.null(qc[".attrs"]["qualityControlLevelID"]), NA, qc[".attrs"]["qualityControlLevelID"])
    }


    df$qualityControlLevelCode[i] <- ifelse(is.null(qc["qualityControlLevelCode"]), NA, qc["qualityControlLevelCode"])
    if (is.na(df$qualityControlLevelID[i]) & !is.na(df$qualityControlLevelCode[i])) {
      df$qualityControlLevelID[i] <- df$qualityControlLevelCode[i]
    }
    if (is.na(df$qualityControlLevelCode[i]) & !is.na(df$qualityControlLevelID[i])) {
      df$qualityControlLevelCode[i] <- df$qualityControlLevelID[i]
    }

    df$qualityControlLevelDefinition[i] <- qc["definition"]
    #time interval-related fields
    df$valueCount[i] <- serieList$valueCount
    timeInterval <- serieList$variableTimeInterval
    df$beginDateTime[i] <- as.POSIXct(timeInterval$beginDateTime)
    df$endDateTime[i] <- as.POSIXct(timeInterval$endDateTime)
    #site related fields
    df$SiteName[i] <- siteName
    df$SiteID[i] <- siteID
    df$SiteCode[i] <- siteCode
    df$FullSiteCode[i] <- fullSiteCode
    df$Latitude[i] <- latitude
    df$Longitude[i] <- longitude
    df$Elevation[i] <- elevation
    df$Comments[i] <- comments
    df$State[i] <- state
    df$County[i] <- county


  }
  return(df)
}
