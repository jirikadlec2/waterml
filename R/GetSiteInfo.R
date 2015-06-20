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
#' server <- "http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx"
#' siteInfo <- GetSiteInfo(server, siteCode="default:Ru5BMMA")

GetSiteInfo <- function(server, siteCode) {

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }


  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
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

    print(paste("downloading SiteInfo from:", url))

    download.time <- system.time(response <- POST(url, body = envelope,
                      add_headers("Content-Type" = "text/xml","SOAPAction" = SOAPAction))
    )
    status.code <- http_status(response)$category

    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))

  } else {
    #if the service is REST
    print(paste("downloading SiteInfo from:", server))
    download.time <- system.time(response <- GET(server,
                     add_headers("Content-Type" = "text/xml","SOAPAction" = SOAPAction))
    )
    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
  }

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################
  doc <- content(response)

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  SiteName = xpathSApply(doc, "//sr:siteName", xmlValue, namespaces=ns)
  N <- length(SiteName)
  SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
  Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)

  SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
  SiteID <- unlist(SiteID)

  Latitude <- xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

  Elevation <- xpathSApply(doc, "//sr:elevation_m", xmlValue, namespaces=ns)
  numElevations <- length(Elevation)
  if (numElevations != N) {
    Elevation <- NA
  }

  # State, County, Comments: different tags for WaterML 1.0 and 1.1
  if (version=="1.1"){
    State = xpathSApply(doc, "//sr:siteProperty[@name='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:siteProperty[@name='County']", xmlValue, namespaces=ns)
    Comments = xpathSApply(doc, "//sr:siteProperty[@name='Site Comments']", xmlValue, namespaces=ns)
  } else {
    State = xpathSApply(doc, "//sr:note[@title='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:note[@title='County']", xmlValue, namespaces=ns)
    Comments = NA
  }
  # Check for empty values of state, county, comments
  numStates <- length(State)
  if (numStates != N) {
    State <- NA
  }
  numCounties <- length(County)
  if (numCounties != N) {
    County <- NA
  }
  numComments <- length(Comments)
  if (numComments != N) {
    Comments <- NA
  }

  VariableCode <- xpathSApply(doc, "//sr:variableCode", xmlValue, namespaces=ns)
  N <- length(VariableCode)

  VariableName <- xpathSApply(doc, "//sr:variableName", xmlValue, namespaces=ns)

  VariableID <- xpathSApply(doc, "//sr:variableCode", xmlGetAttr, name="variableID", namespaces=ns)
  VariableID <- unlist(VariableID)
  if (length(VariableID) == 0) { VariableID <- VariableCode }

  Vocabulary <- xpathSApply(doc, "//sr:variableCode", xmlGetAttr, name="vocabulary", namespaces=ns)

  #######################################################################################
  # START of SPECIAL CASE: process variable: use special case for WaterML 1.0           #
  #for WaterML 1.0 we must use a loop, because elements with unknown values are missing #
  #######################################################################################
  if (version == "1.0") {
    allVariables <- getNodeSet(doc, "//sr:series/sr:variable", namespaces=ns)
    i <- 1
    if (length(allVariables) < N) {
      print("Bad XML format: not enough details about the variables")
      return(NULL)
    }
    #allocate vectors for variables
    ValueType=rep("",N)
    DataType=rep("",N)
    GeneralCategory=rep("",N)
    SampleMedium=rep("",N)
    UnitName=rep("",N)
    UnitType=rep("",N)
    UnitAbbreviation=rep("",N)
    NoDataValue=rep(NA,N)
    IsRegular=rep("",N)
    TimeUnitName=rep("",N)
    TimeUnitAbbreviation=rep("",N)
    TimeSupport=rep("",N)
    Speciation=rep("",N)

    for (i in 1:N) {
      varObj <- allVariables[[i]]
      v <- unlist(xmlToList(varObj))
      ValueType[i] <- v["valueType"]
      DataType[i] <- v["dataType"]
      GeneralCategory[i] <- v["generalCategory"]
      SampleMedium[i] <- v["sampleMedium"]
      UnitName[i] <- v["units.text"]
      UnitType[i] <- v["units..attrs.unitsType"]
      UnitAbbreviation[i] <- v["units..attrs.unitsAbbreviation"]
      IsRegular[i] <- ifelse(is.na(v["timeSupport..attrs.isRegular"]), v["timeSupport.isRegular"],
                                v["timeSupport..attrs.isRegular"])
      TimeUnitName[i] <- v["timeSupport.unit.UnitDescription"]
      TimeUnitAbbreviation[i] <- v["timeSupport.unit.UnitAbbreviation"]
      TimeSupport[i] <- v["timeSupport.timeInterval"]
      NoDataValue[i] <- as.numeric(v["NoDataValue"])
    }

    MethodID <- xpathSApply(doc, "//sr:Method", xmlGetAttr, name="methodID", namespaces=ns)
    MethodCode <- xpathSApply(doc, "//sr:MethodCode", xmlValue, namespaces=ns)

    MethodDescription <- xpathSApply(doc, "//sr:MethodDescription", xmlValue, namespaces=ns)
    if (length(MethodDescription) < N) { MethodDescription <- NA }

    MethodLink <- xpathSApply(doc, "//sr:MethodLink", xmlValue, namespaces=ns)
    if (length(MethodLink) < N) { MethodLink <- NA }

    if (length(MethodID) < N & length(MethodCode) == N) {
      MethodID <- MethodCode
    }
    if (length(MethodCode) < N & length(MethodID) == N) {
      MethodCode <- MethodID
    }
    if (length(MethodID) < N) { MethodID <- NA }
    if (length(MethodCode) < N) { MethodCode <- NA }

    SourceID <- xpathSApply(doc, "//sr:Source", xmlGetAttr, name="sourceID", namespaces=ns)
    if (length(SourceID) < N) { SourceID <- NA }

    Organization <- xpathSApply(doc, "//sr:Organization", xmlValue, namespaces=ns)
    if (length(Organization) < N) { Organization <- NA }

    SourceDescription <- xpathSApply(doc, "//sr:SourceDescription", xmlValue, namespaces=ns)
    if (length(SourceDescription) < N) { SourceDescription <- NA }

    Citation <- xpathSApply(doc, "//sr:Citation", xmlValue, namespaces=ns)
    if (length(Citation) < N) { Citation <- NA }

    QualityControlLevelID=xpathSApply(doc, "//sr:QualityControlLevel", xmlGetAttr,
                                      name="QualityControlLevelID", namespaces=ns)
    QualityControlLevelCode=xpathSApply(doc, "//sr:QualityControlLevelCode", xmlValue, namespaces=ns)

    if (length(QualityControlLevelID) < N & length(QualityControlLevelCode == N)) {
      QualityControlLevelID <- QualityControlLevelCode
    }
    if (length(QualityControlLevelCode) < N & length(QualityControlLevelID == N)) {
      QualityControlLevelCode <- QualityControlLevelID
    }
    if (length(QualityControlLevelID) < N) { QualityControlLevelID <- NA }
    if (length(QualityControlLevelCode) < N) { QualityControlLevelCode <- NA }

    QualityControlLevelDefinition=xpathSApply(doc, "//sr:Definition", xmlValue, namespaces=ns)
    if (length(QualityControlLevelDefinition) < N) { QualityControlLevelDefinition <- NA }
  #################################################################################################
  # END of SPECIAL CASE of WaterML 1.0                                                            #
  #################################################################################################
  } else {

    ValueType <- xpathSApply(doc, "//sr:valueType", xmlValue, namespaces=ns)
    DataType <- xpathSApply(doc, "//sr:dataType", xmlValue, namespaces=ns)
    GeneralCategory <- xpathSApply(doc, "//sr:generalCategory", xmlValue, namespaces=ns)
    SampleMedium <- xpathSApply(doc, "//sr:sampleMedium", xmlValue, namespaces=ns)

    UnitName <- xpathSApply(doc, "//sr:units/sr:unitName", xmlValue, namespaces=ns)
    UnitType <- xpathSApply(doc, "//sr:units/sr:unitType", xmlValue, namespaces=ns)
    UnitAbbreviation <- xpathSApply(doc,
      "//sr:variable/sr:units/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]",
      xmlValue, namespaces=ns)

    #if UnitName is not found, then we look for /variable/unit instead
    if (length(UnitName) == 0) {
      UnitName <- xpathSApply(doc, "//sr:variable/sr:unit/sr:unitName", xmlValue, namespaces=ns)
      UnitType <- xpathSApply(doc, "//sr:variable/sr:unit/sr:unitType", xmlValue, namespaces=ns)
      UnitAbbreviation <- xpathSApply(doc,
        "//sr:variable/sr:unit/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]",
        xmlValue, namespaces=ns)
    }

    NoDataValue <- xpathSApply(doc, "//sr:noDataValue", xmlValue, namespaces=ns)

    IsRegular <- xpathSApply(doc, "//sr:timeScale", xmlGetAttr, name="isRegular", namespaces=ns)
    IsRegular <- unlist(IsRegular)
    if (length(IsRegular) < N) {
      IsRegular <- (DataType != "Sporadic")
    }

    TimeUnitName <- xpathSApply(doc, "//sr:timeScale/sr:unit/sr:unitName", xmlValue, namespaces=ns)
    TimeUnitAbbreviation <- xpathSApply(doc,
      "//sr:timeScale/sr:unit/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]", xmlValue, namespaces=ns)

    TimeSupport <- xpathSApply(doc, "//sr:timeSupport", xmlValue, namespaces=ns)
    Speciation <- xpathSApply(doc, "//sr:variable/sr:speciation", xmlValue, namespaces=ns)

    BeginDateTime <- xpathSApply(doc, "//sr:beginDateTime", xmlValue, namespaces=ns)
    EndDateTime <- xpathSApply(doc, "//sr:endDateTime", xmlValue, namespaces=ns)

    BeginDateTimeUTC <- xpathSApply(doc, "//sr:beginDateTimeUTC", xmlValue, namespaces=ns)
    if (length(BeginDateTimeUTC) == 0) { BeginDateTimeUTC <- BeginDateTime }

    EndDateTimeUTC <- xpathSApply(doc, "//sr:endDateTimeUTC", xmlValue, namespaces=ns)
    if (length(EndDateTimeUTC) == 0) { EndDateTimeUTC <- EndDateTime }

    ValueCount <- xpathSApply(doc, "//sr:valueCount", xmlValue, namespaces=ns)

    MethodID <- xpathSApply(doc, "//sr:method", xmlGetAttr, name="methodID", namespaces=ns)

    MethodCode <- xpathSApply(doc, "//sr:methodCode", xmlValue, namespaces=ns)

    MethodDescription <- xpathSApply(doc, "//sr:methodDescription", xmlValue, namespaces=ns)
    if (length(MethodDescription) < N) { MethodDescription <- NA }

    MethodLink <- xpathSApply(doc, "//sr:methodLink", xmlValue, namespaces=ns)
    if (length(MethodLink) < N) { MethodLink <- NA }

    if (length(MethodID) < N & length(MethodCode) == N) {
      MethodID <- MethodCode
    }
    if (length(MethodCode) < N & length(MethodID) == N) {
      MethodCode <- MethodID
    }
    if (length(MethodID) < N) { MethodID <- NA }
    if (length(MethodCode) < N) { MethodCode <- NA }

    SourceID <- xpathSApply(doc, "//sr:source", xmlGetAttr, name="sourceID", namespaces=ns)
    if (length(SourceID) < N) { SourceID <- NA }

    Organization <- xpathSApply(doc, "//sr:organization", xmlValue, namespaces=ns)
    if (length(Organization) < N) { Organization <- NA }

    SourceDescription <- xpathSApply(doc, "//sr:sourceDescription", xmlValue, namespaces=ns)
    if (length(SourceDescription) < N) { SourceDescription <- NA }

    Citation <- xpathSApply(doc, "//sr:citation", xmlValue, namespaces=ns)
    if (length(Citation) < N) { Citation <- NA }

    QualityControlLevelID=xpathSApply(doc, "//sr:qualityControlLevel", xmlGetAttr,
                                      name="qualityControlLevelID", namespaces=ns)
    QualityControlLevelCode=xpathSApply(doc, "//sr:qualityControlLevelCode", xmlValue, namespaces=ns)

    if (length(QualityControlLevelID) < N & length(QualityControlLevelCode == N)) {
      QualityControlLevelID <- QualityControlLevelCode
    }
    if (length(QualityControlLevelCode) < N & length(QualityControlLevelID == N)) {
      QualityControlLevelCode <- QualityControlLevelID
    }
    if (length(QualityControlLevelID) < N) { QualityControlLevelID <- NA }
    if (length(QualityControlLevelCode) < N) { QualityControlLevelCode <- NA }

    QualityControlLevelDefinition=xpathSApply(doc, "//sr:definition", xmlValue, namespaces=ns)
    if (length(QualityControlLevelDefinition) < N) { QualityControlLevelDefinition <- NA }
  }

  #define the columns for the output data frame
  df <- data.frame(SiteName=rep(SiteName, N),
                   SiteID=rep(SiteID, N),
                   SiteCode=rep(SiteCode, N),
                   FullSiteCode = rep(paste(Network, SiteCode, sep=":"), N),
                   Latitude=rep(as.numeric(Latitude), N),
                   Longitude=rep(as.numeric(Longitude), N),
                   Elevation=rep(as.numeric(Elevation), N),
                   State=rep(State, N),
                   County=rep(County, N),
                   Comments=rep(Comments, N),
                   VariableCode=VariableCode,
                   FullVariableCode=paste(Vocabulary, VariableCode, sep=":"),
                   VariableName=VariableName,
                   ValueType=ValueType,
                   DataType=DataType,
                   GeneralCategory=GeneralCategory,
                   SampleMedium=SampleMedium,
                   UnitName=UnitName,
                   UnitType=UnitType,
                   UnitAbbreviation=UnitAbbreviation,
                   NoDataValue=as.numeric(NoDataValue),
                   IsRegular=IsRegular,
                   TimeUnitName=TimeUnitName,
                   TimeUnitAbbreviation=TimeUnitAbbreviation,
                   TimeSupport=TimeSupport,
                   Speciation=Speciation,
                   methodID=MethodID,
                   methodCode=MethodCode,
                   methodDescription=MethodDescription,
                   methodLink=MethodLink,
                   sourceID=SourceID,
                   organization=Organization,
                   sourceDescription=SourceDescription,
                   citation=Citation,
                   qualityControlLevelID=QualityControlLevelID,
                   qualityControlLevelCode=QualityControlLevelCode,
                   qualityControlLevelDefinition=QualityControlLevelDefinition,
                   valueCount=ValueCount,
                   beginDateTime=as.POSIXct(strptime(BeginDateTime, "%Y-%m-%dT%H:%M:%S")),
                   endDateTime=as.POSIXct(strptime(EndDateTime, "%Y-%m-%dT%H:%M:%S")),
                   beginDateTimeUTC=as.POSIXct(strptime(BeginDateTimeUTC, "%Y-%m-%dT%H:%M:%S")),
                   endDateTimeUTC=as.POSIXct(strptime(EndDateTimeUTC, "%Y-%m-%dT%H:%M:%S")),
                   stringsAsFactors=FALSE)

  return(df)
}
