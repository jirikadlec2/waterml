#' GetVariables
#'
#' This function gets the table of variables from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with ?WSDL,
#'  for example: http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL
#' @keywords WaterML
#' @export
#' @examples
#' GetVariables("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL")

GetVariables <- function(server) {

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
    methodName <- "GetVariableInfoObject"

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(variable=""))
    response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction))
    status.code <- http_status(response)$category
    print(paste("GetVariables from", url, "...", status.code))
    WaterML <- content(response, as="text")
    SOAPdoc <- tryCatch({
      xmlRoot(xmlTreeParse(WaterML, getDTD=FALSE, useInternalNodes = TRUE))
    }, error = function(err) {
      print(paste("error fetching variables from URL:", url, err))
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
      print(paste("error fetching variables from URL:", url))
      return(NULL)
    })
  }
  if (version == "1.1") {
    vars <- doc[[2]]
  } else {
    vars <- doc[[1]]
  }
  N <- xmlSize(vars)
  #define the columns
  df <- data.frame(
    VariableCode=rep("",N), FullVariableCode=rep("",N),
    VariableName=rep("",N), ValueType=rep("",N),
    DataType=rep("",N), GeneralCategory=rep("",N),SampleMedium=rep("",N),
    UnitName=rep("",N), UnitType=rep("",N), UnitAbbreviation=rep("",N),
    NoDataValue=rep(NA,N), IsRegular=rep("",N),
    TimeUnitName=rep("",N), TimeUnitAbbreviation=rep("",N),
    TimeSupport=rep("",N), Speciation=rep("",N),
    stringsAsFactors=FALSE)

  for(i in 1:N) {
    varObj <- vars[[i]]
    v <- unlist(xmlToList(varObj))
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
  }
  return(df)
}
