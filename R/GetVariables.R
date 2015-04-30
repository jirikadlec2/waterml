#' GetVariables
#'
#' This function gets the table of variables from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx
#' @keywords waterml
#' @export
#' @examples
#' GetVariables("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx")

GetVariables <- function(server) {
  #remove everything after .asmx
  m <- regexpr(".asmx", server)
  base.url <- substr(server, 0, m+nchar(".asmx")-1)
  variables.url <- paste(base.url, "/GetVariablesObject?authToken=", sep="")
  doc <- xmlRoot(xmlTreeParse(variables.url, getDTD=FALSE, useInternalNodes = TRUE))
  vars <- doc[[2]]
  N <- xmlSize(vars)
  #define the columns
  df <- data.frame(VariableCode=rep("",N), FullVariableCode=rep("",N),
                   VariableName=rep("",N), ValueType=rep("",N),
                   DataType=rep("",N), GeneralCategory=rep("",N),SampleMedium=rep("",N),
                   UnitName=rep("",N), UnitType=rep("",N), UnitAbbreviation=rep("",N),
                   NoDataValue=rep(NA,N), IsRegular=rep("",N),
                   TimeUnitName=rep("",N), TimeUnitAbbreviation=rep("",N),
                   TimeSupport=rep("",N), Speciation=rep("",N), stringsAsFactors=FALSE)
  for(i in 1:N) {
    varObj <- vars[[i]]
    v <- xmlToList(varObj)
    varcode <- v$variableCode$text
    df$VariableCode[i] <- varcode
    df$FullVariableCode[i] <- paste(v$variableCode$.attrs["vocabulary"], varcode, sep=":")
    df$VariableName[i] <- v$variableName
    df$ValueType[i] <- v$valueType
    df$DataType[i] <- v$dataType
    df$GeneralCategory[i] <- v$generalCategory
    df$SampleMedium[i] <- v$sampleMedium
    df$UnitName[i] <- v$unit$unitName
    df$UnitType[i] <- v$unit$unitType
    df$UnitAbbreviation[i] <- v$unit$unitAbbreviation
    df$NoDataValue <- as.numeric(v$noDataValue)
    df$IsRegular <- v$timeScale$.attrs["isRegular"]
    df$TimeUnitName <- v$timeScale$unit$unitName
    df$TimeUnitAbbreviation <- v$timeScale$unit$unitAbbreviation
    df$TimeSupport <- v$timeScale$timeSupport
    df$Speciation <- v$speciation
  }
  return(df)
}
