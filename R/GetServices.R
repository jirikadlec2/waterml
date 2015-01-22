#' GetServices
#'
#' This function gets the table of web services from the HIS Central catalog
#'
#' @import XML
#' @importFrom RCurl getURL
#' @keywords waterml
#' @export
#' @examples
#' GetServices()

GetServices <- function() {
  catalog = "http://hiscentral.cuahsi.org/webservices/hiscentral.asmx/GetWaterOneFlowServiceInfo"
  text = RCurl::getURL(catalog)
  doc <- xmlRoot(xmlTreeParse(text, getDTD=FALSE, useInternalNodes = TRUE))
  N <- xmlSize(doc) - 1 #because first element is queryInfo

  df <- data.frame(url=rep("",N), title=rep("",N), descriptionURL=rep("",N),
                   organization=rep("",N), citation=rep("",N),abstract=rep("",N),
                   valuecount=rep("",N), variablecount=rep("",N), sitecount=rep("",N),
                   id=rep(NA,N), networkName=rep("",N), minLon=rep(NA,N),
                   minLat=rep(NA,N), maxLon=rep(NA,N), maxLat=rep(NA,N),
                   stringsAsFactors=FALSE)

  for(i in 1:N){

    e <- doc[[i]]
    df[i]$url <- e[["servURL"]]
    df[i]$title <- e[["Title"]]
    df[i]$descriptionURL <- e[["ServiceDescriptionURL"]]
    df[i]$organization <- e[["organization"]]
    df[i]$citation <- e[["citation"]]
    df[i]$abstract <- e[["aabstract"]]
    df[i]$valuecount <- e[["valuecount"]]
    df[i]$sitecount <- e[["sitecount"]]
    df[i]$id <- e[["ServiceID"]]
    df[i]$networkName <- e[["NetworkName"]]
    df[i]$minLon <- e[["minx"]]
    df[i]$minLat <- e[["miny"]]
    df[i]$maxLat <- e[["maxx"]]
    df[i]$maxLon <- e[["maxy"]]

  }
  return(df)
}
