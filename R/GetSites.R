#' GetSites
#'
#' This function gets the table of sites from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .WSDL,
#'  for example: http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL
#'  alternatively this can be the REST URL to get the sites.
#' @keywords waterml
#' @export
#' @examples
#' GetSites("http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL")

GetSites <- function(server) {

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    url <- paste(server, "?site=&authToken=", sep="")
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    if (version == "1.0") {
      methodName <- "GetSites"
    } else {
      methodName <- "GetSitesObject"
    }
    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName)
    response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction),
                     verbose())
    status.code <- http_status(response)$category
    WaterML <- content(response, as="text")
    SOAPdoc <- tryCatch({
      xmlRoot(xmlTreeParse(WaterML, getDTD=FALSE, useInternalNodes = TRUE))
    }, error = function(err) {
      print(paste("error fetching sites from URL:", url))
      return(NULL)
    })
    #get the sitesResponse content element
    doc <- SOAPdoc[[2]][[1]][[1]]
    if (is.null(doc)) {
      doc <- SOAPdoc[[1]][[1]][[1]]
    }
    doc
  } else {
    #if the service is REST
    doc <- tryCatch({
      xmlRoot(xmlTreeParse(url, getDTD=FALSE, useInternalNodes = TRUE))
    }, error = function(err) {
      print(paste("error fetching sites from URL:", url))
      return(NULL)
    })
  }


  N <- xmlSize(doc) - 1 #because first element is queryInfo

  df <- data.frame(SiteName=rep("",N),
                   SiteID=rep(NA, N),
                   SiteCode=rep("",N),
                   FullSiteCode=rep("",N),
                   Latitude=rep(NA,N),
                   Longitude=rep(NA,N),
                   Elevation=rep(NA,N),
                   State=rep("",N),
                   County=rep("",N),
                   Comments=rep("",N),
                   stringsAsFactors=FALSE)

  for(i in 1:N){

    siteInfo <- doc[[i+1]][[1]]
    siteList <- xmlToList(siteInfo)
    siteName <- siteList$siteName
    sCode <- siteList$siteCode
    siteCode <- sCode$text
    siteID <- ifelse(is.null(sCode$.attrs["siteID"]), siteCode, sCode$.attrs["siteID"])
    network <- sCode$.attrs["network"]
    fullSiteCode <- paste(network, siteCode, sep=":")
    latitude <- as.numeric(siteList$geoLocation$geogLocation$latitude)
    longitude <- as.numeric(siteList$geoLocation$geogLocation$longitude)
    elevation <- ifelse(is.null(siteList$elevation_m), NA, siteList$elevation_m)

    comments <- NA
    state <- NA
    county <- NA

    numElements <- xmlSize(siteInfo)
    for (j in 1: numElements){
      element <- siteInfo[[j]]

      if (is.null(element)) {
        print ('element is null!')
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
    df$SiteName[i] <- siteName
    df$SiteCode[i] <- siteCode
    df$SiteID[i] <- siteID
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
