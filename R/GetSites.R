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
#' sites <- GetSites("http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL")

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

    print(paste("downloading sites from:", url, "..."))
    start.time <-  Sys.time()

    response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction))
    status.code <- http_status(response)$category

    end.time <- Sys.time()

    print(paste("download time:", format(end.time - start.time), "status:", status.code))

    doc <- content(response)

    # specify the namespace information
    ns <- WaterOneFlowNamespace(version)

    # extract the data columns with XPath
    SiteName = xpathSApply(doc, "//sr:siteName", xmlValue, namespaces=ns)
    SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
    Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)
    Latitude = xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
    Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

    SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
    SiteID <- ifelse(length(SiteID == 0), SiteCode, SiteID)

    Elevation <- xpathSApply(doc, "//sr:elevation_m", xmlValue, namespaces=ns)
    if (length(Elevation) == 0) {
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
    if (length(State) == 0) {
      State <- NA
    }
    if (length(County) == 0) {
      County <- NA
    }
    if (length(Comments) == 0) {
      Comments <- NA
    }

    df <- data.frame(
      SiteID = ifelse(length(SiteID == 0), SiteCode, SiteID),
      SiteName = SiteName,
      SiteCode = SiteCode,
      FullSiteCode = paste(Network, SiteCode, sep=":", collapse=""),
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      Elevation = as.numeric(Elevation),
      State = State,
      County = County,
      Comments = Comments,
      stringsAsFactors = FALSE)

    return(df)

  } else {
    #if the service is REST
    return(NULL)
  }
}
