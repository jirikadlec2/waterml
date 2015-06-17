#' GetSites
#'
#' This function gets the table of sites from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .WSDL,
#'  for example: http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL
#'  alternatively this can be the REST URL to get the sites.
#' @param west Optional parameter: The west longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param south Optional parameter: The south latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @param east Optional parameter: The east longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param north Optional parameter: The north latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @keywords waterml
#' @export
#' @examples
#' #Getting all sites from a service
#' sites <- GetSites("http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL")
#'
#' #Getting a subset of sites restricted by geographical area
#' server <- "http://drought.usu.edu/usbrreservoirs/cuahsi_1_1.asmx?WSDL"
#' sites_subset <- GetSites(server, west=-113.0, south=35.0, east=110.0, north=40.0)

GetSites <- function(server, west=NULL, south=NULL, east=NULL, north=NULL) {

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

    #choose the right SOAP web method based on WaterML version and parameters
    if (version == "1.0") {
      methodName <- "GetSites"
      envelope <- MakeSOAPEnvelope(namespace, methodName)
    } else {
      if (is.null(west) | is.null(south) | is.null(north) | is.null(east)) {
        methodName <- "GetSitesObject"
        envelope <- MakeSOAPEnvelope(namespace, methodName)
      } else {
        methodName <- "GetSitesByBoxObject"
        envelope <- MakeSOAPEnvelope(namespace, methodName,
                      parameters=c(west=west, south=south, north=north, east=east,IncludeSeries="false"))
      }
    }
    SOAPAction <- paste(namespace, methodName, sep="")


    print(paste("downloading sites from:", url, "..."))

    download.time <- system.time(response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction)))
    status.code <- http_status(response)$category

    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))

    print("reading sites WaterML data...")
    doc <- tryCatch({
      content(response)
    }, warning = function(w) {
      print("Error reading WaterML: Bad XML format.")
      return(NULL)
    }, error = function(e) {
      print("Error reading WaterML: Bad XML format.")
      return(NULL)
    }
    )
    if (is.null(doc)) {
      return(NULL)
    }

    # specify the namespace information
    ns <- WaterOneFlowNamespace(version)

    # extract the data columns with XPath
    SiteName = xpathSApply(doc, "//sr:siteName", xmlValue, namespaces=ns)
    SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
    Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)
    Latitude = xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
    Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

    SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
    SiteID <- unlist(SiteID)

    numSiteIDs <- length(SiteID)
    numSites <- length(SiteCode)
    if (numSiteIDs != numSites) {
      SiteID <- SiteCode
    }

    Elevation <- xpathSApply(doc, "//sr:elevation_m", xmlValue, namespaces=ns)
    numElevations <- length(Elevation)
    if (numElevations != numSites) {
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
    if (numStates != numSites) {
      State <- NA
    }
    numCounties <- length(County)
    if (numCounties != numSites) {
      County <- NA
    }
    numComments <- length(Comments)
    if (numComments != numSites) {
      Comments <- NA
    }

    df <- data.frame(
      SiteID = SiteID,
      SiteName = SiteName,
      SiteCode = SiteCode,
      FullSiteCode = paste(Network, SiteCode, sep=":"),
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
