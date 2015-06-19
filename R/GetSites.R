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
#' @return a data.frame of sites. The data.frame has the following columns:
#' \itemize{
#' \item SiteID: The site ID in the original database
#' \item SiteName: The name of the site
#' \item SiteCode: A short unique code of the site
#' \item FullSiteCode: The complete unique code of the site in the format NETWORK:CODE.
#'               Use this value in the GetSiteInfo and GetValues functions
#' \item Latitude:  The WGS84 latitude in decimal degrees
#' \item Longitude: The WGS84 longitude in decimal degrees
#' \item Elevation: The elevation of the site above sea level in meters
#' \item State:     Only for sites in the USA: the state of the site
#' \item County:    Only for sites in the USA: The county of the site
#' \item Comments:  Additional comments about the sites (note: this field is often empty)
#' }
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

  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    # in other cases we leave the URL as it is
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

    #in case of server error, print the error and exit
    if (status.code == "server error") {
      print(http_status(response)$message)
      return(NULL)
    }
  } else {
    # If the service is REST:
    print(paste("downloading sites from:", server, "..."))

    download.time <- system.time(
      tryCatch({
        downloaded <- FALSE
        response <- GET(server)
        downloaded <- TRUE
      },error=function(e){
        print(conditionMessage(e))
      })
    )

    if (!downloaded) {
      return(NULL)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
  }
  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

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

  N <- length(SiteName)
  bigData <- 10000
  if (N > bigData) {
    print(paste("found", N,"sites"))
    print("processing SiteCode...")
  }
  SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
  Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)

  SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
  SiteID <- unlist(SiteID)

  Latitude <- xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

  numSiteIDs <- length(SiteID)

  if (numSiteIDs != N) {
    SiteID <- SiteCode
  }

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

  #special case: some site doesn't have latitude specified
  numLatitudes <- length(Latitude)
  if (numLatitudes < N) {
    numValid <- N - numLatitudes + 1
    SiteName <- SiteName[numValid:N]
    SiteCode <- SiteCode[numValid:N]
    SiteID <- SiteID[numValid:N]
    Network <- Network[numValid:N]
    Longitude <- Longitude[numValid:N]
    Latitude <- Latitude[numValid:N]
    Elevation <- Elevation[numValid:N]
    State <- State[numValid:N]
    County <- County[numValid:N]
    Comments <- Comments[numValid:N]
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
}
