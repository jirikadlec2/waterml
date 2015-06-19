#' HISCentral_GetSites
#'
#' This function gets the table of sites from the HIS Central catalog
#'
#' @import XML
#' @import httr
#' @param west The west longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param south The south latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @param east The east longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param north The north latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @param ServiceID (optional): The ID of the service on HIS Central. To get the service ID,
#'  use the id column in the output of the GetServices() function.
#' @param Keyword (optional): The concept keyword (common name of variable) for
#'  searching the sites on HIS Central. Examples include Temperature, Precipitation, Snow Depth,... If the Keyword is not
#'  specified then sites with any variable will be returned.
#' @param IncludeServerDetails=TRUE (optional): If set to TRUE, then the output will
#' include the servCode and servURL for each site. If set to FALSE, then we assume
#' that all sites are from the same server and the servURL and servCode are not included.
#' @return a data.frame of sites. The data.frame has the following columns:
#' \itemize{
#' \item SiteName: The name of the site
#' \item SiteCode: A short unique code of the site
#' \item FullSiteCode: The complete unique code of the site in the format NETWORK:CODE.
#'               Use this value in the GetSiteInfo and GetValues functions
#' \item Latitude:  The WGS84 latitude in decimal degrees
#' \item Longitude: The WGS84 longitude in decimal degrees
#' \item ServCode: The code of the service in HIS Central. Same as the networkName in
#'                  the output from GetServices() function.
#'                  This column is only shown if IncludeServerDetails is TRUE.
#' \item ServURL:   The URL of the web service for this site as registered in HIS Central.
#'                  This column is only shown if IncludeServerDetails is TRUE.
#' }
#' @keywords waterml
#' @export
#' @examples
#' #Getting all sites from Czechia (central Europe) from the GLDAS web service
#' sites <- HISCentral_GetSites(west=12.0, south=48.0, east=18.0, north=51.0, ServiceID=262)

HISCentral_GetSites <- function(west=-180, south=-90, east=180, north=90,
                                ServiceID=NULL, IncludeServerDetails=TRUE) {

  catalog = "http://hiscentral.cuahsi.org/webservices/hiscentral.asmx/GetSitesInBox2"

  #create the URL
  servID = ServiceID
  if (is.null(ServiceID)) {
    servID=""
  }
  if (is.null(Keyword)) {
    Keyword=""
  }
  queryParameters <- list(xmin=west, ymin=south, xmax=east, ymax=north,
                          networkIDs=servID, conceptKeyword=Keyword)
  url <- paste(catalog, "?", "&xmin=", west, "&ymin=", south, "&xmax=", east,
               "&ymax=", north, "networkIDs=", servID, "&conceptKeyword=", Keyword,
               sep="")

  print(paste("searching sites from:", url, "..."))

  download.time <- system.time(
    tryCatch({
      downloaded <- FALSE
      response <- GET(catalog, query=queryParameters)
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

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  print("reading sites XML data...")
  doc <- tryCatch({
    content(response)
  }, warning = function(w) {
    print("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }, error = function(e) {
    print("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }
  )
  if (is.null(doc)) {
    return(NULL)
  }

  # specify the namespace information for HIS Central
  ns <- c(xsd="http://www.w3.org/2001/XMLSchema",
          xsi="http://www.w3.org/2001/XMLSchema-instance",
          sr="http://hiscentral.cuahsi.org/20100205/")

  # extract the data columns with XPath
  SiteName = xpathSApply(doc, "//sr:SiteName", xmlValue, namespaces=ns)
  N <- length(SiteName)

  FullSiteCode = xpathSApply(doc, "//sr:SiteCode", xmlValue, namespaces=ns)
  SiteCode = sub(".*:", "", FullSiteCode)

  Latitude <- xpathSApply(doc, "//sr:Latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:Longitude", xmlValue, namespaces=ns)

  ServCode <- xpathSApply(doc, "//sr:servCode", xmlValue, namespaces=ns)
  ServURL <- xpathSApply(doc, "//sr:servURL", xmlValue, namespaces=ns)

  df <- data.frame(
    SiteName = SiteName,
    SiteCode = SiteCode,
    FullSiteCode = FullSiteCode,
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    ServCode = ServCode,
    ServURL = ServURL,
    stringsAsFactors = TRUE)

  return(df)
}
