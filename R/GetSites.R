#' GetSites
#'
#' This function gets the table of sites from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx
#' @keywords waterml
#' @export
#' @examples
#' GetSites("http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx")

GetSites <- function(server) {
  #remove everything after .asmx
  m <- regexpr(".asmx", server)
  base.url <- substr(server, 0, m+nchar(".asmx")-1)
  sites.url <- paste(base.url, "/GetSitesObject?site=&authToken=", sep="")

  print("fetching sites from server...")
  downloaded = FALSE
  download <- tryCatch({
    doc <- xmlRoot(xmlTreeParse(sites.url, getDTD=FALSE, useInternalNodes = TRUE))
    downloaded = TRUE
  }, error = function(err) {
    print(paste("error fetching sites:", err))
    doc <- NULL
  })
  if (!downloaded){
    return(NULL)
  }

  N <- xmlSize(doc) - 1 #because first element is queryInfo

  df <- data.frame(SiteName=rep("",N), SiteCode=rep("",N), FullSiteCode=rep("",N),
                   Latitude=rep(NA,N), Longitude=rep(NA,N), Elevation=rep(NA,N),
                   State=rep("",N), County=rep("",N), Comments=rep("",N),
                   stringsAsFactors=FALSE)

  for(i in 1:N){

    siteInfo <- doc[[i+1]][[1]]
    siteList <- xmlToList(siteInfo)
    siteName <- siteList$siteName
    siteCode <- siteList$siteCode$text
    fullSiteCode <- paste(siteList$siteCode$.attrs["network"], siteCode, sep=":")
    latitude <- as.numeric(siteList$geoLocation$geogLocation$latitude)
    longitude <- as.numeric(siteList$geoLocation$geogLocation$longitude)

    elevation <- NA
    if (!is.null(siteList$elevation_m)) {
      elevation <- as.numeric(siteList$elevation_m)
    }

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
      if (attr == 'State') {
        state <- xmlValue(element)
      }
      if (attr == 'County') {
        county <- xmlValue(element)
      }
    }
    df$SiteName[i] <- siteName
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
