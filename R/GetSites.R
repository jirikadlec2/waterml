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
  sites_url <- paste(server, "/GetSitesObject", sep="")
  doc <- xmlRoot(xmlTreeParse(sites_url, getDTD=FALSE, useInternalNodes = TRUE))
  N <- xmlSize(doc) - 1 #because first element is queryInfo

  df <- data.frame(SiteName=rep("",N), SiteCode=rep("",N), Latitude=rep(NA,N),
                   Longitude=rep(NA,N), Elevation=rep(NA,N),State=rep("",N),
                   County=rep("",N), Comments=rep("",N), stringsAsFactors=FALSE)

  for(i in 1:N){

    siteInfo <- doc[[i+1]][[1]]
    siteList <- xmlToList(siteInfo)
    siteName <- siteList$siteName
    siteCode <- siteList$siteCode$text
    latitude <- as.numeric(siteList$geoLocation$geogLocation$latitude)
    longitude <- as.numeric(siteList$geoLocation$geogLocation$longitude)
    elevation <- as.numeric(siteList$elevation_m)
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
    df$Latitude[i] <- latitude
    df$Longitude[i] <- longitude
    df$Elevation[i] <- elevation
    df$Comments[i] <- comments
    df$State[i] <- state
    df$County[i] <- county
  }
  return(df)
}
