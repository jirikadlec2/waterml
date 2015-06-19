#' GetServices
#'
#' This function gets the table of web services from the HIS Central catalog
#'
#' @import XML
#' @keywords waterml
#' @export
#' @examples
#' GetServices()

GetServices <- function() {
  catalog = "http://hiscentral.cuahsi.org/webservices/hiscentral.asmx/GetWaterOneFlowServiceInfo"
  doc <- xmlRoot(xmlTreeParse(catalog, getDTD=FALSE, useInternalNodes = TRUE))
  N <- xmlSize(doc)

  colnames <- c("url","title","descriptionURL","organization","citation","abstract",
                "valuecount","variablecount","sitecount","id","networkName",
                "minLon","minLat","maxLon","maxLat")

  m <- matrix(ncol=15, nrow=N, dimnames=list(NULL, colnames))
  df <- as.data.frame(m)

  for(i in 1:N){
    element <- xmlToList(doc[[i]])
    #we replace NULL values with NA
    e <- lapply(element, function(x) {ifelse(is.null(x), NA, x)})
    df$url[i] <- e$servURL
    df$title[i] <- e$Title
    df$descriptionURL[i] <- e$ServiceDescriptionURL
    df$organization[i] <- e$organization
    df$citation[i] <- e$citation
    df$abstract[i] <- e$aabstract
    df$valuecount[i] <- e$valuecount
    df$sitecount[i] <- e$sitecount
    df$id[i] <- e$ServiceID
    df$networkName[i] <- e$NetworkName
    df$minLon[i] <- e$minx
    df$minLat[i] <- e$miny
    df$maxLat[i] <- e$maxx
    df$maxLon[i] <- e$maxy
  }
  return(df)
}
