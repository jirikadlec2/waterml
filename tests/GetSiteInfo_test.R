library(WaterML)

##############################################################################################
# TEST the performance of GetSiteInfo for all HydroServers from HIS Central                  #
##############################################################################################

siteinfo_result <- read.csv("tests/getsites_test.csv", stringsAsFactors=FALSE)

#allocate new rows for siteinfo
N <- nrow(siteinfo_result)
siteinfo_result$siteinfo_download_time <- rep(NA, N)
siteinfo_result$siteinfo_download_status <- rep(NA,N)
siteinfo_result$siteinfo_parse_time <- rep(NA, N)
siteinfo_result$siteinfo_parse_status <- rep(NA, N)
siteinfo_result$num_series <- rep(NA, N)
siteinfo_result$random_variable <- rep(NA, N)
siteinfo_result$random_method <- rep(NA, N)
siteinfo_result$random_source <- rep(NA, N)
siteinfo_result$random_qc <- rep(NA, N)
siteinfo_result$start_date <- rep(NA, N)
siteinfo_result$end_date <- rep(NA, N)
siteinfo_result$value_count <- rep(NA, N)

for (i in 1:N) {
  server <- siteinfo_result$server[i]

  #skip servers with GetSites error
  if(siteinfo_result$sites_parse_status != "OK") {
    next
  }
  site_code <- siteinfo_result$random_site_code[i]
  siteinfo <- GetSiteInfo(server, siteCode = site_code)
  #check siteinfo status
  siteinfo_result$siteinfo_download_time[i] <- attr(siteinfo, "download.time")
  siteinfo_result$siteinfo_download_status[i] <- attr(siteinfo, "download.status")
  siteinfo_result$siteinfo_parse_time[i] <- attr(siteinfo, "parse.time")
  siteinfo_result$siteinfo_parse_status[i] <- attr(siteinfo, "parse.status")
  num_series <- nrow(siteinfo)
  siteinfo_result$num_series[i] <- num_series
  #chose a random series with variable, method, source, qc
  if (num_series > 0) {
    random_row <- sample(num_series, size=1)
    random_variable <- siteinfo$FullVariableCode[random_row]
    random_method <- siteinfo$methodID[random_row]
    random_source <- siteinfo$sourceID[random_row]
    random_qc <- siteinfo$qualityControlLevelID[random_row]
    start_date <- siteinfo$beginDateTime[random_row]
    end_date <- siteinfo$endDateTime[random_row]
    value_count <- siteinfo$valueCount[random_row]

    siteinfo_result$random_variable[i] <- random_variable
    siteinfo_result$random_method[i] <- random_method
    siteinfo_result$random_source[i] <- random_source
    siteinfo_result$random_qc[i] <- random_qc
    siteinfo_result$start_date[i] <- strftime(as.POSIXct(start_date), "%Y-%m-%dT%H:%M:%S")
    siteinfo_result$end_date[i] <- strftime(as.POSIXct(end_date), "%Y-%m-%dT%H:%M:%S")
    siteinfo_result$value_count[i] <- value_count
  }
}
write.csv(test_result, "tests/getsiteinfo_test.csv")
