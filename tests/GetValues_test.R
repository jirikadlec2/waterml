library(WaterML)

##############################################################################################
# TEST the performance of GetValues for all HydroServers from HIS Central                    #
##############################################################################################

values_result <- read.csv("tests/getsiteinfo_test.csv", stringsAsFactors=FALSE)

#allocate new rows for values
N <- nrow(values_result)
values_result$values_download_time <- rep(NA, N)
values_result$values_download_status <- rep(NA, N)
values_result$values_parse_time <- rep(NA, N)
values_result$values_parse_status <- rep(NA, N)
values_result$num_values <- rep(NA, N)

for (i in 1:N) {
  server <- values_result$server[i]

  #skip servers with GetSiteInfo error
  if(is.na(values_result$siteinfo_parse_status[i])) {
    next
  }
  if(values_result$siteinfo_parse_status[i] != "OK") {
    next
  }
  site_code <- values_result$random_site_code[i]
  variable_code <- values_result$random_variable[i]
  method_code <- values_result$random_method[i]
  source_code <- values_result$random_source[i]
  qc_code <- values_result$random_qc[i]
  start_date <- values_result$start_date[i]
  end_date <- values_result$end_date[i]

  values <- GetValues(server, site_code, variable_code, start_date, end_date,
                      methodID=method_code, sourceID=source_code, qcID=qc_code)

  #check values status
  values_result$values_download_time[i] <- attr(values, "download.time")
  values_result$values_download_status[i] <- attr(values, "download.status")
  values_result$values_parse_time[i] <- attr(values, "parse.time")
  values_result$values_parse_status[i] <- attr(values, "parse.status")
  num_values <- nrow(values)
  values_result$num_values[i] <- num_values
}
write.csv(siteinfo_result, "tests/getvalues_test.csv")
