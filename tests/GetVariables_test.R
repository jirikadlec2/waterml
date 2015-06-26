library(WaterML)

services <- GetServices()

getvariables_test <- data.frame(server=character(0), variables_download_time=numeric(0),
                                variables_download_status=character(0),
                                variables_parse_time=numeric(0), variables_parse_status=character(0),
                                num_variables=numeric(0), random_variable_code=character(0),
                                stringsAsFactors=FALSE)


#special case: add the new NWISDV service
urls <- services$url
urls[length(urls)+1] <- "http://qa-hiscentral20.cloudapp.net/water1flow_DV/cuahsi_1_1.asmx"

for (i in 1:length(urls)) {
  server <- urls[i]

  variables <- GetVariables(server)
  variables_download_time <- attr(variables, "download.time")
  variables_download_status <- attr(variables, "download.status")
  variables_parse_time <- attr(variables, "parse.time")
  variables_parse_status <- attr(variables, "parse.status")
  random_variable_code <- NA
  num_variables <- nrow(variables)
  if (num_variables > 0) {
    random_variable_code <- sample(variables$FullVariableCode, size=1)
  }
  new_row <- c(server, as.numeric(variables_download_time),
                      variables_download_status,
                      as.numeric(variables_parse_time),
                      variables_parse_status,
                      num_variables, random_variable_code)
  getvariables_test[nrow(getvariables_test)+1,] <- new_row
}
write.csv(getvariables_test, "tests/getvariables_test.csv")
