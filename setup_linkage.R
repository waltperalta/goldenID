#########################################################################
# EM ALGORITHM SCRIPT
#########################################################################

# Author: Walter Peralta
# License: GPL

# Time difference of 3.073691 mins

####################################
## SETTINGS
####################################
#setwd("...")
start_time <- Sys.time()

packages <- c("RecordLinkage", "dplyr", "odbc")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

library(RecordLinkage)
library(odbc)
library(goldenID)

# ESTABLISH CONNECTION
  con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "mandc-sql-01",
                 Database = "SEE_REP",
                 Trusted_Connection = "True")


  query <- readline(prompt = "SQL Query: ")

  # OSC: SELECT memno,email,phone,fname,lname,zip,country,yyyy,mm,dd,address FROM
  # WP_goldenid_linkage_OSC_28022019

  # IDM: SELECT memno,email,phone,fname,lname,zip,country,yyyy,mm,dd,address FROM
  # WP_goldenid_linkage_IDM_28022019


####################################
## CLEANSING
####################################
### FIRST DATASET
output <- cleanse(dataset)
goldenID <- as.integer(NA)
memno <- dataset$memno

# FORMAT
output <- cbind(goldenID, memno, output)
output$goldenID <- as.integer(output$goldenID)
output$memno <- as.character(output$memno)
output$phone <- as.numeric(output$phone)
#output$mobile <- as.numeric(output$mobile)
output$fname <- as.character(output$fname)
output$lname <- as.character(output$lname)
output$zip <- as.character(output$zip)
output$country <- as.character(output$country)
output$address <- as.character(output$address)

# PARSING
# output$domain <- gsub(".*@|\\..*", "", output$email)
# output$ename <- sub("@.*", "", output$email)
output$finitial <- substring(output$fname,1,3)
output$sinitial <- substring(output$lname,1,3)


####################################
## DEDUPE
####################################
colnames(output)
output_deduped <- linkage(output, output$email, exclude = c(1:2), blockfld = c(10:11))

time <- Sys.time() - start_time

print(Sys.time() - start_time)


