# Download White House visitor data
# Experimental script to download Socrata file.
# efg, 2014-08-28

BASEDIR <- "F:/FOIA/WhiteHouse/2016/2016-12-30/"                  ##### 1 of 1
setwd(BASEDIR)

sink("0-Fetch-Data-WH.txt", split=TRUE)     # Save times, md5sum, ...

library(downloader)  # download
library(tools)       # md5sum

Sys.time()           # Start time

# Script started working again 2015-08-28
URL <- "https://open.whitehouse.gov/api/views/p86s-ychb/rows.csv?accessType=DOWNLOAD"
csv.filename <- "White_House_Visitor_Records_Requests.csv"

# Unclear why % transfer is wrong, so silence transfer using "quiet":
download(URL, csv.filename, mode="wb", quiet=TRUE)

file.info(csv.filename)                     # metadata about files
md5sum(csv.filename)                        # Record MD5sums

Sys.time()           # End Time

sink()

