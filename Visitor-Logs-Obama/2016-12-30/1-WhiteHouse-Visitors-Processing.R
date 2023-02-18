# Quality control checks, standardization and descriptive statistics of
# White House Visitor Log data from
# http://www.whitehouse.gov/briefing-room/disclosures/visitor-records.
#
# Run on Windows 7 box with 64-bit R version 3.0.2.
#
# Review/change R statements below with comments:  ##### m of n
#
#  3 July 2012:  First look.
# 13 July 2012:  Added standardization of visitees.
# 28 Jan  2013:  Added 2nd round of duplicate removal to deal with
#                Oct. 2012 and Nov. 2012 duplicates of all records.
# 27 Sept 2013:  Process June 2013 data in Sept 2013 release.
#  8 Nov  2013:  Process July 2013 data in late Oct 2013 release on Nov 8.
# 29 Nov  2013:  Process Aug  2013 data in late Nov 2013 release.
# 30 Nov  2013:  Manually change TAB character in 3 records to "**"
# 27 Dec  2013:  Add code to remove TAB character in 3 records, specifically:
#                (	#U08267) to (#U08267)
# 31 Jan  2014:  Added LISTDIR to fix directory change.
#                Fix fix.WhiteHouse.dates to allow 2014 dates as valid.
# 29 Mar  2015:  Fix fix.WhiteHouse.dates to allow 2015-2016 dates as valid.
#
# Earl F Glynn

################################################################################
# Copyright (C) 2012-2014 Franklin Center for Government and Public Integrity  #
#                                                                              #
# This program is free software: you can redistribute it and/or modify         #
# it under the terms of the GNU General Public License as published by         #
# the Free Software Foundation.                                                #
#                                                                              #
# This program is distributed in the hope that it will be useful,              #
# but WITHOUT ANY WARRANTY; without even the implied warranty of               #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                         #
# For details:  http://www.gnu.org/licenses/gpl.html                           #
################################################################################

### Setup

LISTDIR <- "F:/FOIA/WhiteHouse/List/"   # location/visitee standardization lists

BASEDIR <- "F:/FOIA/WhiteHouse/2016/2016-12-30/"                           ##### 1 of 2
setwd(BASEDIR)

sink("1-WhiteHouse-Visitors-Processing.txt", split=TRUE)
time.1 <- Sys.time()

FILENAME        <- "White_House_Visitor_Records_Requests.csv"              ##### 2 of 2
FILENAME.FIXED  <- "##FIXEDDATA##.csv"    # Temporary file of "fixed" data

# Packages used
library(stringr)  # str_trim
library(chron)    # times

source("../../Common/WhiteHouse-SharedFunctions.R")

PROBLEMS <- "Problems"
if (! file.exists(PROBLEMS))
{
  dir.create(PROBLEMS)
}

################################################################################
### First look for duplicate records

cat("Treat file as strings.  Look for duplicates.\n")

# Read as vector of strings.  Check for duplicates.
s <- readLines(FILENAME)
length(s)
# Counts include header row
#[1] 5748187   (Oct  2016)
#[1] 4418604   (Mar  2015)
#[1] 3891622   (Aug  2014)
#[1] 3367184   (Sept 2013)
#[1] 3599501   (Jan  2013)


counts <- table(s)
table(counts)

#table(counts)
#counts
#      1       2       3       4       5       8      17
#5721454   13030     181      20       5       1       1

#counts       (Mar 2015)
#      1       2       3       4       5       8      17
#4396332   10877     138      16       3       1       1

#counts        (Aug 2014)
#      1       2       3       4       5       8
#3871980    9601     119      15       3       1

#counts        (Sept 2013)
#      1       2       3       4       5       8
#3349429    8672     112      13       3       1

#counts        (Feb 2013)
#      1       2       3       4       5       8
#3014733    7648     104      12       3       1

#counts        (Jan 2013)
#      1       2       3       4       5       6       8
#2876108  359685     103     900       4      13       2

# Sort and write duplicates to separate files for further study.
duplicates <- sort(counts[counts > 1], decreasing=TRUE)

duplicates <- data.frame(counts=as.numeric(duplicates),
                         record=names(duplicates), stringsAsFactors=FALSE)

# Write as tab-delimited file since original file has no tabs.
write.table(duplicates, "Problems/WhiteHouse-Visitors-Duplicate-Records.txt",
            quote=FALSE,row.names=FALSE,sep="\t")

# Create unique list of records
s <- unique(s)
cat("Unique records:\n")
length(s)
#[1] 5734692   (Oct  2016)
#[1] 4407368   (Mar  2015)
#[1] 3881719   (Aug  2014)
#[1] 3358230   (Sept 2013)
#[1] 3236815   (Jan  2013)

################################################################################
### Ad hoc data cleanup [28 Jan 2013]. Can possibly remove these checks
### if White House imposes some quality control on the release of data.
### Expand intial cleanup past removing duplicate records.

# Remove extra header(s), likely left over from raw WAVES files.
select <- grep("^NAMELAST,NAMEFIRST,NAMEMID|^namelast,namefirst,namemid", s)
stopifnot(select[1] == 1)  # First row must be header

if (length(select) > 1)
{
  #Remove any extra header row(s)
  cat(paste("Removing", length(select)-1, "extra header(s).  Line(s):", select[-1], "\n"))
  s <- s[-select[-1]]
  length(s)
}

# Remove records beginning or ending with 15 separators ("rule" found by chance).

# A record starting with 15 separators with no visitor name information was
# noticed in March 2012 release.  Deleted in past as missing a name.

# Records with last 15 separators are missing RELEASE_DATE (first noticed
# with duplicates of Sept. 2012 data with June 2012 appointments)

select <- grep("^,,,,,,,,,,,,,,,|,,,,,,,,,,,,,,,$", s)

if (length(select) > 0)
{
  cat(paste("Removing records beginning/ending with 15 separators.  Count:", length(select), "\n"))
  # Write as file of strings for inspection.  Include header.
  writeLines(s[c(1,select)], "Problems/WhiteHouse-Visitors-15-separators.txt")
  s <- s[-select]
  length(s)
  #[1] 5734691   (Oct  2016)
  #[1] 4407367   (Mar  2015)
  #[1] 3358229   (Sept 2013)
  #[1] 3289826   (June 2013)
  #[1] 3237648   (May  2013)
}

### Nov 30, 2013:  Remove TAB characters to prevent problems in parsing file
### WhiteHouse-Visitor-Records.txt created below.

tab.rows <- grep("\t", s)
cat("Rows with TABs to fix: ", length(tab.rows), "\n")
s <- gsub("\t", "**", s)

# Processing data via a textConnection and object s failed for an unknown reason.
# As workaround, write to temporary file for processing below.
writeLines(s, FILENAME.FIXED)

rm(duplicates, s)

################################################################################
### Verify delimiters, ability to parse

# This has never been a problem, but with the diversity in structure of WAVES
# files, this check is a good idea.

cat("Verify records all have same number of delimiters\n")

# Check delimiter count by row for consistency:  Data are comma delimited.
# Text qualifier '"' used whenever field has embedded comma (~3800 records)

# File contains ~3310 "#" characters, so override R's default comment.char
# for count.fields.

# Fields containing embedded commas use the text qualifier '"'.

field.counts <- count.fields(FILENAME.FIXED, quote='"', sep=",", comment.char="")
length(field.counts)
#[1] 5734691    (Oct  2016)
#[1] 4407367    (Mar  2015)
#[1] 3881718    (Aug  2014)
#[1] 3358229    (Sept 2013)
#[1] 3022500    (Feb  2013)
#[1] 3136181    (Jan  2013, after ad hoc removals)

count.table <- table(field.counts)
sum(count.table)

# Proof that all records parse to same number of tokens.
count.table

#field.counts
#     28
#5734691

#field.counts    (Aug 2014)
#     28
#3881718

#field.counts    (June 2013)
#     28
#3289826

#field.counts   (Feb 2013)
#     28
#3022500

#field.counts   (Jan 2013, after ad hoc removals)
#     28
#3136181

rm(field.counts)

################################################################################
### Read as CSV delimited data.

cat("Load data into data.frame.\n")

d <- read.csv(FILENAME.FIXED, as.is=TRUE, quote='"', comment.char="")
dim(d)

#[1] 5734690      28   (Oct  2016)
#[1] 3881717      28   (Aug  2014)
#[1] 3289825      28   (June 2013)
#[1] 3022499      28   (Feb  2013)
#[1] 3136180      28   (Jan  2013, after ad hoc removals)

# Since string vector s above included a header, the count of unique records
# is one less here.

unlink(FILENAME.FIXED)   # Temporary file no longer needed

################################################################################
# Cleanup data for easier name comparisons

d$NAMELAST  <- standardize_name(d$NAMELAST)
d$NAMEFIRST <- standardize_name(d$NAMEFIRST)
d$NAMEMID   <- standardize_name(d$NAMEMID)

d$visitee_namelast  <- standardize_name(d$visitee_namelast)
d$visitee_namefirst <- standardize_name(d$visitee_namefirst)

# Not sure these names are useful
d$CALLER_NAME_LAST  <- standardize_name(d$CALLER_NAME_LAST)
d$CALLER_NAME_FIRST <- standardize_name(d$CALLER_NAME_FIRST)

################################################################################
# Remove record(s) without first OR last names

select <- (nchar(d$NAMELAST) == 0)  |
          (nchar(d$NAMEFIRST) == 0) |
          (d$NAMELAST == "\\")      |  ## Add this problem record too
          (is.na(d$NAMELAST))       |  ## Don't bother if no last name
          (is.na(d$NAMEFIRST))      |  ## Don't bother if no first name
          (is.na(d$RELEASE_DATE))
cat("Records missing first or last names:\n")
sum(select)
#[1] 377       (Oct 2016)
#[1] 340       (May 2015)
#[1] 325       (Aug 2014)
#[1] 324       (May 2013)
#[1] 324       (Jan 2013)

write.table(d[select,], "Problems/WhiteHouse-Visitors-Missing-Names.txt",
            quote=FALSE,sep="\t", row.names=FALSE)

# Remove blank record(s)/missing names
d <- d[!select,]
dim(d)


################################################################################
# Introduce combined fields

# Combined name fields
d$NAME    <- paste(d$NAMELAST, d$NAMEFIRST, d$NAMEMID, sep="|")
d$visitee <- paste(d$visitee_namelast, d$visitee_namefirst, sep="|")
d$CALLER  <- paste(d$CALLER_NAME_LAST, d$CALLER_NAME_FIRST, sep="|")

################################################################################

# Standardizing spelling of visitor names does not seem prudent without
# additional information, but some standardization of the visitee may be
# safe, especially when compared to known White House staff members.

# For now let's only standardize these visitees:
# POTUS, FLOTUS, POTUS/FLOTUS and VPOTUS

fix.visitee <- function(v, file.list, standard.name)
{
  name.set <- read.csv(file.list, as.is=TRUE)
  v[v %in% name.set$visitee] <- standard.name
  v
}

d$VISITEE <- fix.visitee(d$VISITEE, paste0(LISTDIR, "visitee-POTUS.csv"),        "potus|")
d$VISITEE <- fix.visitee(d$VISITEE, paste0(LISTDIR, "visitee-FLOTUS.csv"),       "flotus|")
d$VISITEE <- fix.visitee(d$VISITEE, paste0(LISTDIR, "visitee-POTUS-FLOTUS.csv"), "potus/flotus|")
d$VISITEE <- fix.visitee(d$VISITEE, paste0(LISTDIR, "visitee-VPOTUS.csv"),       "vpotus|")

################################################################################

# Cleanup location fields
d$MEETING_LOC  <- standardize_location(d$MEETING_LOC)
d$MEETING_ROOM <- standardize_location(d$MEETING_ROOM)

# LOCATION is combined MEETING_LOC and MEETING ROOM
d$LOCATION <- paste(d$MEETING_LOC, d$MEETING_ROOM, sep="|")

# Standardize selected locations
# (Good source for info:
# http://www.msnbc.msn.com/id/30932789/ns/politics-white_house/t/inside-white-house-interactive/)

FixLocations <- function(L, file.list, standard.name)
{
  name.set <- read.csv(file.list, as.is=TRUE)
  L[L %in% name.set$location] <- standard.name
  L
}

d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-BowlingAlley.csv"), "oeob|bowling alley")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-EastExec.csv"),     "wh|east exec")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-EastWing.csv"),     "wh|east wing")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-ew100.csv"),        "wh|ew 100")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-ew206.csv"),        "wh|ew 206")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-ew212.csv"),        "wh|ew 212")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-Mess.csv"),         "wh|wh mess")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-oeob430abc.csv"),   "oeob|430 abc")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-oeob430bc.csv"),    "oeob|430 bc")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-OvalOffice.csv"),   "wh|oval office")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-Residence.csv"),    "wh|residence")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-Roosevelt.csv"),    "wh|roosevelt")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-RoseGarden.csv"),   "wh|rose garden")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-SituationRoom.csv"),"wh|situation room")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-SouthGrounds.csv"), "wh|south grounds")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-StateFloor.csv"),   "wh|state floor")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-WestExec.csv"),     "wh|west exec")
d$LOCATION <- fix.locations(d$LOCATION, paste0(LISTDIR, "location-WestWing.csv"),     "wh|west wing")

################################################################################
################################################################################
# Put all dates in standard format so sorting as character strings results in
# chronological order.

# Write problem dates to separate file for further study.
# Since problem dates all appear to be from 2009, don't bother with
# any special rules to attempt corrections.

converted.dates    <- fix.WhiteHouse.dates(d$APPT_MADE_DATE)
bad.dates <- table(d$APPT_MADE_DATE[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-APPT_MADE_DATE.csv")
d$APPT_MADE_DATE <- converted.dates

converted.dates    <- fix.WhiteHouse.dates(d$APPT_START_DATE)
bad.dates <- table(d$APPT_START_DATE[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-APPT_START_DATE.csv")
d$APPT_START_DATE  <- converted.dates

converted.dates    <- fix.WhiteHouse.dates(d$APPT_END_DATE)
bad.dates <- table(d$APPT_END_DATE[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-APPT_END_DATE.csv")
d$APPT_END_DATE    <- converted.dates

converted.dates    <- fix.WhiteHouse.dates(d$APPT_CANCEL_DATE)
bad.dates <- table(d$APPT_CANCEL_DATE[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-APPT_CANCEL_DATE.csv")
d$APPT_CANCEL_DATE <- converted.dates

converted.dates    <- fix.WhiteHouse.dates(d$LASTENTRYDATE)
bad.dates <- table(d$LastEntryDate[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-LastEntryDate.csv")
d$LastEntryDate    <- converted.dates

# In July 2014 the RELEASE_DATE format was modified to be
# something like "01/25/2013 08:00:00 AM +0000".  The new release times
# were either 07:00:00 AM or 08:00:00 AM.  To use the existing
# conversion functions, the last ":00 AM +0000" was stripped from
# each of the new date-time fields.
d$RELEASE_DATE <- substr(d$RELEASE_DATE, 1, 16)

################################################################################
################################################################################
### Special fix for April 26, 2013 release.
### The RELEASE_DATE for data should be 04/26/2013, NOT 04/26/2012.

#d$RELEASE_DATE <- gsub("04/26/2012", "04/26/2013", d$RELEASE_DATE)

### In 11/28/2014 release, 59316 records show a blank RELEASE_DATE,
### when the number had only been a handful before.  The 9257 records
### marked as released on 11/28/2014 show a stamp "11/28/2014 11:00:00 PM +0000"
### but many of the 59316 were released that day.

### Special KLUDGE fix for Nov 28, 2014 release.
#d$RELEASE_DATE[d$RELEASE_DATE == ""] <- "11/28/2014 08:00"
#> sum(d$RELEASE_DATE == "")
#[1] 59316

converted.dates    <- fix.WhiteHouse.dates(d$RELEASE_DATE)
bad.dates <- table(d$RELEASE_DATE[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-RELEASE_DATE.csv")
d$RELEASE_DATE     <- converted.dates

# Nov 2014
#> sum(is.na(d$RELEASE_DATE))
#[1] 59316

# For records released in 11/2014 with Appointments from 8/2014 but
# with missing release date:
SPECIAL.FIX <- is.na(d$RELEASE_DATE) & (substr(d$APPT_START_DATE,1,7) == "2014-08")

# 9257 marked at 11:00:00; let's mark the other 59314 at 10:00:00
# Let's use separate time in case this ever makes a difference.
d$RELEASE_DATE[SPECIAL.FIX] <-  "2014-11-28 10:00:00"

sum(is.na(d$RELEASE_DATE))

################################################################################

# Add Time of Arrival (TOA) and Time of Departure (TOD) to date/time checks
converted.dates    <- fix.WhiteHouse.dates(d$TOA)
bad.dates <- table(d$TOA[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-TOA.csv")
d$TOA              <- converted.dates

converted.dates    <- fix.WhiteHouse.dates(d$TOD)
bad.dates <- table(d$TOD[is.na(converted.dates)])
write.csv(bad.dates,"Problems/BAD-TOD.csv")
d$TOD              <- converted.dates

# Additional fix added 23 Aug 2012
# Cleanup APPT_START_DATE:
# Replace missing APPT_START_DATE with date from APPT_END_DATE.
missing.start.date <- is.na(d$APPT_START_DATE)
sum(missing.start.date)
#[1] 1281  (Oct 2016)
#[1] 7     (Feb 2013)
#[1] 7     (Jan 2013)

write.table(d[missing.start.date,], "Problems/WhiteHouse-Visitors-No-Start-Date.txt",
            quote=FALSE,sep="\t")

# Use end date with no time for missing start date. Good enough for most purposes.
d$APPT_START_DATE[missing.start.date] <-
  substr(d$APPT_END_DATE[missing.start.date],1,10)

# Trim description field
d$Description <- tolower(str_trim(d$Description))
d$Description <- gsub("/$", "", d$Description)
d$Description <- str_trim(d$Description)
d$Description <- gsub("\\.$", "", d$Description)
d$Description <- str_trim(d$Description)

# Multiple blanks to single blank
d$Description <- gsub(" +", " ", d$Description)

################################################################################
# December 2012 data:  For some reason after standardization of dates,
# all records from Oct. 2012 and Nov. 2012 releases were found to be
# duplicated.  Cause:  "12" vs "2012" years in dates in the raw recods.
# efg, 28 Jan 2013.

cat("Remove duplicates a second time, after field standardization\n")
dim(d)
#[1] 5734313      32    (Oct  2016)
#[1] 3881392      32    (Aug  2014)
#[1] 3289501      32    (June 2013)
#[1] 3022175      32    (Feb  2013)

d <- unique(d)
dim(d)
#[1] 5734061      32    (Oct  2016)
#[1] 3881294      32    (Aug  2014)
#[1] 3289414      32    (June 2013)
#[1] 3022099      32    (Feb  2013)

# After removing duplicates (again), let's write out cleanup files.

################################################################################
### Write cleaned up data with additional fields to tab-delimited file
### (since the original file contained no tabs)

CLEANED.ENHANCED.dir <- "CLEANED-ENHANCED"
if (! file.exists(CLEANED.ENHANCED.dir))
{
  dir.create(CLEANED.ENHANCED.dir)
}

# For now, sort by visitor name, appointment date
d <- d[order(d$NAMELAST,d$NAMEFIRST,d$NAMEMID,d$APPT_START_DATE),]
dim(d)
#[1] 5734061      32     #  (Oct  2016)
#[1] 3881294      32     #  (Aug  2014)
#[1] 3289414      32     #  (June 2013)
#[1] 3022099      32     #  (Feb  2013)
#[1] 2963213      32     #  (Jan  2013)

# Do NA RELEASE_DATEs still exist?
# Ad hoc cleanup above introduced in Jan 2013 should prevent this from
# happening, but leave code here in case it does.

problem.records <- is.na(d$RELEASE_DATE)
if (sum(problem.records) > 0)
{
  d.subset <- d[problem.records,]
  cat("Writing file for missing RELEASE_DATE.  Missing Count:", nrow(d.subset), "\n")
  flush.console()
  write.table(d.subset,
              paste(CLEANED.ENHANCED.dir, "/", "WhiteHouse-Visitor-Records-Missing-Release-Date",
                    ".txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE)

  # Remove from master list
  d <- d[!problem.records,]
  dim(d)
}

# Write cleaned data to disk
filename <- paste(CLEANED.ENHANCED.dir, "/", "WhiteHouse-Visitor-Records.txt", sep="")
write.table(d, filename, quote=FALSE, sep="\t", row.names=FALSE)

# Write separate files by release date to enable study of current data.
# Externally use md5/CRC checksums to verify if changes made in historical files.

release.date <- names(table(d$RELEASE_DATE))
for (i in 1:length(release.date))
{
  d.subset <- d[d$RELEASE_DATE == release.date[i],]
  cat(i, "Writing file for", release.date[i], nrow(d.subset), "\n")
  flush.console()
  write.table(d.subset,
              paste(CLEANED.ENHANCED.dir, "/", "WhiteHouse-Visitor-Records-",
                    substr(release.date[i],1,10), ".txt", sep=""),   # fix date Aug 2014
              quote=FALSE, sep="\t", row.names=FALSE)
}

################################################################################
### Create stats for global dataset and most recent release
COUNTS <- "Counts"  # Directory name for frequency count files

create.stats <- function(VERSION, d)
{
  ### Create COUNTS directory if it does not exist.
  COUNTS.dir <- paste(COUNTS, VERSION, sep="-")
  if (! file.exists(COUNTS.dir))
  {
    dir.create(COUNTS.dir)
  }

  ### Descriptive stats for each field

  field.summary <- NULL
  for (i in 1:length(colnames(d)))
  {
    column <- colnames(d)[i]
    field <- tolower(d[,i])

    counts <- table(field)

    counts <-  data.frame(as.table(counts))
    colnames(counts) <- c(column,"Count")

    # Sort in decreasing order by frequency count
    counts <- counts[order(counts[,2], counts[,1], decreasing=TRUE),]

    filename <- paste(COUNTS.dir, "/", sprintf("%02d",i),"-", column,
                      ".csv",sep="")
    write.csv(counts, file=filename, row.names=FALSE)
    cat(i, column, "\n")
    flush.console()

    zero.length.field <- (nchar(field) == 0)  # trim char strings?
    Missing <-  sum(zero.length.field)
    N <- length(field) - Missing
    NUnique <- length(unique(field))

    field.summary <- rbind(field.summary,
                           c(i, column,
                             as.vector(summary( nchar(field[!zero.length.field]) ) ),
                             N, Missing, NUnique) )

  }
  colnames(field.summary) <- c("No", "Field", "Min", "Q1", "Median", "Mean",
                               "Q3", "Max", "N", "Missing", "NUnique")
  filename <- paste("WhiteHouse-Visitor-Log-Field-Summary-", VERSION, ".csv", sep="")
  write.csv(field.summary, file=filename, row.names=FALSE)
}

create.stats("ALL",     d)
create.stats("RELEASE", d.subset)  # last one from section above

time.2 <- Sys.time()
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

