# Segment White House Visitor Data into three groups:
# 1. "Office Visitors" (tourists)  |  "waves visitoroffice"  (starting ~4/2016)
# 2. POTUS/FLOTUS/VPOTUS visitors
# 3. Staff visitors

# Updated 3 June 2013.
# 26 Apr 2013.  Added counts for last six months in yearly counts file.
# Earl F Glynn

#
# Review/change R statements below with comments:  ##### m of n
#

################################################################################
# Copyright (C) 2013, Franklin Center for Government and Public Integrity      #
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

BASEDIR <- "F:/FOIA/WhiteHouse/2016/2016-12-30/"       ##### change with new data (1 of 9)
setwd(BASEDIR)

# For now, don't split POTUS and VPOTUS since there may be overlap (March 2013)
POTUS <- "POTUS"
STAFF <- "Staff"

sink("2-WhiteHouse-Visitors-Groups.txt", split=TRUE)
time.1 <- Sys.time()

################################################################################
### Helper Functions

### table to data.frame
### (future:  use as.data.frame.matrix for multi-way tables)
table.to.data.frame <- function(Table, FieldName)
{
  df <- data.frame(Field=names(Table), counts=as.numeric(Table),
             stringsAsFactors=FALSE)
  colnames(df)[1] <- FieldName
  df
}

# Kludge to write frequency counts to file with label on field.
# (oddly R doesn't provide label over rownames normally).
write.field.counts <- function(Field, FieldName, Filename)
{
  counts <- table(Field)
  df <- table.to.data.frame(counts, FieldName)
  write.csv(df, file=Filename, row.names=FALSE)
}

# Write White House Visitor data with certain fields first
write.chronology.file <- function(Subset, Filename)
{
  first.fields <- c("visitee","APPT_START_DATE","LOCATION","Total_People",
                    "NAME","Description")
  fields <- colnames(Subset)
  fields <- c(first.fields, setdiff(fields, first.fields))

  Subset <- Subset[order(Subset$visitee,  Subset$APPT_START_DATE,
                         Subset$LOCATION, Subset$Total_People,
                         Subset$NAME),]
  write.csv(Subset[,fields], Filename, row.names=FALSE)

  invisible(Subset)
}

### Common footer for all graphics
graphics.footer <- function()
{                                            ##### change with new data (2 of 9)
  mtext("  Source:  www.whitehouse.gov/briefing-room/disclosures/visitor-records, 30 Dec 2016",
        BOTTOM<-1, adj=0, line=-1, cex=0.75, col="blue", outer=TRUE)
  mtext(expression(italic("Kansas Meadowlark  ")), BOTTOM, adj=1, line=-1, col="blue", outer=TRUE)
}

################################################################################
### Read data

# Input file is result of some cleanup by WhiteHouse-Visitors.R script:
d <- read.delim("CLEANED-ENHANCED/WhiteHouse-Visitor-Records.txt",
                as.is=TRUE)
dim(d)
#[1] 3181486      32  (Apr 2013)
#[1] 3022098      32  (Feb  2013)
#[1] 2827135      32  (Nov  2012)
#[1] 2362315      32  (June 2012)

# Records with blank or missing names should have been removed by earlier script.

###############################################################################
### Tourists ("Office Visitors")

TOURISTS <- "Tourists"
if (! file.exists(TOURISTS))
{
  dir.create(TOURISTS)
}

selectVisitors <- d$visitee == "office|visitors" |
                  d$visitee == "waves|visitorsoffice"  # Starting ~4/2016
tourists  <- d[selectVisitors,]
dim(tourists)
#[1] 2129416      32  (Apr  2013)
#[1] 2031337      32  (Feb  2013)
#[1] 1899324      32  (Nov  2012)
#[1] 1590791      32  (June 2012)

# For now we don't care about the tourists that visit unnamed White House
# staff members, but let's see if there are any "frequent" visitors.
# The expected results should show frequent common names.  Uncommon names with
# high frequency counts might be a bit curious.

write.field.counts(tourists$NAME,           "Name",
                   "Tourists/WhiteHouse-Tourists-Counts-Name.csv")

# Count how many times people with given name visit the White House.
counts <- table(tourists$NAME)
write.field.counts(counts,                  "VisitCounts",
                   "Tourists/WhiteHouse-Tourists-Counts-VisitCounts.csv")

# Why do some people visit the White House so often, but never visit someone
# named?
frequent <- table(tourists$NAME)
frequent <- table.to.data.frame(frequent, "NAME")
frequent <- frequent[frequent$counts > 4,]
frequent <- frequent[order(frequent$counts, frequent$NAME, decreasing=TRUE),]
write.csv(frequent, "Tourists/WhiteHouse-Tourists-Frequent.csv", row.names=FALSE)

# Other stats
write.field.counts(tourists$Total_People, "TotalPeople",
                   "Tourists/WhiteHouse-Tourists-Counts-TotalPeople.csv")
write.field.counts(tourists$LOCATION,       "Location",
                   "Tourists/WhiteHouse-Tourists-Counts-Location.csv")
write.field.counts(tourists$Type.of.Access, "TypeOfAccess",
                   "Tourists/WhiteHouse-Tourists-Counts-TypeOfAccess.csv")
write.field.counts(tourists$visitee,        "visitee",
                   "Tourists/WhiteHouse-Tourists-Counts-Visitee.csv")

# Description stats, including individual words
write.field.counts(tourists$Description,    "Description",
                   "Tourists/WhiteHouse-Tourists-Counts-Description.csv")

words <- sort(table(unlist(strsplit(tourists$Description, " "))),decreasing=TRUE)
words <- data.frame(Word=names(words), Count=as.integer(words),
                    stringsAsFactors=FALSE)
write.csv(words, "Tourists/WhiteHouse-Tourists-Counts-Description-Raw-Words.csv",
                 row.names=FALSE)

# Chronology files
write.chronology.file (tourists, "Tourists/WhiteHouse-Tourists-Chronology.csv")

table(substr(tourists$APPT_START_DATE,1,4))

#  2009   2010   2011   2012   (Feb 2013)
#182506 673579 643756 531496

#  2009   2010   2011   2012
#182505 673579 643786 399454   (Nov 2012)

                                      #####  Normally add year in April release.
for (year in 2009:2016)               #####  change with new data (3 of 9)
{
  SELECT <- (substr(tourists$APPT_START_DATE,1,4) == as.character(year))
  write.chronology.file (tourists[SELECT,],
         paste("Tourists/WhiteHouse-Tourists-Chronology-", year, ".csv", sep=""))
}

################################################################################
### Official Visitors:  POTUS visitors + White House Staff visitors
### (Excludes "tourists")

OFFICIAL <- "Official-Visitors"
if (! file.exists(OFFICIAL))
{
  dir.create(OFFICIAL)
}

official.visitors <- d[d$visitee != "office|visitors",]
dim(official.visitors)
#[1] 1052070      32   (Apr 2013)
#[1]  990761      32   (Feb 2013)
#[1]  927811      32
#[1]  771523      32   (June 2012)

### Assign group name "Staff" or "POTUS" to all visitors.

# Assign "Staff" to everyone for now
official.visitors$group <- STAFF

# If POTUS/FLOTUS/VPOTUS connection, then mark as "POTUS".
# Look for references in visitee and description fields.
potus.index <- unique(sort(c(grep("potus",  official.visitors$visitee),
                             grep("flotus", official.visitors$visitee),
                             grep("vpotus", official.visitors$visitee),
                             grep("potus",  official.visitors$Description),
                             grep("flotus", official.visitors$Description),
                             grep("vpotus", official.visitors$Description))))
official.visitors$group[potus.index] <- POTUS

table(official.visitors$group)

# POTUS  Staff  (Apr  2013)
#190225 861845

# POTUS  Staff  (Feb  2013)
#173766 816995

# POTUS  Staff  (Nov  2012)
#169308 758503

# POTUS  Staff  (June 2012)
#145261 626262

################################################################################
### Visit counts by visitor name by group: table and chart

process.visitor.counts <- function(Subset, Textname, Filename)
{
  # as.data.frame.matrix trick described here:
  # http://r.789695.n4.nabble.com/converting-a-table-to-a-dataframe-or-a-matrix-td856133.html
  By.NAME <- as.data.frame.matrix(table(Subset$NAME, Subset$group))
  # R kludge to get column name in first column in output file
  By.NAME <- data.frame(NAME=rownames(By.NAME),
                        POTUS=By.NAME$POTUS,
                        Staff=By.NAME$Staff, stringsAsFactors=FALSE)
  write.csv(By.NAME, paste(Filename,".csv", sep=""), row.names=FALSE)

  png( paste(Filename,".png", sep=""), width=600, height=600)
    plot(By.NAME$POTUS, By.NAME$Staff,
         main="White House  Visitors: Visits to Staff vs. POTUS",
         xlab="Number of Visits to POTUS/FLOTUS/VPOTUS",
         ylab="Number of Visits to White House Staff")
    mtext(paste(format(nrow(Subset), big.mark=",", scientific=FALSE),
                " non-tourist visitors to White House", Textname))
    grid()
    graphics.footer()
  dev.off()

  invisible(By.NAME)
}

### All Years                              ##### change with new data (4 of 9)
By.NAME <- process.visitor.counts(official.visitors, "(2009 through Sept 2016)",
                "Official-Visitors/WhiteHouse-POTUSandStaff-Counts-Name")

table( substr(official.visitors$APPT_START_DATE,1,4) )

#  2009   2010   2011   2012   2013   (Apr 2013)
# 95790 296534 308567 327519  23660

#  2009   2010   2011   2012          (Feb  2013)
# 95790 296534 308567 289870

#  2009   2010   2011   2012          (Nov  2012)
# 95782 296541 308575 226913

#  2009   2010   2011   2012          (June 2012)
# 95780 296836 308575  70332

# Add breakdown by year
By.YEAR <- as.data.frame.matrix(table(official.visitors$NAME,
                                      substr(official.visitors$APPT_START_DATE,1,4)))
By.YEAR <- data.frame(NAME=rownames(By.YEAR), By.YEAR,
                      stringsAsFactors=FALSE, row.names=NULL)
names(By.YEAR) <- gsub("^X", "Y", names(By.YEAR))

By.MONTH <- as.data.frame.matrix(table(official.visitors$NAME,
                                      substr(official.visitors$APPT_START_DATE,1,7)))
LAST.SIX.MONTHS <-  By.MONTH[,(ncol(By.MONTH)-5):ncol(By.MONTH)]
LAST.SIX.MONTHS <- data.frame(NAME=rownames(LAST.SIX.MONTHS), LAST.SIX.MONTHS,
                              stringsAsFactors=FALSE, row.names=NULL)
names(LAST.SIX.MONTHS) <- gsub("^X", "M", names(LAST.SIX.MONTHS))

By.NAME.YEAR <- merge(merge(By.NAME, By.YEAR), LAST.SIX.MONTHS)
write.csv(By.NAME.YEAR,
          "Official-Visitors/WhiteHouse-POTUSandStaff-Counts-Name-Year.csv", row.names=FALSE)

THIS.MONTH <- By.NAME.YEAR[By.NAME.YEAR[,ncol(By.NAME.YEAR)] > 0,]
MONTH <- names(THIS.MONTH)[ncol(THIS.MONTH)]
MONTH <- gsub("\\.", "-", MONTH)
write.csv(THIS.MONTH,
          paste0("Official-Visitors/WhiteHouse-POTUSandStaff-Counts-Name-",
                 MONTH, ".csv"),
          row.names=FALSE)


### Past Years:  2009-2016                   ##### Normally add year in April release.
for (year in 2009:2016)                      ##### change with new data (5 of 9)
{
  SELECT <- (substr(official.visitors$APPT_START_DATE,1,4) == as.character(year))
  year.subset <- official.visitors[SELECT,]
  process.visitor.counts(year.subset, paste("(", as.character(year), ")", sep=""),
    paste("Official-Visitors/WhiteHouse-POTUSandStaff-Counts-Name-", year, sep=""))
}

################################################################################
### Events

process.visitor.events <- function(Subset, TextName, FnPrefix, FnSuffix)
{
  # Aggregate creates unexpected structures when FUN returns more than a single
  # value.  So use three aggregte calls here.

  # Total_People gives number of people reported to be part of appointment.
  reported <- aggregate(Subset$Total_People,
                        by=list(Visitee = Subset$visitee,
                                Start   = Subset$APPT_START_DATE,
                                Location= Subset$LOCATION),
                        FUN="max")
  colnames(reported)[4] <- "People.Reported"

  # Let's see how many people we can find visiting the person at
  # specified time and place.
  observed <- aggregate(Subset$Total_People,
                        by=list(Visitee = Subset$visitee,
                                Start   = Subset$APPT_START_DATE,
                                Location= Subset$LOCATION),
                        FUN="length")
  colnames(observed)[4] <- "People.Observed"

  Description.Length <- nchar(Subset$LOCATION)
  # How many of the records for this event have a Description field defined?
  Descriptions  <- aggregate(Description.Length,
                             by=list(Visitee = Subset$visitee,
                                     Start   = Subset$APPT_START_DATE,
                                     Location= Subset$LOCATION),
                             FUN=function(x) {sum(x > 0)} )
  colnames(Descriptions)[4] <- "Descriptions"

  # Merges, additional fields
  Events <- merge(reported, observed)
  Events$Missing   <- Events$People.Reported - Events$People.Observed
  Events$pcMissing <- round(100 * Events$Missing / Events$People.Reported, 1)

  Events <- merge(Events, Descriptions)
  Events$pcDesc <- round(100 * Events$Descriptions/Events$People.Reported,1)
  write.csv(Events,
            paste("Official-Visitors/WhiteHouse-", FnPrefix,
                  "-Events-Visitors-Observed-vs-Reported-", FnSuffix, ".csv", sep=""),
            row.names=FALSE)

  png(paste("Official-Visitors/WhiteHouse-", FnPrefix,
            "-Events-Visitors-Observed-vs-Reported-", FnSuffix, ".png", sep=""),
      width=600, height=600)
  log.scale <- ifelse(max(Events$People.Reported, na.rm=TRUE) <= 250, "", "xy")
  plot(Events$People.Reported, Events$People.Observed, log=log.scale,
       main=paste("White House Events '", TextName, "'", sep=""),
       xlab=paste("People Reported", ifelse(log.scale == "xy","[log scale]","")),
       ylab=paste("People Observed", ifelse(log.scale == "xy","[log scale]","")))
  mtext("Observed vs. Reported Visitor Counts", col="blue")
  mtext(paste(format(nrow(Subset), big.mark=",", scientific=FALSE), "Visitors,",
              format(nrow(Events), big.mark=",", scientific=FALSE), "Events"),
        line=-2)
  graphics.footer()
  dev.off()

  invisible(Events)
}

################################################################################
### POTUS

potus.visitors <- official.visitors[official.visitors$group == POTUS,]

# Chronology files
write.chronology.file (potus.visitors, "Official-Visitors/WhiteHouse-POTUS-Chronology.csv")

table(substr(potus.visitors$APPT_START_DATE,1,4))

# 2009  2010  2011  2012  (Feb 2013)
#22433 57357 52287 41689

# 2009  2010  2011  2012  (Nov  2012)
#22431 57357 52291 37229

# 2009  2010  2011  2012  (June 2012)
#22429 57358 52282 13192

                                             #####  Normally add year in April release.
for (year in 2009:2016)                      ##### change with new data (6 of 9)
{
  SELECT <- (substr(potus.visitors$APPT_START_DATE,1,4) == as.character(year))
  write.chronology.file (potus.visitors[SELECT,],
         paste("Official-Visitors/WhiteHouse-POTUS-Chronology-", year, ".csv", sep=""))
}

# Events files
process.visitor.events(potus.visitors, "POTUS Visitors", POTUS, "All")

                                             ##### Normally add year in April release.
for (year in 2009:2016)                      ##### change with new data (7 of 9)
{
  SELECT <- (substr(potus.visitors$APPT_START_DATE,1,4) == as.character(year))
  year.subset <- potus.visitors[SELECT,]
  process.visitor.events(year.subset,
    paste("POTUS Visitors (", year, ")", sep=""),
    POTUS, year)
}

################################################################################
### White House Staff

staff.visitors <- official.visitors[official.visitors$group == STAFF,]

# Chronology files
write.chronology.file (staff.visitors, "Official-Visitors/WhiteHouse-Staff-Chronology.csv")

table(substr(staff.visitors$APPT_START_DATE,1,4))

# 2009   2010   2011   2012   (Nov  2012)
# 73351 239184 256284 189684

#  2009   2010   2011   2012  (June 2012)
# 73351 239478 256293  57140

                                             ##### Normally add year in April release.
for (year in 2009:2016)                      ##### change with new data (8 of 9)
{
  SELECT <- (substr(staff.visitors$APPT_START_DATE,1,4) == as.character(year))
  write.chronology.file (staff.visitors[SELECT,],
         paste("Official-Visitors/WhiteHouse-Staff-Chronology-",year,".csv", sep=""))
}

# Events files
process.visitor.events(staff.visitors, "White House Staff Visitors", STAFF, "All")

                                             #####  Normally add year in April release.
for (year in 2009:2016)                      ##### change with new data (9 of 9)
{
  SELECT <- (substr(staff.visitors$APPT_START_DATE,1,4) == as.character(year))
  year.subset <- staff.visitors[SELECT,]
  process.visitor.events(year.subset,
    paste("White House Staff Visitors (", year, ")", sep=""),
    STAFF, year)
}

time.2 <- Sys.time()
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

