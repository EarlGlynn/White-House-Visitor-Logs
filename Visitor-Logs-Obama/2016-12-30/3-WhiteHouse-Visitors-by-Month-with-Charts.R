# Count visitors by visit month.
# 29 July 2013

# 29 March 2014.  Will need modifications for 2014 in April 2014.
# Earl F Glynn

################################################################################
# Copyright (C) 2014, Franklin Center for Government and Public Integrity      #
#                                                                              #
# This program is free software: you can redistribute it and/or modify         #
# it under the terms of the GNU General Public License as published by         #
# the Free Software Foundation.                                                #
################################################################################

library(RColorBrewer)

BASEDIR <- "F:/FOIA/WhiteHouse/2016/2016-12-30/"                  ##### 1 of 7
setwd(BASEDIR)

sink("3-WhiteHouse-Visitors-by-Month-with-Charts.txt", split=TRUE)
time.1 <- Sys.time()

CLEANED <- "CLEANED-ENHANCED"
POTUS   <- "POTUS"
STAFF   <- "Staff"
TOURIST <- "Tourist"

################################################################################
### Read data
visitors <- read.delim(paste0(CLEANED, "/WhiteHouse-Visitor-Records.txt"),
                  as.is=TRUE, colClasses="character")
dim(visitors)

################################################################################
# Assign groups based on visitee

visitors$group <- ""
visitors$group[visitors$visitee == "office|visitors"] <- TOURIST
visitors$group[visitors$visitee != "office|visitors"] <- STAFF
potus.index <- unique(sort(c(grep("potus",  visitors$visitee),
                             grep("flotus", visitors$visitee),
                             grep("vpotus", visitors$visitee))))
visitors$group[potus.index] <- POTUS

################################################################################
### Define APPT_MONTH

# Visit month: YYYY-MM
visitors$APPT_MONTH <- substr(visitors$APPT_START_DATE,1,7)
table(visitors$APPT_MONTH)

### crosstab
xtab <- as.data.frame.matrix(table(visitors$APPT_MONTH, visitors$group))

count <- data.frame(APPT_MONTH=row.names(xtab),
                    POTUS=xtab$POTUS,
                    Staff=xtab$Staff,
                    Tourist=xtab$Tourist,
                    stringsAsFactors=FALSE)

write.csv(count, "White-House-Visitors-By-Month-Table.csv", row.names=FALSE)

################################################################################
### Barplot from all visitor records

pdf("White-House-Visitors-By-Month-Chart.pdf", width=10, height=7.5)

plot.counts <- t(as.matrix(count[,2:4]))
colors <- brewer.pal(3, "Pastel1")

barplot(plot.counts,
        ylim=c(0, 130000), xlim=c(0,1.2*(nrow(count)+1)),
        yaxt="n",
        col=colors,
        legend.text=c("Visits to POTUS", "Visits to Staff", "Tourists"),
        args.legend=list(x=13, y=0.75*par("usr")[4], cex=0.75, bty="n"),
        main="White House Visitors During Obama Administration")
axis(2, at=20000*0:6, c("0", "20K", "40K", "60K", "80K", "100K", "120K"), las=2)
grid(nx=NA, ny=NULL, lwd=2)
mtext("Visit Counts by Month")
                                                                                                 ##### April only
abline(v=0.1 + 1.2*12*0:7, col="blue", lwd=2)                                                    ##### 2 of 7

visit.months <- c("Jan 2009", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2010", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2011", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2012", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2013", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2014", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2015", "", "", "April", "", "", "July", "", "", "Oct", "", "",
                  "Jan 2016", "", "", "April", "", "", "July", "", "", "Oct", "", "")            ##### 3 of 7


text(-0.5+1.2*(1:ncol(plot.counts)), par("usr")[3]-0.01*par("usr")[4],
     labels=visit.months, srt=45,
     cex=1, adj=1, xpd=TRUE)

                                                                                                 ##### April only
text(0.5+1.2*12*0:7, 0.95*par("usr")[4], 2009:2016,                                              ##### 4 of 7  (2 fixes)
     cex=1.5, adj=0, col="blue")

text(0.5,60000, paste0("The White House Voluntary\n",
                       "Disclosure Policy allowed\n",
                       "release of visit records\n",
                       "starting Sept. 15, 2009.\n\n",
                       "Judicial Watch filed a\n",
                       "lawsuit to force release of\n",
                       "all records back to Jan. 2009.\n",
                       "That case is still pending."),
     adj=0, cex=0.5)

mtext("  Source: www.whitehouse.gov/briefing-room/disclosures/visitor-records, 30 Dec 2016",      ##### 5 of 7
        BOTTOM<-1, adj=0.05, line=-1.5, cex=0.75, col="blue", outer=TRUE)
mtext(expression(italic("KansasMeadowlark.com  ")), BOTTOM, adj=0.95, line=-1.5, col="blue", outer=TRUE)

################################################################################
### Barplot from visitor records 2014-2016

plot.counts <- plot.counts[,-(1:(5*12))]   # delete years 2009-2013

barplot(plot.counts,
        ylim=c(0, 115000), xlim=c(0,1.2*(ncol(plot.counts)+1)),
        yaxt="n",
        col=colors,
        legend.text=c("Visits to POTUS", "Visits to Staff", "Tourists"),
        args.legend=list(x=0.65*par("usr")[2],
                         y=0.87*par("usr")[4],
                         bty="n"),
        main="White House Visitors During Obama Administration")
axis(2, at=20000*0:6, c("0", "20K", "40K", "60K", "80K", "100K", "120K"), las=2)
grid(nx=NA, ny=NULL, lwd=2)
mtext("Visit Counts by Month")
abline(v=0.1 + 1.2*12*0:2, col="blue", lwd=2)

visit.months <- c("Jan 2014", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "Jan 2015", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "Jan 2016", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep")                      ##### 6 of 7

text(-0.5+1.2*(1:ncol(plot.counts)), par("usr")[3]-0.01*par("usr")[4],
     labels=visit.months, srt=45,
     cex=1, adj=1, xpd=TRUE)

text(0.5+1.2*12*0:2, 0.95*par("usr")[4], 2014:2016,
     cex=1.5, adj=0, col="blue")

#text(14*1.2,65000,
#     paste0("Supposedly White House\n",
#            "tours ended March 9, 2013,\n",
#            "but the chart shows tours\n",
#            "did not stop until April."),
#     adj=0, cex=0.75)

mtext("  Source: www.whitehouse.gov/briefing-room/disclosures/visitor-records, 30 Dec 2016",     ##### 7 of 7
        BOTTOM<-1, adj=0.05, line=-1.5, cex=0.75, col="blue", outer=TRUE)
mtext(expression(italic("KansasMeadowlark.com  ")), BOTTOM, adj=0.95, line=-1.5, col="blue", outer=TRUE)
dev.off()

################################################################################
### Cross tab of visit date vs release date

table(visitors$APPT_MONTH)
table(visitors$RELEASE_DATE)
xtab <- as.data.frame.matrix(table(visitors$APPT_MONTH, visitors$RELEASE_DATE))

s <- NULL

s <- c(s, "Visit month summaries")
for (i in 1:nrow(xtab))
{
  row.stats <- xtab[i,]
  indices <- row.stats > 0
  releases <- paste0(names(row.stats)[indices], " ", row.stats[indices], collapse="; " )
  s <- c(s, paste0(row.names(xtab)[i], ": ", releases, collapse=""))
}

s <- c(s, "Release month summaries")
for (i in 1:ncol(xtab))
{
  col.stats <- xtab[,i]
  indices <- col.stats > 0
  visit.months <- paste0(row.names(xtab)[indices], " ", col.stats[indices], collapse="; " )
  s <- c(s, paste0(names(xtab)[i], ": ", visit.months, collapse=""))
}

writeLines(s, "White-House-Visitors-by-Release-and-Visit.txt")

time.2 <- Sys.time()
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()


