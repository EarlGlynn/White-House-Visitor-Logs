# White House Visitors:  Whom did they go to see and where?
#
# efg, 24 Aug 2012.  Updated March 5, 2013.
# Earl F Glynn
# Franklin Center for Government and Public Integrity
#
# Run this script using output files from the WhiteHouse-Vistors.R script:
#   White_House_Visitor_Records_Requests-CLEANED-ENHANCED.txt

# All with visits counts < 10 were removed from COUNTS/29-NAME.csv.

################################################################################
### Setup

BASEDIR <- "F:/FOIA/WhiteHouse/2016/2016-09-30/"          ##### 1 of 1
setwd(BASEDIR)

sink("4b-Frequent-Visitor-Connections.txt")

FILENAME <- "CLEANED-ENHANCED/WhiteHouse-Visitor-Records.txt"

CONNECTIONS <- "Connections"
if (! file.exists(CONNECTIONS))
{
  dir.create(CONNECTIONS)
}

################################################################################

# Speed up searches by only looking through "Official Visitors" to White House
# and ignoring the huge number of tourist records.

# All visitors
d <- read.delim(FILENAME, as.is=TRUE, quote="")
nrow(d)

# Ignore "tourists" since it's unknown whom they visitied.
d <- d[d$visitee != "office|visitors",]
nrow(d)

# Limit to the ~13,500 most frequent White House visitors -- those with 10 or more visits
v <- read.csv("COUNTS-ALL/29-NAME.csv", as.is=TRUE)
nrow(v)
v <- v[v$Count >= 10,]
nrow(v)

################################################################################
### Visitor (NAME) - Visitee Connections

connections1 <- NULL
connections2 <- NULL

for (i in 1:nrow(v))
{
  s <- d[d$NAME == v$NAME[i],]

  # A frequent visitor to "office|visitors" will not have any hits here.  Ignore.
  if (nrow(s) > 0)
  {
    # Visitor (NAME) - Visitee Connections
    counts1 <- table(s$visitee)

    df1 <- data.frame(Visitor=v$NAME[i],
                     Visitee=names(counts1),
                     Counts=as.numeric(counts1),
                     stringsAsFactors=FALSE)
    connections1 <- rbind(connections1, df1)

    # Visitor - Location/Room Connections
    Location.Room <- paste(s$MEETING_LOC, s$MEETING_ROOM, sep="|")
    counts2 <- table(Location.Room)

    df2 <- data.frame(Visitor=v$NAME[i],
                      Location=names(counts2),
                      Counts=as.numeric(counts2),
                      stringsAsFactors=FALSE)
    connections2 <- rbind(connections2, df2)

    cat(i, v$NAME[i], nrow(df1), nrow(connections1),
                      nrow(df2), nrow(connections2), "\n")

    flush.console()
  }
}

connections1 <- connections1[order(connections1$Visitor, connections1$Visitee),]
write.csv(connections1, "Connections/Frequent-Visitor-Visitee-Connections.csv",  row.names=FALSE)

connections2 <- connections2[order(connections2$Visitor, connections2$Location),]
write.csv(connections2, "Connections/Frequent-Visitor-Location-Connections.csv", row.names=FALSE)

sink()
