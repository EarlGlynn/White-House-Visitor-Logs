# Besides the President (POTUS), First Lady (FLOTUS) and
# Vice President (VPOTUS), who received frequent White House visitors
# and in what location?

# efg, 24 Aug 2012.  Updated March 5, 2013.   Update 2019-11-16.
# Earl F Glynn
# Franklin Center for Government and Public Integrity
#
# Run this script with the output file from the WhiteHouse-Vistors.R script:
#   White_House_Visitor_Records_Requests-CLEANED-ENHANCED.txt

# All with visits counts < 10 were removed from COUNTS/30-visitee.csv.

################################################################################
### Setup


BASEDIR <- "I:/Government-Federal/WhiteHouse/2016/2016-12-30/"          ##### 1 of 1
setwd(BASEDIR)

sink("4a-Frequent-Visitee-Connections.txt")

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

# Limit to the White House vistees receiving 10 or more visitors,
# whether or not visitor is frequent but excluding POTUS/FLOTUS/VPOTUS.

v <- read.csv("COUNTS-ALL/30-visitee.csv", as.is=TRUE)
nrow(v)
#[1] 10265 (Nov 2012)


#v[1:6,]
# office|visitors 1899324
# potus|  145415
# lierman|kyle    12024
# |       10971              # Explore as separate group?
# flotus| 10804
# hetzel|office   10669      # Likely Tess Hetzel, EW Visitor's Office

# Delete first five rows
v <- v[-1:-6,]
nrow(v)

# Now restrict to visitees with 10 or more visitors
v <- v[v$Count >= 10,]
nrow(v)
#[1] 4411  (July 2012)


################################################################################
### Visitee - Visitor Connections
### Visitee - Location/Room Connections

connections1 <- NULL
connections2 <- NULL

for (i in 1:nrow(v))
{
  matches <- (d$visitee == v$visitee[i])
  if (sum(matches) == 0)
  {
    cat(i, v$visitee[i], "NO MATCHES *****", "\n")
  } else {
    s <- d[matches,]

    counts <- table(s$NAME)
    df1 <- data.frame(Visitee=v$visitee[i],
                     Visitor=names(counts),
                     Counts=as.numeric(counts),
                     stringsAsFactors=FALSE)
    connections1 <- rbind(connections1, df1)

    Location.Room <- paste(s$MEETING_LOC, s$MEETING_ROOM, sep="|")
    counts <- table(Location.Room)
    df2 <- data.frame(Visitee=v$visitee[i],
                     Location=names(counts),
                     Counts=as.numeric(counts),
                     stringsAsFactors=FALSE)
    connections2 <- rbind(connections2, df2)

    cat(i, v$visitee[i], nrow(df1), nrow(connections1), nrow(df2), nrow(connections2), "\n")
    flush.console()
  }
}

connections1 <- connections1[order(connections1$Visitee, connections1$Visitor),]
write.csv(connections1, "Connections/Frequent-Visitee-Visitor-Connections.csv",  row.names=FALSE)

connections2 <- connections2[order(connections2$Visitee, connections2$Location),]
write.csv(connections2, "Connections/Frequent-Visitee-Location-Connections.csv", row.names=FALSE)

sink()
