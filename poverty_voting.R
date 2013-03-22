library(ggplot2)
library(reshape2)
setwd("/home/simon/R_stuff/election_poverty")
cp <- read.csv("child_poverty_by_constituency.csv", header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
head(cp)
names(cp) <- c("constituency", "n.children", "pc.in.poverty")
head(cp)
res <- read.csv("GE2010-results.CSV", header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
head(res)
res <- subset(res, select=c(Constituency.Name, Region, Electorate, Votes, Con, Lab, LD, UKIP, BNP)) # remove most parties
levels(factor(res$Region))
# res <- subset(res, Region!="Northern Ireland" & Region != "Scotland" & Region != "Wales") # Select England only
levels(res$Region)
unique(res$Constituency.Name[res$Region==""]) # Some constituencies do not have regions assigned. Not sure why. 
length(res$Constituency.Name[res$Region==""]) # only one row without a region?
head(res)
res[res$Region=="",] # oh, good a helpful total row. 
res <- subset(res, Constituency.Name!="" & Region != "" & Votes != 29687604) # drop the total row

# merge
length(cp$constituency)
mdf <- merge(cp, res, by.x = "constituency", by.y = "Constituency.Name", all.x = TRUE)
head(mdf)
length(mdf$constituency)
mdf$constituency[is.na(mdf$Region) == TRUE]
mdf$constituency[is.na(mdf$Votes) == TRUE]
mdf$turnout <- round((mdf$Votes/mdf$Electorate)*100,2)
range(mdf$turnout, na.rm = TRUE)
mdf$pc.con <- round((mdf$Con/mdf$Votes)*100,2)
range(mdf$pc.con, na.rm = TRUE)
mdf$pc.lab <- round((mdf$Lab/mdf$Votes)*100,2)
range(mdf$pc.lab, na.rm = TRUE)
mdf$pc.ukip <- round((mdf$UKIP/mdf$Votes)*100,2)
mdf$pc.bnp <- round((mdf$BNP/mdf$Votes)*100,2)
mdf$pc.ld <- round((mdf$LD/mdf$Votes)*100,2)

# To plot all of these together need to reshape
melted <- melt(mdf, id=c("constituency","pc.in.poverty"))
head(melted)
levels(factor(melted$variable))
correlates<-subset(melted, variable == "turnout" |
                     variable == "pc.con" | variable == "pc.lab" | variable == "pc.ukip" |
                     variable == "pc.bnp" | variable == "pc.ld")
correlates$variable<-as.character(correlates$variable)
levels(factor(correlates$variable))
correlates$variable[correlates$variable == "pc.con"]<-"Conservative"
correlates$variable[correlates$variable == "pc.lab"]<-"Labour"
correlates$variable[correlates$variable == "pc.ukip"]<-"UKIP"
correlates$variable[correlates$variable == "pc.bnp"]<-"BNP"
correlates$variable[correlates$variable == "pc.ld"]<-"Liberal Democrats"
correlates$value<-as.numeric(correlates$value)
fig <- ggplot(correlates, aes(x = pc.in.poverty, y = value, group = variable)) + geom_point() + facet_wrap(
  ~variable)
fig <- fig + ggtitle("Voting trends by proportion of constituency children in \n poverty, UK general election 2010")
fig <- fig + ylab("Per cent") + xlab("Per cent children in poverty")
fig
ggsave(filename="votes_by_poverty.png", width = 5.5, height = 5)