# setwd("C:/Users/Andrew/Dropbox/Baseball/InsideEdgeDefense/Shiny")
library(ggplot2)
full.dat=read.csv("InsideEdgeModelResults.csv",stringsAsFactors=FALSE,row.names=1)
names(full.dat)[c(2,11)] <- c("Innings","Position")

positions=c("All","P","C","1B","2B","3B","SS","LF","CF","RF")
teams=c("All",sort(unique(full.dat$Team)))

# Took the median of the distribution of opportunities per inning for each player
plays.per.inning=data.frame(Position=positions[-1],
                            PPI=c(0.13821138,0.08095238,0.21276596,0.37337662,0.29536923,0.39735972,0.21415635,0.29383430,0.23135433))

full.dat=merge(full.dat,plays.per.inning)
