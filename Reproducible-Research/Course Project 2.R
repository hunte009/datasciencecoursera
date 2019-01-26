library(readr)
library(dplyr)
library(stats)
library(qcc)

setwd("C:/Users/User/Documents/coursera/Reproducible Research")
# Dowloading data if it's not already done
if(!file.exists("stormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "stormData.csv.bz2", method = "curl")
}

# Loading data
df <- read.csv(bzfile("stormData.csv.bz2"), sep=",", header=T)

tidydf <- df[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]
tidydf <- df[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]

fatalities <- aggregate(FATALITIES ~ EVTYPE, data=tidydf, sum)
fatalities <- arrange(fatalities,-fatalities$FATALITIES)
fatalities[1,]

injuries <- aggregate(INJURIES ~ EVTYPE, data=tidydf, sum)
injuries <- arrange(injuries,-injuries$INJURIES)
injuries[1,]

round(injuries$INJURIES[1]/sum(injuries$INJURIES)*100,0)
round(fatalities$FATALITIES[1]/sum(fatalities$FATALITIES)*100,0)

str(tidydf[4:7,])

tidydf <- mutate(tidydf,prop=tidydf$PROPDMG)
tidydf <- mutate(tidydf,crop=tidydf$CROPDMG)
tidydf$crop[tidydf$CROPDMGEXP=="K"] <- tidydf$crop[tidydf$CROPDMGEXP=="K"]*1000
tidydf$crop[tidydf$CROPDMGEXP=="M"] <- tidydf$crop[tidydf$CROPDMGEXP=="M"]*1000000
tidydf$crop[tidydf$CROPDMGEXP=="B"] <- tidydf$crop[tidydf$CROPDMGEXP=="B"]*1000000000
tidydf$prop[tidydf$PROPDMGEXP=="K"] <- tidydf$prop[tidydf$PROPDMGEXP=="K"]*1000
tidydf$prop[tidydf$PROPDMGEXP=="M"] <- tidydf$prop[tidydf$PROPDMGEXP=="M"]*1000000
tidydf$prop[tidydf$PROPDMGEXP=="B"] <- tidydf$prop[tidydf$PROPDMGEXP=="B"]*1000000000

damage <- aggregate(crop + prop ~ EVTYPE, data=tidydf, sum)
damage <- arrange(damage,-damage$`crop + prop`)
damage[1:5,]

round(damage$`crop + prop`[1]/sum(damage$`crop + prop`)*100,0)

par(mfrow=c(1,2))
barplot(fatalities$FATALITIES[1:5], main="Number fatalities by event type", 
        xlab="Number of fatalities",
        horiz=FALSE,
        names.arg=fatalities$EVTYPE[1:5], 
        cex.names=0.6,
        col="red")

barplot(injuries$INJURIES[1:5], main="Number injuries by event type", 
        xlab="Event type",
        horiz=FALSE,
        names.arg=injuries$EVTYPE[1:5], 
        cex.names=0.6,
        col="blue")

par(mfrow=c(1,1))
barplot(damage$`crop + prop`[1:10]/1e9, main="Damage by event type", 
        xlab="Event type",
        ylab="Damage in bilion dollars",
        horiz=FALSE,
        names.arg=damage$EVTYPE[1:10], 
        ylim=c(0,160),
        cex.names=0.6,
        col=1:10)
grid(nx = NA, ny = NULL, col = "darkgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
