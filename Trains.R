
library(data.table)
library(ggplot2)
library(tidyr)
library(readr)

 
path <- "/Users/james.bennett/Desktop/Trains/Train_Accidents/Data"

all.files <- list.files(path = path ,pattern = ".txt",full.names = T)

df <- lapply(all.files, fread, sep=",")
dt <- rbindlist( df )
setkey( dt , YEAR, MONTH, DAY, TIMEHR, TIMEMIN )

matrix(names(df[[1]]))
        
source("/Users/james.bennett/Desktop/Trains/Train_Accidents/binder.R")
totacts <- files2DF(df)



rm(dt)
rm(df)

summary(totacts)

dim(totacts) ####36217 x 145

h <- colnames(totacts)[-1]
o <- colnames(totacts)[-146]

setnames(totacts, o, h)

colnames(totacts)

# View of accident damage - Most of them are minor accideint 
par(mfcol=c(1,1), oma=c(1,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(2,0,0))
boxplot(totacts$ACCDMG, main = "Boxplot of accident damage")

hist(totacts$ACCDMG)

which(totacts$ACCDMG == max(totacts$ACCDMG))
totacts[c(22929,24124),]  ###they Appear to be duplicates

duplicated(totacts[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

totacts <- totacts[!duplicated(totacts[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]


##finding NA

nafind <- function(x){sum(is.na(x))}

apply(totacts,2, "nafind")

# Do we need all the variables?

matrix(names(totacts))

# Remove unnecessary variables, then get a summary

nacount <- apply(totacts,2, "nafind")

varWna <- which(nacount > 0)

which(colnames(totacts)[varWna] == "TYPEQ")

varWna <- varWna[-which(colnames(totacts)[varWna]== "TYPEQ")]



#totacts <- totacts[, -varWna]


totacts <- totacts[, -c(5,6,13,26,36,43,45,49,81,85,90,91,92,93,94,95,96,97, 100, 101, 102, 103, 104, 106, 115, 116, 117, 118, 121, 137, 140, 141, 142, 143, 144, 145, 146)]

# Save your data frame

# check you working directory and change it if necessary
getwd()

write.csv(totacts, file ="/Users/james.bennett/Desktop/Trains/Output/totactsClean.csv", row.names = F)

summary(totacts)
# How many accidents?

dim(totacts)


sum(totacts$ACCDMG)  ###Total cost of Accidents is $5,464,913,062


sum(totacts$ACCDMG)/18  ### Average cost per year is 303,606,281


# first yearly costs (sums)
dmgyrsum <- tapply(totacts$ACCDMG, totacts$YEAR, sum)

###most killed in a single Accident

which(totacts$TOTKLD == max(totacts$TOTKLD))

totacts[19706,'TOTKLD']  ##25 is the most people killed in an accident 

mean(dmgyrsum)

sum(totacts$TOTKLD) # Total number killed - 819 People


max(totacts$TOTKLD) # Largest number killed in an accident - 25 people


sum(totacts$TOTINJ) # Total number injured - 8216 Injuries


max(totacts$TOTINJ) ##what is the most people injured in an accident 1000

round(sum(totacts$TOTINJ)/18) # What is the average number of injuries per year?  456

# types of variables
str(totacts)



#**************************************************
#
#   Time series of Accidents
#
#**************************************************

# Yearly no. of accidents

plot(2:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, length), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Accidents per Year", lwd =2)


# Yearly total cost of accidents

plot(2:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

symbols(2002:2019, tapply(totacts$ACCDMG, totacts$YEAR, sum), circles=tapply(totacts$ACCDMG, totacts$YEAR, max),inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total Accident Damage")
lines(2002:2019, tapply(totacts$ACCDMG, totacts$YEAR, sum))
# Yearly maximum cost of accidents

plot(2:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Max Damage per Year", lwd =2)

symbols(2002:2019, tapply(totacts$ACCDMG, totacts$YEAR, max), circles=tapply(totacts$ACCDMG, totacts$YEAR, max),inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total Accident Damage")
lines(2002:2019, tapply(totacts$ACCDMG, totacts$YEAR, max))



# Total killed and total injured and the sum of them.
plot(2:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Deaths per Year", lwd =2)

symbols(2002:2019, tapply(totacts$TOTKLD, totacts$YEAR, sum), circles = tapply(totacts$TOTKLD, totacts$YEAR, max), inches = 0.35, fg ="yellow", bg ="red", xlab = "Year", ylab = "People Killed", main = "Total Killed")
lines(2002:2019, tapply(totacts$TOTKLD, totacts$YEAR, sum))

plot(2:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Max Deaths per Year", lwd =2)

symbols(2002:2019, tapply(totacts$TOTKLD, totacts$YEAR, max), circles = tapply(totacts$TOTKLD, totacts$YEAR, max), inches = 0.35, fg ="yellow", bg ="red", xlab = "Year", ylab = "People Killed", main = "Total Killed")
lines(2002:2019, tapply(totacts$TOTKLD, totacts$YEAR, max))

plot(2:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Injuries per Year", lwd =2)

symbols(2002:2019, tapply(totacts$TOTINJ, totacts$YEAR, sum), circles = tapply(totacts$TOTINJ, totacts$YEAR, max), inches = 0.35, fg ="black", bg ="red", xlab = "Year", ylab = "People Injured", main = "Total Injured")
lines(2002:2019, tapply(totacts$TOTINJ, totacts$YEAR, sum))

plot(2:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Max Injuries per Year", lwd =2)

symbols(2002:2019, tapply(totacts$TOTINJ, totacts$YEAR, max), circles = tapply(totacts$TOTINJ, totacts$YEAR, max), inches = 0.35, fg ="black", bg ="red", xlab = "Year", ylab = "People Injured", main = "Total Injured")
lines(2002:2019, tapply(totacts$TOTINJ, totacts$YEAR, max))


#***********************************
#
# 	histograms of ACCDMG and TEMP
#
#***********************************


par(mfrow = c(2,2))

hist(totacts$TEMP, breaks = "scott", main = "Accident Temperatures (Scott)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = "fd", main = "Accident Temperatures (FD)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, main = "Accident Temperatures (Sturges)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = 100, main = "Accident Temperatures (100)", xlab = "Temp (F)", col = "steelblue")

par(mfrow = c(1,1))





#*********************************************************************
#
# 				Box Plots of Metrics
#         and Extreme Accidents
#
#*********************************************************************

#*****************************
# ACCDMG

boxplot(totacts$ACCDMG, main = "Xtreme Accident damage")
boxplot(totacts$TOTKLD, main = "Deaths in Extreme incident")

# Plot only the extreme points
# (extreme defined by the box plot rule)

# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)

dmgbox2 <- boxplot(totacts$TOTKLD)

# How many extreme damage accidents?

length(dmgbox$out) ##extreme accident dmg 5922

length(dmgbox2$out) ## 631 accidents involving deaths (this is not commom)
dmgbox$stats


# What proportion of accidents are extreme? (round to 2 digits) - 13%
round(length(dmgbox$out)/length(totacts$ACCDMG),2)

# What is the proportion of costs for extreme damage accidents? (round to 2 digits)
round(sum(dmgbox$out)/sum(totacts$ACCDMG),2) ##13% causes 74% of the damages - Insanity!!
# Create a data frame with just the extreme ACCDMG accidents
round(length(dmgbox2$out)/length(totacts$TOTKLD),2)
##.01 are extreme - deaths are an ware event
round(sum(dmgbox2$out)/sum(totacts$TOTKLD),2)

##all deaths are were events

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)
###4862 are were 

# Look at the boxplots and histograms of these extreme accidents

boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:18, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Extreme Accident Damage per Year")

# also plot number of accidents per year.

plot(1:18, tapply(xdmg$ACCDMG, xdmg$YEAR, length), type = "l", xlab = "Year", ylab = "No. of Accidents", main = "Number of Extreme Accidents per Year")

# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot
##Lots of Derailments - wonder is speeding has to do with this - Type = 1

# Repeat for TOTKLD and TOTINJ
# Create a variable called Casualty = TOTKLD + TOTINJ

max(totacts$TOTINJ) ##1000 in a single accident 
max(totacts$TOTKLD) ##25 in a single maccident
Casualidad = totacts$TOTKLD + totacts$TOTINJ
max(Casualidad) ###1001
plot(2:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of KILLED", lwd =2)
plot(2:max(totacts$YEAR), tapply(totacts$TOTINJ, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Injured", lwd =2)
plot(2:max(totacts$YEAR), tapply(Casualidad, totacts$YEAR, max), type = "l", col = "blue", xlab = "Year", ylab = "Frequency", main = "Combined Casualties", lwd =2)




