
plot(2002:2019, tapply(xdmg$ACCDMG, as.factor(xdmg$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")


# Source SPM_Panel.R

#source(paste(mysourcepathSP,"SPM_Panel.R",sep = ""))
source("SPM_Panel.R")


# without panel functions
par(mfcol=c(1,1), oma=c(0,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
#pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = xdmg)

# with panel function

uva.pairs(xdmg[,c("TRKDMG","EQPDMG","ACCDMG","TOTINJ","TOTKLD")]) 

png("metrics.png")
uva.pairs(xdmg[ ,c("TRKDMG","EQPDMG","ACCDMG","TOTINJ","TOTKLD")]) 
dev.off()

#Create total number of cars

xdmg$CARSUM <- apply(xdmg[,c(52:57)], 1, sum)

xdmg$propDmg <- apply(xdmg[,c(60:61)], 1, sum)
xdmg$O_dmg <- xdmg$ACCDMG - xdmg$propDmg

xdmg$TYPE <- as.numeric(xdmg$TYPE)

library(ggplot2)
g <- ggplot(xdmg, aes(YEAR,ACCDMG))
g + geom_point(aes(color = TYPEQ), size = 4, alpha = 1/3) + facet_grid(.~AMPM) + geom_smooth(method = "lm")


p <- ggplot(xdmg, aes(x = xdmg$YEAR, fill = as.factor(xdmg$TYPE))) 
png("gghist.png")
p + geom_histogram() + facet_grid(.~TYPEQ) + ggtitle("Types of Accidents vs. Types of Trains") + theme_bw() + 
        scale_fill_manual(values = c("red","yellow","magenta","green", "blue", "pink", "brown", "purple","gray","Turquoise", "beige","cyan","orange"),
                name = "Type of Accident",breaks = c("1","2","3","4","5","6","7","8","9","10","11","12","13"),
                          labels=c("Derailment","Head on Collision", "Rear-end Collision", "side Collision", "Raking Collision","Broken Train Collision",
                                   "Hwy-Rail Crossing","RR Grade Crossing", "Obstruction","Explotive Denonation", "Fire/Violent Rupture", "Other Impacts", "Others - see Narr"))
dev.off()
xdmg$CASUALTY <- xdmg$TOTKLD + xdmg$TOTINJ

totacts$CASUALTY <- totacts$TOTKLD + totacts$TOTINJ

xcas <- totacts[totacts$CASUALTY > 0,]
xcas$CARSUM <- apply(xcas[,c(52:57)], 1, sum)
xcas$propDmg <- apply(xcas[,c(60:61)], 1, sum)
xcas$O_dmg <- xcas$ACCDMG - xcas$propDmg
dim(xcas)


# Look at SPM for preditors and ACCDMG and 
uva.pairs(xdmg[,c("ACCDMG","CASUALTY", "TRNSPD", "TONS","CARSUM", "TIMEHR" )])

png("SPM.png")
uva.pairs(xdmg[,c("ACCDMG","CASUALTY", "TRNSPD", "TONS","CARSUM", "TIMEHR","O_dmg")])
dev.off()

#***********************************************************
#
#  	Principal Components with the Correlation Matrix	
#
#***********************************************************

xdmg$CARSDMG <- as.numeric(xdmg$CARSDMG)
# Principal Components with the Correlation Matrix for extreme data 2 (metrics)

xdmg.pca <- princomp(xdmg[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ", "O_dmg")], cor = TRUE)


# Variance

summary(xdmg.pca)


# View data in the first 2 PC

png("biplot.png")
biplot(xdmg.pca)
dev.off()

xdmg[9, c(21,60,61)]

# Again use png to save it.
png("metrics2.png")
uva.pairs(xdmg[ ,c("TRKDMG","EQPDMG","ACCDMG","TOTINJ","TOTKLD","O_dmg")]) 
dev.off()


# Remove outliers in component 2

xdmg.pca <- princomp(xdmg[-c(9, 2354, 1380),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View the first 2 PC without ouliers

biplot(xdmg.pca)

# Variance plot
png("scree.png")
screeplot(xdmg.pca, main = "Variance for PC of Metrics")
dev.off()


# Loadings

barplot(xdmg.pca$loadings[,1])
barplot(xdmg.pca$loadings[,2])

# Cumulative variance

source("PCAplots.R")

cumplot(xdmg.pca, col = "blue")

#***********************************************************
#
#		PCA for Possible predictors of damage	
#
#***********************************************************



pred.pca <- princomp(xdmg[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")], cor = T )

biplot(pred.pca)


#**********************************************
# Interaction plots with quantitative variables
#**********************************************


#  Extreme Damage

Speed <- cut(xdmg$TRNSPD, c(min(xdmg$TRNSPD),median(xdmg$TRNSPD),max(xdmg$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Weight <- cut(xdmg$TONS, c(min(xdmg$TONS),median(xdmg$TONS),max(xdmg$TONS)), include.lowest = T, labels = c("light", "heavy"))

interaction.plot(Speed, Weight, xdmg$ACCDMG, ylab = "Cost ($)")

# Do this for casualty

interaction.plot(Speed, Weight, xdmg$CASUALTY, ylab = "Cost ($)")

## Other damage which we persume to include BI/Death expenses

#interaction.plot(Speed, Weight, xdmg$##, ylab = "Cost ($)")



#*************************************************
#
#   Creating new variables
#
#*************************************************


# Type of accident

summary(totacts$TYPE)

# Make it a categoical variable
totacts$TYPE <- as.numeric(totacts$TYPE)
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))


summary(totacts$TYPE)

# Type of Train

summary(totacts$TYPEQ)

totacts$TYPEQ[1:50]

summary(as.factor(totacts$TYPEQ))

# Converting missing to NA

totacts$TYPEQ[which(totacts$TYPEQ == "")] <- NA


# New labels


totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"))


# Add type E

levels(totacts$TYPEQ) <- c(levels(totacts$TYPEQ), "E")

summary(totacts$TYPEQ)


# get this for all acciddents (totacts) 


totacts$TYPEQ[which(totacts$TYPEQ == "")] <- NA


# New labels


totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"))

summary(totacts$TYPEQ)





# CAUSE

summary(totacts$CAUSE)

# Create a new variable called Causa
# that uses labels for CAUSE.
# Add it to totacts.

totacts$causa <- rep(NA, nrow(totacts))

totacts$causa[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$causa[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$causa[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$causa[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$causa[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, CAUSE, has to be a factor

totacts$causa <- factor(totacts$causa)

summary(totacts$causa)
###Adding causa to xdmg 
xdmg$causa <- rep(NA, nrow(xdmg))

xdmg$causa[which(substr(xdmg$CAUSE, 1, 1) == "M")] <- "M"
xdmg$causa[which(substr(xdmg$CAUSE, 1, 1) == "T")] <- "T"
xdmg$causa[which(substr(xdmg$CAUSE, 1, 1) == "S")] <- "S"
xdmg$causa[which(substr(xdmg$CAUSE, 1, 1) == "H")] <- "H"
xdmg$causa[which(substr(xdmg$CAUSE, 1, 1) == "E")] <- "E"

xdmg$causa <- factor(xdmg$causa)

summary(xdmg$causa)
##
xcas$causa <- rep(NA, nrow(xcas))

xcas$causa[which(substr(xcas$CAUSE, 1, 1) == "M")] <- "M"
xcas$causa[which(substr(xcas$CAUSE, 1, 1) == "T")] <- "T"
xcas$causa[which(substr(xcas$CAUSE, 1, 1) == "S")] <- "S"
xcas$causa[which(substr(xcas$CAUSE, 1, 1) == "H")] <- "H"
xcas$causa[which(substr(xcas$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, CAUSE, has to be a factor

xcas$causa <- factor(xcas$causa)

summary(xcas$causa)

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

#xcas <- totacts[totacts$CASUALTY > 0,]

# Do a check


#*************************************************
#
#   Graphics for Categorical Variables
#
#*************************************************


#*****************
# Type of accident
#*****************

# Basic bar plot
barplot(table(xdmg$TYPE)) #compare with the totacts plot


# Put all bar plots together

barplot(t(100*cbind(table(totacts$TYPE)/nrow(totacts), table(xdmg$TYPE)/nrow(xdmg), table(xcas$TYPE)/nrow(xcas))), horiz = F, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), args.legend = list(x = 50, y = 80), xlab = "Percent of Accidents")



#*****************
# Type of train
#*****************

barplot(table(xdmg$TYPEQ)) #compare with the totacts plot

# Put all bar plots together

barplot(t(100*cbind(table(totacts$TYPEQ)/nrow(totacts), table(xdmg$TYPEQ)/nrow(xdmg), table(xcas$TYPEQ)/nrow(xcas))), horiz = F, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), xlab = "Types of trains", args.legend = (list = c(x= 50, y = 70)))


#*****************
# causa
#*****************

barplot(table(totacts$causa))#compare with the totacts plot

##xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
xdmg_hum <- xdmg[xdmg$causa == "H",]
xdmg_Mis <- xdmg[xdmg$causa == "M",]
xdmg_Trc <- xdmg[xdmg$causa == "T",]
xdmg_E_M <- xdmg[xdmg$causa == "E",]
xdmg_sig <- xdmg[xdmg$causa == "S",]

xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E5")] <- "AB"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E3")] <- "CD"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E8")] <- "Doors"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E7")] <- "Locomotives"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E4")] <- "TC"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E6")] <- "Wheels"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E2")] <- "Body"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E0")] <- "Brakes"
xdmg_E_M$failures[which(substr(xdmg_E_M$CAUSE, 1,2) == "E1")] <- "TFB"

summary(xdmg_E_M)



sum(xdmg_hum$ACCDMG)/sum(totacts$ACCDMG) ##13.7$ of acc_dmg
sum(xdmg_Mis$ACCDMG)/sum(totacts$ACCDMG)###13.7% of acc_dmg
sum(xdmg_Trc$ACCDMG)/sum(totacts$ACCDMG)##32.24% of acc_dmg
sum(xdmg_E_M$ACCDMG)/sum(totacts$ACCDMG)## 13.4% of acc_dmg
sum(xdmg_sig$ACCDMG)/sum(totacts$ACCDMG)### .28%

sum(xdmg_hum$CASUALTY)/sum(totacts$CASUALTY) ##17.525
sum(xdmg_Mis$CASUALTY)/sum(totacts$CASUALTY)###17.82
sum(xdmg_Trc$CASUALTY)/sum(totacts$CASUALTY)## 22.49
sum(xdmg_E_M$CASUALTY)/sum(totacts$CASUALTY)## .9%
sum(xdmg_sig$CASUALTY)/sum(totacts$CASUALTY)### .001


# Put all bar plots together


barplot(t(100*cbind(table(totacts$causa)/nrow(totacts), table(xdmg$causa)/nrow(xdmg), table(xcas$causa)/nrow(xcas))), horiz = F, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), xlab = "Types of accidents", args.legend = list(x = 50, y =70))



#*********************************
# Distributions for categorial variables
#*********************************

library(lattice)

bwplot(ACCDMG~causa, data = xdmg)
bwplot(ACCDMG~TYPE, data = xdmg)
bwplot(ACCDMG~TYPEQ, data = xdmg)

# scatter plots

xyplot(ACCDMG~TRNSPD | causa, data = xdmg, type = c("p", "r"))

xyplot(ACCDMG~TRNSPD | TYPE, data = xdmg, type = c("p", "r"))
# Repeat for TYPE and TYPEQ
xyplot(ACCDMG~TRNSPD | TYPEQ, data = xdmg, type = c("p", "r"))


#*********************************
# Interaction with Speed
#*********************************

interaction.plot(Speed,xdmg$causa,xdmg$ACCDMG, col = c("black", "red", "purple", "green","blue"), ylab = "Proximate cause - Cost")
interaction.plot(Speed,xdmg$causa,xdmg$CASUALTY, col = c("black", "red", "purple", "green","blue"), ylab = "Proximate cause - Casualty")
interaction.plot(Speed,xdmg$TYPE,xdmg$ACCDMG, col = c("black", "red", "purple", "green","blue"), ylab = "Type  - Cost")
interaction.plot(Speed,xdmg$TYPE,xdmg$CASUALTY, col = c("black", "red", "purple", "green","blue"), ylab = "Type - Casualty")
interaction.plot(Speed,xdmg$TYPEQ,xdmg$ACCDMG, col = c("black", "red", "purple", "green","blue"), ylab = "Type  - Cost")
interaction.plot(Speed,xdmg$TYPEQ,xdmg$CASUALTY, col = c("black", "red", "purple", "green","blue"), ylab = "Type - Casualty")

#**********************
# Extreme Damage
#**********************


# Create the Derail variable & 
# then look at interactions with CAUSE

xdmg$Derail <- (xdmg$TYPE == "Derailment")

xyplot(ACCDMG~TRNSPD | causa * Derail, data = xdmg, type = c("p", "r"))

# Create a Freight variable
# then look at interactions with CAUSE &
# TYPE

xdmg$Freight <- (xdmg$TYPEQ == "Freight")

xyplot(ACCDMG~TRNSPD | causa * Freight, data = xdmg, type = c("p", "r"))

# Derail and Freight

xyplot(ACCDMG~TRNSPD | Derail * Freight, data = xdmg, type = c("p", "r"))
ggplot(xdmg ,aes(x = TRNSPD, y = ACCDMG)) + geom_point()
ggplot(xdmg ,aes(x = TRNSPD, y = CASUALTY)) + geom_point()
plot(totacts$TOTINJ+totacts$TOTKLD ~ totacts$TRNSPD)


write.csv(xdmg, file ="extreme.csv", row.names = F)



