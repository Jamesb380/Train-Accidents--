
#***********************************************
#
#		Regression Evaluation
#
#**********************************************

#***********************************************
#
#		Load the Data
#
#***********************************************


xdmg <- read.csv("extreme.csv", header = T)
str(xdmg)
dim(xdmg)
colnames(xdmg)
#***********************************************
#
#		More Data Cleaning
#
#***********************************************


# Go down the list of variables and select 
# the ones to use in modeling
# Variable List

matrix(names(xdmg))

xdmgold <- xdmg
xdmg <- xdmg[-109]
#Pulling old crap out here
summary(xdmg[,1:80])

xdmg$WEATHER <- factor(xdmg$WEATHER, labels=c("Clear", "Cloudy", "Rain", "Fog", "Sleet", "Snow") )
xdmg$VISIBLTY <- factor(xdmg$VISIBLTY, labels = c("Dawn", "Day", "Dusk", "Dark"))
xdmg$AMPM <- factor(xdmg$AMPM, labels = c("PM", "AM"))
summary(xdmg$WEATHER)
summary(xdmg$VISIBLTY)
summary(xdmg$AMPM)
# What about TRKCLAS?
summary(xdmg$TRKCLAS)
xdmg$TRKCLAS[which(xdmg$TRKCLAS=="")] <- NA
# Select the variables
xdmg$TIMEHR[which(xdmg$AMPM =="")]
xdmg$AMPM[which(xdmg$AMPM =="")] <- "AM"
summary(xdmg$AMPM)
xdmgClean <-xdmg



# Write the data to a file
write.csv(xdmgClean, "xdmgClean.csv")

summary(xdmgClean)

#keep cols 18-20, 26-33, 35-59, 71, 106, 107
#xdmClean <- xdmg(,c(18:20, 26:33, 35:59, 71, 106))
#*****************************************************
#
# 		Regression with All variables
#
#*****************************************************
# Main effects model
train.main <- lm(ACCDMG~., data=xdmgClean)
summary(train.main)
#adj r2 ~.36
#Using all as predictors from clean data set^^
# Size of data set
