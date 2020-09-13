

#**************************************************
# Function to read files in a directory
# and put them into a list
# screening them for the extension type


file.input2 <- function(my.path, extension = "csv")
{
  setwd(my.path)
  my.files <- list.files(my.path)
  sapply(1:length(my.files), function(i)
  {
    if (gregexpr(extension, my.files[i])>0)
      read.csv(my.files[i], na.strings = c("", " ", NA))
  })
  
}

# Example with csv files in txt
# files <- file.input2("/Users/donaldbrown/Dropbox/department/Classes/SYS4021/sys 421 2012/data/TrainAccidents/", "txt")



# Function to create a data frame as the combination of multiple data frames in a list.

files2DF <- function(path, extension = "csv")
{
  files <- file.input2(path, extension)
  #comvar <- innerjoin(files)
  DF <- files[[1]]
  for(i in 2:length(files))
  {
    comvar = intersect(colnames(DF), colnames(files[[i]]))
    DF <- rbind(DF[,comvar], files[[i]][,comvar])
  }
  DF
}


# Example with csv files with txt extension
#acts <- files2DF("/Users/donaldbrown/Dropbox/department/Classes/SYS4021/sys 421 2012/data/TrainAccidents/", "txt")


# Function to put data frames from a list into a single DF 
# with an inner join on the set of variables (Vars) provided


combine.data <- function(Data.List, Vars)
{
  DF <- rbind(Data.List[[1]][, Vars])
  for(i in 2:length(Data.List))
  {
    DF <- rbind(DF, Data.List[[i]][,Vars])
  }
  DF
}



# function to find & remove variables with missing
# data but keep ones in list of names

removeNAvar <- function(DF, Names = NULL)
{
  nacount <- apply(DF,2, "nafind")
  varWna <- which(nacount > 0)
  keepidx <- sapply(Names, function(i)which(colnames(DF)[varWna] == i))
  varWna <- varWna[-keepidx]
  DF[,-varWna]
}

# Function to cound the NAs in a variable
# or DF column

nafind <-function(x){sum(is.na(x))}


#************************************
#
# Data Cleaning Function
#
#************************************

# Get the clean data set
# This function removes duplicates
# removes the accident associated with the 9/11/01
# attack
# Removes rows with NA's but keeps TYPEQ
# Cleans TYPE, TYPEQ, and CAUSE

traindataclean <- function(DF, KeepVar = list("TYPEQ"))
{
  DFClean <- DF[!duplicated(DF[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN", "ACCDMG")]),]
  nine11 <- which(DFClean$MONTH == 9 & DFClean$DAY == 11 & DFClean$YEAR == 1 & DFClean$ACCDMG > 1.5e7)
  DFClean <- DFClean[-nine11, ]
  DFClean <- removeNAvar(DFClean, KeepVar)
  DFClean$TYPE <- factor(DFClean$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))
  DFClean$TYPEQ[which(DFClean$TYPEQ == "")] <- NA
  DFClean$TYPEQ <- factor(DFClean$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"))
  DFClean$CAUSE <- factor(sapply(DFClean$CAUSE, function(i)substr(i, 1, 1)))
  DFClean
}

#*********
# Extreme Accidents
# use box plot whisker

extreme.damage <- function(DF)
{
  dmgbox <- boxplot(DF$ACCDMG)
  xDF <- DF[DF$ACCDMG > dmgbox$stats[5],]
  xDF
}



# Joining the narratives
# Combine them into one vector or list
# where each entry is the complete narrative
# for that accident

join.narr <- function(df.txt)
{
  n <- ncol(df.txt)
  txt.out <- rep(NULL, nrow(df.txt))
  for(i in 1:n)
  {
    txt.out <- paste(txt.out, df.txt[,i])
  }
  txt.out
}