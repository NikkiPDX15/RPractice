# HotCase Container Look up -- By Trailer 
# Gresham Parts Master Warehouse
# Made by: Nichole Freeman, Supervisor
# Completed Date: 7.12.2017

# references:https://www.programiz.com/r-programming/examples/user-input 
# for readline comments 

# enter trailer name
ask <- readline(prompt="Hot Part? What is the trailer:")

# convert character into integer if needed (for later use)
# by using as.integer(promt)

# get container log (what's left to be unloaded)
require(readxl)
ConLog <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")
# Subset what columns we need
ConDoor <- ConLog[c(3:5)]
#clear out lines by getting rid of NA's 
NADoor <- as.array(which(!is.na(ConDoor$CONTAINER)))
ConDoor2 <- data.frame(ConDoor[NADoor,])

# Tells us which door the hotcase is in
ConDoor2[which(ConDoor2$CONTAINER==ask),]


######## Now for unloaded containers ##########

# reads file of the product that is on the floor
ask2 <- readline(prompt="Hot Part? What is the trailer:")
promt <- data.frame(ask2)
ULConLog <- read.table(file.choose(), header=T, sep=",")
UL <- ULConLog[c(1:6)]

# tells us which door the hotcase was unloaded 
UL[which(UL$CONTAINER == ask2),]



