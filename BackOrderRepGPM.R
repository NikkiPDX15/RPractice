# Back Order Report 
# Gresham Parts Master Warehouse
# Made by: Nichole Freeman, Supervisor
# Completed Date: 7.9.2017

# Purpose of this program is to read, sort, and compare the 
# Master Back order sheet as well as the GPM Container Log

# This compare report takes 15-20 minutes to do manually
# Now it only takes 2 minutes
# 1.75 minutes would be self formatting and printing the sheet

# Back Order CSV sheet is imported
BackOrder <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")
# Subset dataframe to choose only GPMs Back Orders
sub1 <- subset(BackOrder,BackOrder$WHS ==440)
# Subset to keep columns we need
sub2 <- sub1[c(2,3,8,12:18)]

# Container CSV sheet is imported
ConLog <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")
# Subset what columns we need
ConDoor <- ConLog[c(3:5)]
# get rid of rows that do not have a container under container column
# this also means that we will not be able to see the UPS with this comparison
# which is why an Overall Back Order sheet is printed :)
# gets rid of NA (empty lines)
NADoor <- as.array(which(!is.na(ConDoor$CONTAINER)))
ConDoor2 <- data.frame(ConDoor[NADoor,])

# compares the two sheets, generates which rows have a container value shared
# in both the Back Order sheet and the Container Log
Print1 <- as.array(which(sub2$CONTAINER.TRLR %in% ConDoor$CONTAINER)) 
Print2 <- as.array(which(ConDoor2$CONTAINER %in% sub2$CONTAINER.TRLR)) 

# creates final data frame 
FinalBOReport <- data.frame(sub2[Print1, 1:10]) # want to Keep UPS 
FinalCD <- data.frame(ConDoor2[Print2, 1:3])
#Merges data to one clean sheet
Ultimate <- merge.data.frame(FinalCD, FinalBOReport, 
                             by.x = "CONTAINER", 
                             by.y = "CONTAINER.TRLR", all.x = TRUE)

# Below Prints the Containers that are in the Yard that 
# have backorder parts on them that still need to be emptied

write.table(Ultimate, file = "BackOrderGPM_InYard.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")  

# Below is the overall backorder report that includes UPS 
# UPS back orders not included in the above sheet because it hosts no container

write.table(sub2, file = "BackOrderGPM_OverAll.csv",row.names=FALSE, na="",
          col.names=TRUE, sep=",")


# For New computers using this program:
#set work directory so files can be saved in the area you want it to be saved
# First, find path then you can set it in the folder you want
# getwd()
# setwd("")