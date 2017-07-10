# Back Order Report 
# Gresham Parts Master Warehouse
# Made by: Nichole Freeman, Supervisor
# Completed Date: 7.9.2017

# Purpose of this program is to read, sort, and compare the 
# Master Back order sheet as well as the GPM Container Log

BackOrder <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")

BOList <- as.list(BackOrder$CONTAINER.TRLR)

sub1 <- subset(BackOrder,BackOrder$WHS ==440)
sub2 <- sub1[c(2,3,8,12:18)]

#IS.NA.useful some other time 
#BORep <-sub2[rowSums(is.na(sub2[8,])) == 0, ]
#sub3 <- as.list(sub2[8])

#unique(sub3)
#News <- subset(sub3, !duplicated(BackOrder$CONTAINER.TRLR))


ConLog <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")
ConDoor <- ConLog[c(3:5)]
NADoor <- as.array(which(!is.na(ConDoor$CONTAINER)))
ConDoor2 <- data.frame(ConDoor[NADoor,])


Print1 <- as.array(which(sub2$CONTAINER.TRLR %in% ConDoor$CONTAINER)) # Want to keep UPS
Print2 <- as.array(which(ConDoor2$CONTAINER %in% sub2$CONTAINER.TRLR)) 

FinalBOReport <- data.frame(sub2[Print1, 1:10]) # want to Keep UPS 
FinalCD <- data.frame(ConDoor2[Print2, 1:3])

Ultimate <- merge.data.frame(FinalCD, FinalBOReport, 
                             by.x = "CONTAINER", 
                             by.y = "CONTAINER.TRLR", all.x = TRUE)

# Below Prints the Containers that are in the Yard that 
# have backorder parts on them that need to be emptied still

write.table(Ultimate, file = "BackOrderGPM_InYard.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")  

# Below is the overall backorder report that includes UPS.
write.table(sub2, file = "BackOrderGPM_OverAll.csv",row.names=FALSE, na="",
          col.names=TRUE, sep=",")