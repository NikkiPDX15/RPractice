# Unloaded trailer -- Back Order Report 
# Gresham Parts Master Warehouse
# Made by: Nichole Freeman, Supervisor
# Completed Date: 7.11.2017


# Purpose of this program is to read, sort, and compare the 
# Master Back order sheet as well as the GPM Container Log 
# to see if the container with back orders on it was unloaded

#install.packages("ghit")

#install.packages("tabulizer")
BackOrder <- read.table(file.choose(),na.strings=c("", "NA"), header=T, sep=",")

sub1 <- subset(BackOrder,BackOrder$WHS ==440)
sub2 <- sub1[c(2,3,8,12:18)]


ULConLog <- read.table(file.choose(), header=T, sep=",")
UL <- ULConLog[c(1:6)]

Print1 <- as.array(which(sub2$CONTAINER.TRLR %in% UL$CONTAINER)) 
Print2 <- as.array(which(ULConLog$CONTAINER %in% sub2$CONTAINER.TRLR)) 

FinalBOReport <- data.frame(sub2[Print1, 1:10]) # want to Keep UPS 
FinalULCD <- data.frame(UL[Print2, 1:6])

UltimateUL <- merge.data.frame(FinalULCD, FinalBOReport, 
                             by.x = "CONTAINER", 
                             by.y = "CONTAINER.TRLR", all.x = TRUE)

# Below Prints the Containers that have been unloaded.
# the door represents where the empty container was pulled from
#which means it might be in a yard, but at least it narrows the search down
# when looking for parts. Need a part that was unloaded on Jun30? look for 
# container log on June 30 wit that trailer number :)

write.table(UltimateUL, file = "BackOrderGPM_OnFloor.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")  

# Below is the overall backorder report that includes UPS.
write.table(sub2, file = "BackOrderGPM_OverAll.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")



# For New computers using this program:
#set work directory so files can be saved in the area you want it to be saved
# First, find path then you can set it in the folder you want
# getwd()
# setwd("")