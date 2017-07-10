# Date 7.5
# Container log read with Back order Read



BackOrder <- read.table(file.choose(), header=T, sep=",")
#save file as csv and choose which one :)

boview1 <- BackOrder[c(2,3,8)]

#Part_Num <- BackOrder[2]
#Part_Desc <- BackOrder[3]
#TOT_BO_QTY <- BackOrder[8]
#WHS <- BackOrder[12]
#PROMISED_DATE	<- BackOrder[13]
#IB_QTY	<- BackOrder[14]
#IB_ORDER	<- BackOrder[15]
#CONTAINER/TRLR	<- BackOrder[16]
#CASE <- BackOrder[17]
#NUM_OF_LINES <- BackOrder[18]

#try <- table(BackOrder$WHS, BackOrder$PART_NUM) # data is savedddd
#try2 <-table(BackOrder$WHS==440)  #shows true and false statements

#if(BackOrder$WHS == 440){print(BackOrder$PART_NUM)}
#can do an and/or statment with & and |

sub1 <- subset(BackOrder, BackOrder$WHS ==440)
#sub2 <- subset(sub1, BackOrder$PART_NUM & BackOrder$PART_DESC & BackOrder$TOT_BO_QTY &
#             BackOrder$PROMISED_DATE & BackOrder$IB_QTY & BackOrder$CONTAINER.TRLR &
#            BackOrder$CASE & BackOrder$NUM_OF_LINES)
#Garbage code

sub2 <- sub1[c(2,3,8,12:18)]
#order(BackOrder$CONTAINER.TRLR, na.last=TRUE)
#Need to work on ordering black containers 


ConLog <- read.table(file.choose(), header=T, sep=",")

ConDoor <- ConLog[c(3:5)]

# Could use merge, try later

# Just container numbers to try to match
#BOCont <- sub2[8]
# Doesnt work, rows do not match Result <- cbind(ConDoor, BOCont)

#WriteXLS(BOCont, ExcelFileName = "BOCont.xls") NOPE
#This writes a table in csv

write.table(ConDoor, file = "ConDoor.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(sub2, file = "sub2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
dataMerge <- merge(ConDoor, sub2 , by= c("CONTAINER"),all=T)


#need to reinstall Java?

#write.xlsx(sub2, file="BOF.xlsx", sheetName="sheet1", row.names=FALSE)
#write.xlsx(ConDoor, file="BOF", sheetName="sheet2", append=TRUE, row.names=FALSE)