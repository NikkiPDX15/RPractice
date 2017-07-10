#Date: 7.5
# Back order files read

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


