# OIP Program 
# Gresham Parts Master Warehouse
# Made by: Nichole Freeman, Supervisor
# Started Date: 7.12.2017

# install.packages("gdata")  for reading excel future use**
# install.packages("lubridate")

#Ask today's date for late comparison
Date <- readline(prompt = "What is today's date (M/DD/2017): ")

require(lubridate)
WeekOut1 <- as.Date(Date, format='%m/%d/%Y') - 1
WeekOut2 <- as.Date(WeekOut1, format='%m/%d/%Y') - 1
WeekOut3 <- as.Date(WeekOut2, format='%m/%d/%Y') - 1
WeekOut4 <- as.Date(WeekOut3, format='%m/%d/%Y') - 1
WeekOut5 <- as.Date(WeekOut4, format='%m/%d/%Y') - 1
WeekOut6 <- as.Date(WeekOut5, format='%m/%d/%Y') - 1


SNESDC1<- format(WeekOut1, format='%m/%d/%Y')
SNESDC2<- format(WeekOut2, format='%m/%d/%Y')
SNESDC3<- format(WeekOut3, format='%m/%d/%Y')
SNESDC4<- format(WeekOut4, format='%m/%d/%Y')
SNESDC5<- format(WeekOut5, format='%m/%d/%Y')
SNESDC6<- format(WeekOut6, format='%m/%d/%Y')

SDCSNE_OKDate <- c(Date,SNESDC1, SNESDC2, SNESDC3, SNESDC4, SNESDC5, SNESDC6)


# Date reviews
# http://www.statmethods.net/input/dates.html



# save for todays date
# today <- Sys.Date()
# format(today, format="%m/%d/%y")


#Read in csv from Red Prairie
df1 = read.csv (file.choose(), na.strings=c("", "NA"))
# Rid of blank rows
df2 <- df1[-c(1:4),]
# Rid of Black columns
df3<- df2[-c(1,5,7,10,11,16,17,19:21)]
#give column names
col_headings <- c('Order#','Dealer','Order', 'Stat', 'Lines', 
                  'SDN#', 'Weight', 'Cube', 'Stock Days', 
                  'Oracle Date', 'Red Prairie Date')
names(df3) <- col_headings
OrderDone= c("INTE", "INDE")

df33<- subset(df3, df3$Order == OrderDone)

#gets rid of NA's in rows
df4 <- df33[which(!is.na(df33$`Order#`)),]
#Sets late bar
Late <- subset(df4, df4$`Oracle Date` != Date)

# Set the inprocess sort
InProcess <- c(0,80,81)
# Set pending sort
PendingLoad <- c(10)

########## AUR ########################
AUR <- df4[df4$`Stock Days` %in% "Aurora RDC",]
#AURLATEDate <- subset(AUR, AUR$`Oracle Date` != Date)
AUR_IP_Total <- AUR[AUR$Stat %in% InProcess,]
AUR_IP_OK <- subset(AUR_IP_Total, AUR_IP_Total$`Oracle Date` == Date)
AUR_PENDING_LOAD_Total <- AUR[AUR$Stat %in% PendingLoad,]
AUR_OK_PENDING_LOAD <- subset(AUR_PENDING_LOAD_Total, AUR_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayAUR = as.numeric(as.array(AUR$Lines))
Total_AUR_Lines <- sum(ArrayAUR) # Total Lines for this RDC

ArrayAUR_IP_Total = as.numeric(as.array(AUR_IP_Total$Lines))
ArrayAUR_IP_OK = as.numeric(as.array(AUR_IP_OK$Lines))
ArrayAUR_PENDING_LOAD_Total = as.numeric(as.array(AUR_PENDING_LOAD_Total$Lines))
ArrayAUR_OK_PENDING_LOAD = as.numeric(as.array(AUR_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultAUR_LATEIP = (sum(ArrayAUR_IP_Total) - sum(ArrayAUR_IP_OK))
ResultAUR_OKIP = sum(ArrayAUR_IP_OK)
ResultAUR_LATE_PendLoad = (sum(ArrayAUR_PENDING_LOAD_Total) - sum(ArrayAUR_OK_PENDING_LOAD))
ResultAUR_OKPendLoad = sum(ArrayAUR_OK_PENDING_LOAD)

# to check if all total up, can do the following:
#sum(ResultAUR_LATE_PendLoad, ResultAUR_OKPendLoad, 
    #              ResultAUR_OKIP, ResultAUR_LATEIP)

#####################################
############ COP ####################

COP <- df4[df4$`Stock Days` %in% "Coppell RDC",]

COP_IP_Total <- COP[COP$Stat %in% InProcess,]
COP_IP_OK <- subset(COP_IP_Total, COP_IP_Total$`Oracle Date` == Date)
COP_PENDING_LOAD_Total <- COP[COP$Stat %in% PendingLoad,]
COP_OK_PENDING_LOAD <- subset(COP_PENDING_LOAD_Total, COP_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayCOP = as.numeric(as.array(COP$Lines))
Total_COP_Lines <- sum(ArrayCOP) # Total Lines for this RDC

ArrayCOP_IP_Total = as.numeric(as.array(COP_IP_Total$Lines))
ArrayCOP_IP_OK = as.numeric(as.array(COP_IP_OK$Lines))
ArrayCOP_PENDING_LOAD_Total = as.numeric(as.array(COP_PENDING_LOAD_Total$Lines))
ArrayCOP_OK_PENDING_LOAD = as.numeric(as.array(COP_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultCOP_LATEIP = (sum(ArrayCOP_IP_Total) - sum(ArrayCOP_IP_OK))
ResultCOP_OKIP = sum(ArrayCOP_IP_OK)
ResultCOP_LATE_PendLoad = (sum(ArrayCOP_PENDING_LOAD_Total) - sum(ArrayCOP_OK_PENDING_LOAD))
ResultCOP_OKPendLoad = sum(ArrayCOP_OK_PENDING_LOAD)

###################################
##############DOU ################

DOU <- df4[df4$`Stock Days` %in% "Douglasville RD",]

DOU_IP_Total <- DOU[DOU$Stat %in% InProcess,]
DOU_IP_OK <- subset(DOU_IP_Total, DOU_IP_Total$`Oracle Date` == Date)
DOU_PENDING_LOAD_Total <- DOU[DOU$Stat %in% PendingLoad,]
DOU_OK_PENDING_LOAD <- subset(DOU_PENDING_LOAD_Total, DOU_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayDOU = as.numeric(as.array(DOU$Lines))
Total_DOU_Lines <- sum(ArrayDOU) # Total Lines for this RDC

ArrayDOU_IP_Total = as.numeric(as.array(DOU_IP_Total$Lines))
ArrayDOU_IP_OK = as.numeric(as.array(DOU_IP_OK$Lines))
ArrayDOU_PENDING_LOAD_Total = as.numeric(as.array(DOU_PENDING_LOAD_Total$Lines))
ArrayDOU_OK_PENDING_LOAD = as.numeric(as.array(DOU_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultDOU_LATEIP = (sum(ArrayDOU_IP_Total) - sum(ArrayDOU_IP_OK))
ResultDOU_OKIP = sum(ArrayDOU_IP_OK)
ResultDOU_LATE_PendLoad = (sum(ArrayDOU_PENDING_LOAD_Total) - sum(ArrayDOU_OK_PENDING_LOAD))
ResultDOU_OKPendLoad = sum(ArrayDOU_OK_PENDING_LOAD)

##################################
##############FLO##################

FLO <- df4[df4$`Stock Days` %in% "Florence RDC",]

FLO_IP_Total <- FLO[FLO$Stat %in% InProcess,]
FLO_IP_OK <- subset(FLO_IP_Total, FLO_IP_Total$`Oracle Date` == Date)
FLO_PENDING_LOAD_Total <- FLO[FLO$Stat %in% PendingLoad,]
FLO_OK_PENDING_LOAD <- subset(FLO_PENDING_LOAD_Total, FLO_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayFLO = as.numeric(as.array(FLO$Lines))
Total_FLO_Lines <- sum(ArrayFLO) # Total Lines for this RDC

ArrayFLO_IP_Total = as.numeric(as.array(FLO_IP_Total$Lines))
ArrayFLO_IP_OK = as.numeric(as.array(FLO_IP_OK$Lines))
ArrayFLO_PENDING_LOAD_Total = as.numeric(as.array(FLO_PENDING_LOAD_Total$Lines))
ArrayFLO_OK_PENDING_LOAD = as.numeric(as.array(FLO_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultFLO_LATEIP = (sum(ArrayFLO_IP_Total) - sum(ArrayFLO_IP_OK))
ResultFLO_OKIP = sum(ArrayFLO_IP_OK)
ResultFLO_LATE_PendLoad = (sum(ArrayFLO_PENDING_LOAD_Total) - sum(ArrayFLO_OK_PENDING_LOAD))
ResultFLO_OKPendLoad = sum(ArrayFLO_OK_PENDING_LOAD)

######################################
############### LEB ##################

LEB <- df4[df4$`Stock Days` %in% "LEB YRC",]

LEB_IP_Total <- LEB[LEB$Stat %in% InProcess,]
LEB_IP_OK <- subset(LEB_IP_Total, LEB_IP_Total$`Oracle Date` == Date)
LEB_PENDING_LOAD_Total <- LEB[LEB$Stat %in% PendingLoad,]
LEB_OK_PENDING_LOAD <- subset(LEB_PENDING_LOAD_Total, LEB_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayLEB = as.numeric(as.array(LEB$Lines))
Total_LEB_Lines <- sum(ArrayLEB) # Total Lines for this RDC

ArrayLEB_IP_Total = as.numeric(as.array(LEB_IP_Total$Lines))
ArrayLEB_IP_OK = as.numeric(as.array(LEB_IP_OK$Lines))
ArrayLEB_PENDING_LOAD_Total = as.numeric(as.array(LEB_PENDING_LOAD_Total$Lines))
ArrayLEB_OK_PENDING_LOAD = as.numeric(as.array(LEB_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultLEB_LATEIP = (sum(ArrayLEB_IP_Total) - sum(ArrayLEB_IP_OK))
ResultLEB_OKIP = sum(ArrayLEB_IP_OK)
ResultLEB_LATE_PendLoad = (sum(ArrayLEB_PENDING_LOAD_Total) - sum(ArrayLEB_OK_PENDING_LOAD))
ResultLEB_OKPendLoad = sum(ArrayLEB_OK_PENDING_LOAD)

################################
###########ONT##################

ONT <- df4[df4$`Stock Days` %in% "Ontario RDC",]

ONT_IP_Total <- ONT[ONT$Stat %in% InProcess,]
ONT_IP_OK <- subset(ONT_IP_Total, ONT_IP_Total$`Oracle Date` == Date)
ONT_PENDING_LOAD_Total <- ONT[ONT$Stat %in% PendingLoad,]
ONT_OK_PENDING_LOAD <- subset(ONT_PENDING_LOAD_Total, ONT_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayONT = as.numeric(as.array(ONT$Lines))
Total_ONT_Lines <- sum(ArrayONT) # Total Lines for this RDC

ArrayONT_IP_Total = as.numeric(as.array(ONT_IP_Total$Lines))
ArrayONT_IP_OK = as.numeric(as.array(ONT_IP_OK$Lines))
ArrayONT_PENDING_LOAD_Total = as.numeric(as.array(ONT_PENDING_LOAD_Total$Lines))
ArrayONT_OK_PENDING_LOAD = as.numeric(as.array(ONT_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultONT_LATEIP = (sum(ArrayONT_IP_Total) - sum(ArrayONT_IP_OK))
ResultONT_OKIP = sum(ArrayONT_IP_OK)
ResultONT_LATE_PendLoad = (sum(ArrayONT_PENDING_LOAD_Total) - sum(ArrayONT_OK_PENDING_LOAD))
ResultONT_OKPendLoad = sum(ArrayONT_OK_PENDING_LOAD)

############################
###########PLO##############

PLO <- df4[df4$`Stock Days` %in% "Portland RDC",]

PLO_IP_Total <- PLO[PLO$Stat %in% InProcess,]
PLO_IP_OK <- subset(PLO_IP_Total, PLO_IP_Total$`Oracle Date` == Date)
PLO_PENDING_LOAD_Total <- PLO[PLO$Stat %in% PendingLoad,]
PLO_OK_PENDING_LOAD <- subset(PLO_PENDING_LOAD_Total, PLO_PENDING_LOAD_Total$`Oracle Date` == Date)

#trun Lines column into array of numbers to total lines
ArrayPLO = as.numeric(as.array(PLO$Lines))
Total_PLO_Lines <- sum(ArrayPLO) # Total Lines for this RDC

ArrayPLO_IP_Total = as.numeric(as.array(PLO_IP_Total$Lines))
ArrayPLO_IP_OK = as.numeric(as.array(PLO_IP_OK$Lines))
ArrayPLO_PENDING_LOAD_Total = as.numeric(as.array(PLO_PENDING_LOAD_Total$Lines))
ArrayPLO_OK_PENDING_LOAD = as.numeric(as.array(PLO_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultPLO_LATEIP = (sum(ArrayPLO_IP_Total) - sum(ArrayPLO_IP_OK))
ResultPLO_OKIP = sum(ArrayPLO_IP_OK)
ResultPLO_LATE_PendLoad = (sum(ArrayPLO_PENDING_LOAD_Total) - sum(ArrayPLO_OK_PENDING_LOAD))
ResultPLO_OKPendLoad = sum(ArrayPLO_OK_PENDING_LOAD)


############################
########### SDC ###########


SDC <- df4[df4$`Stock Days` %in% "SDC",]

SDC_IP_Total <- SDC[SDC$Stat %in% InProcess,]
SDC_IP_OK <- subset(SDC_IP_Total, SDC_IP_Total$`Oracle Date` == SDCSNE_OKDate)
SDC_PENDING_LOAD_Total <- SDC[SDC$Stat %in% PendingLoad,]
SDC_OK_PENDING_LOAD <- subset(SDC_PENDING_LOAD_Total, SDC_PENDING_LOAD_Total$`Oracle Date` == SDCSNE_OKDate)

#trun Lines column into array of numbers to total lines
ArraySDC = as.numeric(as.array(SDC$Lines))
Total_SDC_Lines <- sum(ArraySDC) # Total Lines for this RDC

ArraySDC_IP_Total = as.numeric(as.array(SDC_IP_Total$Lines))
ArraySDC_IP_OK = as.numeric(as.array(SDC_IP_OK$Lines))
ArraySDC_PENDING_LOAD_Total = as.numeric(as.array(SDC_PENDING_LOAD_Total$Lines))
ArraySDC_OK_PENDING_LOAD = as.numeric(as.array(SDC_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultSDC_LATEIP = (sum(ArraySDC_IP_Total) - sum(ArraySDC_IP_OK))
ResultSDC_OKIP = sum(ArraySDC_IP_OK)
ResultSDC_LATE_PendLoad = (sum(ArraySDC_PENDING_LOAD_Total) - sum(ArraySDC_OK_PENDING_LOAD))
ResultSDC_OKPendLoad = sum(ArraySDC_OK_PENDING_LOAD)

##############################
#######SNE ##################

SNE <- df4[df4$`Stock Days` %in% "SNE",]

SNE_IP_Total <- SNE[SNE$Stat %in% InProcess,]
SNE_IP_OK <- subset(SNE_IP_Total, SNE_IP_Total$`Oracle Date` == SDCSNE_OKDate)
SNE_PENDING_LOAD_Total <- SNE[SNE$Stat %in% PendingLoad,]
SNE_OK_PENDING_LOAD <- subset(SNE_PENDING_LOAD_Total, SNE_PENDING_LOAD_Total$`Oracle Date` == SDCSNE_OKDate)

#trun Lines column into array of numbers to total lines
ArraySNE = as.numeric(as.array(SNE$Lines))
Total_SNE_Lines <- sum(ArraySNE) # Total Lines for this RDC

ArraySNE_IP_Total = as.numeric(as.array(SNE_IP_Total$Lines))
ArraySNE_IP_OK = as.numeric(as.array(SNE_IP_OK$Lines))
ArraySNE_PENDING_LOAD_Total = as.numeric(as.array(SNE_PENDING_LOAD_Total$Lines))
ArraySNE_OK_PENDING_LOAD = as.numeric(as.array(SNE_OK_PENDING_LOAD$Lines))

# Sum lines for RDC for Final break Down
ResultSNE_LATEIP = (sum(ArraySNE_IP_Total) - sum(ArraySNE_IP_OK))
ResultSNE_OKIP = sum(ArraySNE_IP_OK)
ResultSNE_LATE_PendLoad = (sum(ArraySNE_PENDING_LOAD_Total) - sum(ArraySNE_OK_PENDING_LOAD))
ResultSNE_OKPendLoad = sum(ArraySNE_OK_PENDING_LOAD)




##################### Now we create a table for all of our Data################

OIP <- data.frame(matrix(ncol = 6, nrow = 10))
x <- c("RDC", "TOTAL", "(Status 0, 80, 81) LATE LINES IN PROCESS",
       "(Status 0, 80, 81) ON-TIME LINES IN PROCESS", "(Status 10) Pending 
       Load - LATE", "(Status 10) Pending Load - OK")
colnames(OIP) <- x

OIP[1,1] = "AUR"
  OIP[1,2] = Total_AUR_Lines
  OIP[1,3] = ResultAUR_LATEIP
  OIP[1,4] = ResultAUR_OKIP
  OIP[1,5] = ResultAUR_LATE_PendLoad
  OIP[1,6] = ResultAUR_OKPendLoad
OIP[2,1] = "COP"
  OIP[2,2] = Total_COP_Lines
  OIP[2,3] = ResultCOP_LATEIP
  OIP[2,4] = ResultCOP_OKIP
  OIP[2,5] = ResultCOP_LATE_PendLoad
  OIP[2,6] = ResultCOP_OKPendLoad
OIP[3,1] = "DOU"
  OIP[3,2] = Total_DOU_Lines
  OIP[3,3] = ResultDOU_LATEIP
  OIP[3,4] = ResultDOU_OKIP
  OIP[3,5] = ResultDOU_LATE_PendLoad
  OIP[3,6] = ResultDOU_OKPendLoad
OIP[4,1] = "FLO"
  OIP[4,2] = Total_FLO_Lines
  OIP[4,3] = ResultFLO_LATEIP
  OIP[4,4] = ResultFLO_OKIP
  OIP[4,5] = ResultFLO_LATE_PendLoad
  OIP[4,6] = ResultFLO_OKPendLoad
OIP[5,1] = "LEB"
  OIP[5,2] = Total_LEB_Lines
  OIP[5,3] = ResultLEB_LATEIP
  OIP[5,4] = ResultLEB_OKIP
  OIP[5,5] = ResultLEB_LATE_PendLoad
  OIP[5,6] = ResultLEB_OKPendLoad
OIP[6,1] = "ONT"
  OIP[6,2] = Total_ONT_Lines
  OIP[6,3] = ResultONT_LATEIP
  OIP[6,4] = ResultONT_OKIP
  OIP[6,5] = ResultONT_LATE_PendLoad
  OIP[6,6] = ResultONT_OKPendLoad
OIP[7,1] = "PLO"
  OIP[7,2] = Total_PLO_Lines
  OIP[7,3] = ResultPLO_LATEIP
  OIP[7,4] = ResultPLO_OKIP
  OIP[7,5] = ResultPLO_LATE_PendLoad
  OIP[7,6] = ResultPLO_OKPendLoad
OIP[8,1] = "SDC"
  OIP[8,2] = Total_SDC_Lines
  OIP[8,3] = ResultSDC_LATEIP
  OIP[8,4] = ResultSDC_OKIP
  OIP[8,5] = ResultSDC_LATE_PendLoad
  OIP[8,6] = ResultSDC_OKPendLoad
OIP[9,1] = "SNE"
  OIP[9,2] = Total_SNE_Lines
  OIP[9,3] = ResultSNE_LATEIP
  OIP[9,4] = ResultSNE_OKIP
  OIP[9,5] = ResultSNE_LATE_PendLoad
  OIP[9,6] = ResultSNE_OKPendLoad

OIP[10,1] = "Total"
  OIP[10,2] = sum(Total_AUR_Lines,Total_COP_Lines,Total_DOU_Lines,Total_FLO_Lines,
                  Total_LEB_Lines,Total_ONT_Lines, Total_PLO_Lines,
                  Total_SDC_Lines,Total_SNE_Lines)
  OIP[10,3] = sum(ResultAUR_LATEIP, ResultCOP_LATEIP, ResultDOU_LATEIP, ResultFLO_LATEIP,
                  ResultLEB_LATEIP, ResultONT_LATEIP, ResultPLO_LATEIP,
                  ResultSDC_LATEIP, ResultSNE_LATEIP)
  OIP[10,4] = sum(ResultAUR_OKIP, ResultCOP_OKIP, ResultDOU_OKIP, ResultFLO_OKIP,
                  ResultLEB_OKIP, ResultONT_OKIP, ResultPLO_LATEIP,
                  ResultSDC_OKIP, ResultSNE_OKIP)
  OIP[10,5] = sum(ResultAUR_LATE_PendLoad, ResultCOP_LATE_PendLoad, ResultDOU_LATE_PendLoad, 
                  ResultFLO_LATE_PendLoad,ResultLEB_LATE_PendLoad, ResultONT_LATE_PendLoad,
                  ResultPLO_LATE_PendLoad, ResultSDC_LATE_PendLoad, ResultSNE_LATE_PendLoad)
  OIP[10,6] = sum(ResultAUR_OKPendLoad, ResultCOP_OKPendLoad, ResultDOU_OKPendLoad, 
                  ResultFLO_OKPendLoad,ResultLEB_OKPendLoad, ResultONT_OKPendLoad,
                  ResultPLO_OKPendLoad, ResultSDC_OKPendLoad, ResultSNE_OKPendLoad)

  
  
#########################################
  ########### RP PROCESS KEY #############
  
TenS <- df4[which(df4$Stat==10),]
TenA <- as.array(TenS$Lines)
Ten <- sum(as.numeric(TenA))

EightyOneS <- df4[which(df4$Stat== 81),]
EightyOneA <- as.array(EightyOneS$Lines)
EightyOne <- sum(as.numeric(EightyOneA))

EightyS <- df4[which(df4$Stat== 80),]
EightyA <- as.array(EightyS$Lines)
Eighty <- sum(as.numeric(EightyA))

ZeroS <- df4[which(df4$Stat==0 & df4$Order == "INTE"),]
ZeroA <- as.array(ZeroS$Lines)
Zero <- sum(as.numeric(ZeroA))

  
write.table(OIP, file = "GPM_OIP.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")  


RPSTAT <- data.frame(matrix(ncol = 4, nrow = 4))
x <- c("RP", "STATUS KEY", "STATUS", "Balances")
colnames(RPSTAT) <- x

RPSTAT[1,1] <- 10
  RPSTAT[1,2] <- "PICKED/PACKED & PENDING LOAD"
  RPSTAT[1,3]<- Ten

RPSTAT[2,1] <- 81
  RPSTAT[2,2] <- "PICKING IN-PROCESS"
  RPSTAT[2,3] <- EightyOne
  
RPSTAT[3,1] <- 80
  RPSTAT[3,2] <- "ALLOCATED WAITING TO BEGIN PICK"
  RPSTAT[3,3] <- Eighty
  
RPSTAT[4,1] <- 0 
  RPSTAT[4,2] <- "PARTIALLY ALLOCATED"
  RPSTAT[4,3]<- Zero








