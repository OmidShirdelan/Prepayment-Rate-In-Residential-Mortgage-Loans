Ratefinder<- function(date1)
{
  x <- as.character(date1)
  mm=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
  h<- subset(newRate2000,month==mm & Year==y)
  #D <- d%/%7
  Interest.Rate <-  mean(h$Rate)
  return(Interest.Rate)
}
#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,function(x) mean(subset(newRate,month==sorted2000$month & Year==sorted2000$Year) )
#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,function(x) mean(subset(newRate,month==sorted2000$month & Year==sorted2000$Year) ))                              
#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,Ratefinder)
#subset(newHPI2000,newHPI2000$yy==2000 & newHPI2000$mm==1,select=WY)
###----------HPI
HPIfinder<- function(date,state1)
{
  #names(newHPI2000)[names(newHPI2000)=="ORG"] <- "OR"
  # names(newHPI2000)[names(newHPI2000)=="IND"] <- "IN"
  x <- sprintf("%s25",date)#forconverting the date without day to use in function#
  m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
   if(state1=="PR"|state1=="VI"|state1=="GU" ){
     HPI=NA
   } else {
  hh<- subset(HPI99,HPI99$year==y & HPI99$month==m,select=state1)
  #D <- d%/%7
  HPI <- as.numeric(hh)}
  return(HPI)
}
#  Unstate2000 <- subset(Unstate,UnState$Year>1999)
#sorted2000$hpi <- mapply(HPIfinder,sorted2000$svcg_Date,sorted2000$st) 

#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,function(x) mean(subset(newRate,month==sorted2000$month & Year==sorted2000$Year) )
#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,function(x) mean(subset(newRate,month==sorted2000$month & Year==sorted2000$Year) ))                              
#sorted2000$MoRate30 <- sapply(sorted2000$svcg_Date,Ratefinder)
#subset(newHPI2000,newHPI2000$yy==2000 & newHPI2000$mm==1,select=WY)
#--------------Unemployment
#names(newHPI2000)[names(newHPI2000)=="ORG"] <- "OR"
Unemp992<- function(date,state1)
{
  x <- sprintf("%s25",date)#forconverting the date without day to use in function#
  m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
  if(state1=="PR"|state1=="VI"|state1=="GU" ){
          unem=NA
       } else {
  hh<- subset(unemp99lag2,unemp99lag2$yrs==y & unemp99lag2$month==m,select=state1)
  #D <- d%/%7
  unem<- as.numeric(hh)}
  return(unem)
}
###---
unemp991<- function(date,state1)
{
  x <- sprintf("%s25",date)#forconverting the date without day to use in function#
  m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
  if(state1=="PR"|state1=="VI"|state1=="GU" ){
    unem=NA
  } else {
    hh<- subset(unemp99lag1,unemp99lag1$yrs==y & unemp99lag1$month==m,select=state1)
    #D <- d%/%7
    unem<- as.numeric(hh)}
  return(unem)
}
# ---  sorted2000$Unemploy <- mapply(HPIfinder,sorted2000$svcg_Date,sorted2000$st)
# --- Ratefinder for house stock ---#
Ratefinder<- function(date1)
   {
         x <- as.character(date1)
         mm=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
         #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
           y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
           houseStock<- subset(HouseStock2000,month==mm & Year==y)
           #D <- d%/%7
            # houseStock <-  mean(h$Rate)
             return(houseStock$StockRate)
}
### ----  HOUSING tURNOVER --- ###
turnover$yrs=as.numeric(format(as.Date(turnover$Date,format="%m/%d/%Y"),"%Y"))
turnover$mon=as.numeric(format(as.Date(turnover$Date,format="%m/%d/%Y"),"%m"))

turnoverfinder<- function(date1)
{
  x <- as.character(date1)
  mm=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
  trnovr<- subset(turnover,mon==mm & yrs==y )
  #D <- d%/%7
  # houseStock <-  mean(h$Rate)
  return(trnovr$turnover)
}

sorted2000$turnover <- sapply(sorted2000$svcg_Date,turnoverfinder)
### ----------- 
swapRt10TB2yrs <- read.csv("E:\\Rates\\swap 10-2yrs.csv",header=T)
swapfinder<- function(date1)
{
  x <- as.character(date1)
  mm=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
  #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
  y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
  trnovr<- subset(swapRt10TB2yrs,mon==mm & yrs==y )
  #D <- d%/%7
  # houseStock <-  mean(h$Rate)
  return(trnovr$swap)
}
sorted51$swap <- sapply(sorted51$svcg_Date,swapfinder)
ficoFndr<- function(date,ID,FIC0)
{ 
  if(is.na(FIC0)){
    
    x <- sprintf("%s25",date)
    m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
    #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
    y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
    
    ffico<- complimputedSam15[grep(ID,complimputedSam15$id_loan),]
    fic<- subset(ffico,
                 ffico$yrs==y & ffico$mth==m ,select="fico")}
  else {
    fic<- FIC0}
  
  #D <- d%/%7
  # fic <- subset(ff,which(grep(ID,complimputedSam15$id_loan))
  return(as.numeric(fic))
}
mapply(ficoFndr,sam15$svcg_cycle,sam15$id_loan,sam15$fico)
var(as.numeric(sam15$fico),na.rm = T)
cor(sam15$fico,ffic0,use  =c( "all.obs",
                              "complete.obs", "na.or.complete", "pairwise.complete.obs"))
####

dtiFndr<- function(date,ID,dti0)
{ 
  if(is.na(dti0)){
    
    x <- sprintf("%s25",date)
    m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
    #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
    y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
    
    fdti<- complimputedSam15[grep(ID,complimputedSam15$id_loan),]
    dti<- subset(fdti,
                 fdti$yrs==y & fdti$mth==m ,select="dti")}
  else {
    dti<- dti0}
  
  #D <- d%/%7
  # fic <- subset(ff,which(grep(ID,complimputedSam15$id_loan))
  return(as.numeric(dti))
}
mapply(dtiFndr,sam15$svcg_cycle,sam15$id_loan,sam15$dti)
var(as.numeric(sam15$dti),na.rm = T)
cor(sam15$fico,ffic0,use  =c( "all.obs",
                              "complete.obs", "na.or.complete", "pairwise.complete.obs"))

cnt_borFndr<- function(date,ID,cnt_bor)
{ 
  if(is.na(cnt_bor)){
    
    x <- sprintf("%s25",date)
    m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
    #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
    y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
    
    fcnt_bor<- complimputedSam15[grep(ID,complimputedSam15$id_loan),]
    cnt_bor<- subset(fcnt_bor,
                     fcnt_bor$yrs==y & fcnt_bor$mth==m ,select="cnt_borr")}
  else {
    cnt_bor<- cnt_bor}
  
  #D <- d%/%7
  # fic <- subset(ff,which(grep(ID,complimputedSam15$id_loan))
  return(as.numeric(cnt_bor))
}
sam15$cnt_borr2<- mapply(cnt_borFndr,sam15$svcg_cycle,sam15$id_loan,sam15$cnt_borr)
var(as.numeric(sam15$cnt_borr),na.rm = T)
#cor(sam15$cnt_borr,sam15$cnt_borr2,use  =c( "all.obs",
#      "complete.obs", "na.or.complete", "pairwise.complete.obs"))

###

swap2Fndr<- function(date,ID,swap2)
{ 
  if(is.na(swap2)){
    
    x <- sprintf("%s25",date)
    m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
    #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
    y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
    
    fswap2<- complimputedSam15[grep(ID,complimputedSam15$id_loan),]
    swap2<- subset(fswap2,
                   fswap2$yrs==y & fswap2$mth==m ,select="swap2")}
  else {
    swap2<- swap2}
  
  #D <- d%/%7
  # fic <- subset(ff,which(grep(ID,complimputedSam15$id_loan))
  return(as.numeric(swap2))
}
sam15$swap2<- mapply(swap2Fndr,sam15$svcg_cycle,sam15$id_loan,sam15$swap2)
var(as.numeric(sam15$swap2),na.rm = T)
# cor(sam15$cnt_borr,sam15$swap22,use  =c( "all.obs",
#                                             "complete.obs", "na.or.complete", "pairwise.complete.obs"))
# 

###
swap10Fndr<- function(date,ID,swap210)
{ 
  if(is.na(swap210)){
    
    x <- sprintf("%s25",date)
    m=as.numeric(format(as.Date(x,format="%Y%m%d"),"%m"))
    #d=as.numeric(format(as.Date(x,format="%Y%m%d"),"%d"))
    y=as.numeric(format(as.Date(x,format="%Y%m%d"),"%Y"))
    
    fswap10<- complimputedSam15[grep(ID,complimputedSam15$id_loan),]
    swap21<- subset(fswap210,
                    fswap210$yrs==y & fswap210$mth==m ,select="swap10")}
  else {
    swap21<- swap210}
  
  #D <- d%/%7
  # fic <- subset(ff,which(grep(ID,complimputedSam15$id_loan))
  return(as.numeric(swap21))
}
sam15$swap102<- mapply(swap2Fndr,sam15$svcg_cycle,sam15$id_loan,sam15$swap10)
var(as.numeric(sam15$swap10),na.rm = T)
