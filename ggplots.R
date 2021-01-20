ggplot(df65,aes(x=incent,fill=Termination),state="count")+
  geom_histogram(aes(y=(..count..)/sum(..count..),group=Termination),position="dodge",binwidth=.2)+theme_bw()+
  ggtitle("Termination and Refinancing incentive")+ylab("proportion of loans")
################################

ggplot(newpooling2001,aes(monthlyAge))
+xlab("")+theme_minimal()+ ylab("Conditional Prepayment Rate(CPR)")
+ggtitle("Monthly prepayment rate in our case") 
+geom_line(aes(y=ratio),colour="green",size=1)
+scale_x_date(date_labels = "%b/%Y", date_breaks = "1 year")
+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
   
  #geom_line(aes(y=PrepayPred*100),colour="green",size=1.5)+
  

  #############################################
base+geom_line(aes(y=ratio*100),colour="green",size=1)
+scale_x_date(date_labels = "%b/%y", date_breaks = "1 year")
+ylab("CPR")+theme_bw()+
  labs(title="percentage of prepayment per month in our study case")
############################################################
ggplot(df65,aes(x=loan_age,fill=Termination))+
  geom_histogram(aes(y=(..count..)/sum(..count..),group=Termination),
                 position="dodge",binwidth=.4)+theme_minimal()+
  ggtitle("Termination and Loan ageing influence")+
  ylab("proportion of loans")
############################################################
ggplot(pooling,aes(monthlyAge))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))+ scale_x_date(date_labels = "%b/%Y", date_breaks = "1 year")+geom_line(aes(y=ratio,colour="Actual"),size=1)+ylab("CPR")+
  geom_line(aes(y=PrepayPred,colour="model fit"))+ggtitle(" Model performance")
###########################
ggplot(df12, aes(incent2,residuals ))+geom_smooth(span=0.1)+theme_minimal()+xlab("Incentive Spread")+ggtitle(" S_Curve ")
 
  
  scale_colour_manual("model performance")
##################################################################

           #########----seasonality-----------###
  ggplot(seast,aes(x=month1,y=seas,group=1,color="red"))
  +geom_line(stat="identity")+geom_point(color="blue")
  +theme_minimal()
  +theme(legend.position="none",axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
  +ylab("Seasonality multiplier")+xlab("Month of Year")+ggtitle("Seasonality")
              ######################
  
  ####-------S_Curve-----------####
  ggplot(df12, aes(incent2,residuals ))
  +geom_smooth(span=0.1)+theme_minimal()
  +xlab("Incentive Spread")+ggtitle(" S_Curve ")
  
  #######----Fico-----------####
  ggplot(df654,aes(x=fico,fill=status),state="count")+
    +     geom_histogram(aes(y=(..count..)/sum(..count..),group=status),position=position_stack(reverse =T),binwidth=.8)+theme_minimal()+
    +     ggtitle("Termination  stutus and Fico score")+ylab("proportion of loans")
  ####-------------------############
  library(survival)
  library(survminer)
  df65_km <- survfit(Surv(loan_age,termin)~1,data=df65)
  
  ggsurvplot(df65_km,data=df65,conf.int = TRUE ,legend.title ="Prepaid",
             xlab ="Loan Age",ylab="Survival Probablity",legend.labs="Loan",
             ggtheme =theme_minimal())
  
  ggsurvplot(df65_km,data=df65,fun = "event",conf.int = TRUE ,
             legend.title ="Prepaid",xlab ="Loan Age",ylab=" Cumulative Survival",
             legend.labs="Loan",ggtheme =theme_minimal())
################
  ggplot(pooling65,aes(monthlyAge))+xlab("")+ theme_minimal()+ ylab("CPR")+ggtitle("Financial incentive VS CPR ")+geom_line(aes(y=ratio*100,colour="CPR"),size=1)+geom_line(aes(y=Inc,colour="Incentive"),size=1)+scale_x_date(date_labels = "%b/%Y", date_breaks = "1 year")+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
  
  df654$loan_agecat1<-cut(df654$loan_age, c(0,12,36,60,108,178)) 
  ######------------------ loan age categorical -------------#######
  
  ggplot(df654[!is.na(df654$loan_agecat1), ],aes(x=loan_agecat1,fill=status))+
    geom_bar(position="dodge",aes(y = (..count..)/sum(..count..)))+
    theme_minimal()+ylab("proportion of loans")+
    scale_y_continuous(labels = scales::percent)+
    xlab("Loan age category")
  
###
  
  ggplot(df654[!is.na(df654$loan_agecat1), ],aes(x=loan_agecat1,fill=Termination))+
    geom_bar(position="dodge",aes(y = (..count..)/sum(..count..)))+theme_minimal()+
    ylab("proportion of loans")+scale_y_continuous(labels = scales::percent)+
    xlab("Loan age category")
   ###### states bar plots
  ggplot(df654[order(df654$st,decreasing =F),],aes(x=st,fill=status))+ geom_bar(position=position_stack(),aes(y = (..count..)/sum(..count..)))+theme_minimal()+coord_flip() +ylab("portioned disturbutions per states")+scale_y_continuous(labels = scales::percent)+xlab("states in the U.S.")
  
  ##### ------------- SATO -------------  #########
  
  ggplot(df654,aes(x=sato,fill=status),state="count")+
    +     geom_histogram(aes(y=(..count..)/sum(..count..),group=status),position="dodge",binwidth=.2)+theme_minimal()+
    +     ggtitle("Termination status and SATO")+ylab("proportion of loans")
  
  ##########