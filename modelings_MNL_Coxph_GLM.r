 library(survival)
 library(nnet)
 library(splines)
 library(ggplot2)
 ### Survival Anlysis
coxph10<- coxph( Surv(AGE, termin ) ~ smooth(incent2) + smooth(current_upb2) + 
                   smooth(loan_age) + smooth(current_int_rt) + smooth(fico1) + 
                   smooth(cnt_units) + smooth(cltv) + smooth(unemp2) + smooth(ltv) + 
                   smooth(cnt_borr2) + smooth(turnover) + smooth((loan_age)^2) + 
                   smooth(Curr_LTV) + smooth(dti2) + smooth(sato), data = sample10)

  mod <-coxph( Surv(loan_age, cd_zero_bal ) ~ smooth(incent2) + smooth(current_upb2) + 
                          smooth(loan_age) + smooth(current_int_rt) + smooth(fico1) + 
                          smooth(cnt_units) + smooth(cltv) + smooth(unemp2) + smooth(ltv) + 
                          smooth(cnt_borr2) + smooth(turnover) + smooth((loan_age)^2) + 
                          smooth(Curr_LTV) + smooth((dti2)) + smooth((sato)), data = Northeast361)
               
                  ################################
  
  mod <- glm(termin ~ smooth(incent)+
             smooth(loan_age)+smooth(Curr_LTV)+
             smooth(sato)+smooth(fico1)+
             smooth(cnt_units)+smooth(cltv)+
             smooth(unemp2)+smooth(ltv)+
             smooth(cnt_borr2),family=binomial, Northeast362)
  
   ######################
  thresh <- seq(0.0000,0.1,0.00001)
  Sensitivity <- numeric(length(thresh))
  Specificity <- numeric(length(thresh))
  for (j in seq(along=thresh)){
    pp <- ifelse(Northeast362$linpre<thresh[j],"0","1")
    xx <- xtabs(~termin+pp,Northeast362)
    Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
    #Sensitivity[j] <- xx[2,2]/(xx[2,1]+xx[2,2])
  }
             ###--------------------Multinomial Logistic 
 
  #Multinomial Logistic regression
  
  df1$NSPF <- factor(df1$termin)
  df1$out<-relevel(df1$NSPF, ref="1")
  mymodel<-multinom(out~ smooth(incent)+
                      smooth(loan_age)+smooth(Curr_LTV)+
                      smooth(sato)+smooth(fico1)+
                      smooth(cnt_units)+smooth(cltv)+
                      smooth(unemp2)+smooth(ltv)+
                      smooth(cnt_borr2), data=df1)
  summary(mymodel)
  
   ####------------------Out of sample model performance
  
  sam15$curr_upbTotal <- sam15$current_upb2+sam15$current_upbPre
  sam15$predictMNReg <- predict(mymodel,sam15)
  sam15$current_upbPred <- as.numeric(sam15$predictMNReg)*sam15$curr_upbTotal ;summary(sam15$current_upbPred)
     ###
  pooling$PrepayPred<- tapply(newsam15$current_upbPrepmn,
                         +        newsam15$AGE, FUN=sum)/tapply(newsam15$current_upb2,
                                 newsam15$AGE, FUN=sum)
  newpooling<- pooling[which(pooling$MNPre<0.1),]
  ggplot(newpooling,aes(monthlyAge))+geom_line(aes(y=ratio),colour="blue")+geom_line(aes(y=MNPre),colour="red")
  ggplot(pooling,aes(monthlyAge))
                  +geom_line(aes(y=ratio),colour="green")+geom_line(aes(y=MNPre),colour="red")
      ########
  ggplot(newpooling,aes(monthlyAge))+
    geom_line(aes(y=ratio),colour="blue",size=2)+
    geom_line(aes(y=MNPre),colour="green",size=1.5,linetype = 2)
  
  #####
  prepay1<- tapply(sam15$current_upbPre,
                   sam15$AGE,
                   FUN=sum)/tapply(sam15$current_upb2,FUN=sum)
                                  
  
  plot(1:194,prepay1,type="l",col="green") 
  lines(1:194,MNReg,col="red")
                                 #2-tailed z test
  z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
  #p <- (1 - pnorm(abs(z), 0, 1)) * 2
  #p
  attach(newsam15)
  newsam15$current_upbPrepmn<- ifelse(!Predict==0,
      +  orig_upb-((1-(1+(current_int_rt/1200))^loan_age)/(1-(1+
      (current_int_rt/1200))^mths_remng))*orig_upb,0)
  
  cm <- table(predict(mymodel),df1$NSPF) ;cm
  df1$current_upbTotal <- ifelse(df1$current_upbTotal <0,0,df1$current_upbTotal)
  df12 <- subset(df1,select=c('id_loan','loan_age','predict'))
   sam15Pred<- left_join(sam15,df12, by = c("id_loan" = "id_loan", "loan_age" = "loan_age"))
   rm(df12)
       #################### Termination variable
   termin3 <- function(code){
     if (is.na(code)){
       return (0)
     }
     else if (code==01){
       return(1)
     }
     else if (code==03 | code==06 |code==09 ){
       return (2)
     }
     
     else {
       return(NA)
       
     }
     
     
   }
   
   finesam14$termin1 <- sapply(finesam14$cd_zero_bal,termin3)
   df65$termin1 <- sapply(df65$cd_zero_bal,termin3)
   df65$termin <- ifelse(is.na(df65$cd_zero_bal),0,1)
  ######################### 
   

   df1$NSPF <- factor(df1$termin)
   df1$out<-relevel(df1$NSPF, ref="1")
   mymodel<-multinom(out~ smooth(incent)+ns(loan_age,knots=c(12,36,72,192))+
                       smooth(Curr_LTV)+
                       #smooth(curr_upbTotal)+
                     smooth(sato)+smooth(fico1)+smooth(channelC)+channelR+channelT+
                       stCross+monthCross+
                       smooth(unemp2)+smooth(ltv) +smooth(orig_upb)
                       , data=df1); 
   cm <- table(predict(mymodel),df1$NSPF)
  ( cm[1,2]+cm[2,1])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1]);summary(mymodel)
  ############################################### 
   
                                             ##### Ploting Results
   df1$predict <- predict(mymodel)
   df12 <- subset(df1,select=c('id_loan','loan_age','predict'))
   newsam15<- left_join(sam15,df12, by = c("id_loan" = "id_loan", "loan_age" = "loan_age"))
    summary(newsam15);rm(df12)
    newsam15$current_upbT <- newsam15$current_upb2+newsam15$current_upbPre
    newsam15$predict[is.na(newsam15$predict)] <- 0
    #newpooling<- pooling[which(pooling$MNPre<0.1),]
    newsam15$predict1<- as.integer(newsam15$predict)
    newsam15$predict1[newsam15$predict1==2] <- 0
    newsam15$Prepay_Predict <- newsam15$predict1*newsam15$current_upbT
   pooling$PrepayPred<- tapply(newsam15$Prepay_Predict,
                               +newsam15$AGE, FUN=sum)/tapply(newsam15$current_upb2,
                                newsam15$AGE, FUN=sum)
   # P-values--------------------#############
   z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
   (p <- (1 - pnorm(abs(z), 0, 1)) * 2)
  