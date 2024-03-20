
#MAIN Analyses

pleaserun <- function(expvar, outvar, cov) { 
  
  form = as.formula(paste("datan[[outvar]] ~ datan[[expvar]] + offset(log(totalpop)) + ", paste(cov,  collapse = "+")))
  
  model <- glm.nb(form, data=datan)
  #####create an empty dataset, will save model output to it
  
  df_var = data.frame(matrix(ncol=7,nrow=1))
  #assign column names
  colnames(df_var)<-c(paste('exposure'),#1
                      paste('outcome'),#2
                      paste( "RR1"),#3
                      paste("LL1"), #4
                      paste("UL1"),#5
                      paste("deviance"),#6
                      paste("aic")#7
  )
  
  #output results of interest
  
  df_var[1]<-paste(expvar)
  df_var[2]<-paste(outvar)
  
  beta1<-(summary(model)$coefficients[2])
  df_var[3]<-exp(beta1)
  se1<-(summary(model)$coefficients[2,2])
  df_var[4]<-exp(beta1-(1.96*se1))
  df_var[5]<-exp(beta1+(1.96*se1))

  df_var[6]<-summary(model)$deviance
  df_var[7]<-summary(model)$aic
  
  return(df_var)

  
}



# Interaction models #

pleaserunint <- function(expvar, outvar, 
                         intvar,
                         reflevel, level1,
                         level2, level_desc, cov) 
{ 
  
  #####choose reference level 
  
  {d<-data[which(is.na(data[[intvar]]) == 0),] }  
  
  intvar4<-as.factor(ifelse(d[[intvar]]==reflevel,0,
                            ifelse(d[[intvar]]==level1,1,
                                   ifelse(d[[intvar]]==level2,2,NA))))
                                         # ifelse(d[[intvar]]==level3,3, NA)))))
  
 
  form = as.formula(paste("d[[outvar]] ~ d[[expvar]] + as.factor(intvar4)  + offset(log(totalpop)) + ", paste(cov,  collapse = "+")))
  
  main <- glm.nb(form, data=d)

  int_for=as.formula(paste("d[[outvar]] ~ d[[expvar]] + as.factor(intvar4)  +
                            d[[expvar]]:as.factor(intvar4) +
                           offset(log(totalpop)) +  ", paste(cov,  collapse = "+")))
  
  model <- glm.nb(int_for, data=d)
  
  #####create an empty dataset, will save model output to it
  
  df_var = data.frame(matrix(ncol=14,nrow=1))
  
  #assign column names
  colnames(df_var)<-c(paste('exposure'),#1
                      paste('outcome'),#2
                      paste('modifier'),#3
                      
                      paste( "RR1"),#4
                      paste("LL1"), #5
                      paste("UL1"),#6
                      
                      paste('RRR'),#7
                      paste('RRR_LL'),#8
                      paste('RRR_UL'),#9
                      
                      paste('deviance main'),#10
                      paste('aic main'),#11
                      paste('deviance int'),#12
                      paste('aic int'),#13
                      
                      paste('Modifier level'))#14
  
  df_var[1]<-paste(expvar)
  df_var[2]<-paste(outvar)
  df_var[3]<-paste(intvar)
  
  beta1<-(summary(model)$coefficients[2])
  df_var[4]<-exp(beta1)
  se1<-(summary(model)$coefficients[2,2])
  df_var[5]<-exp(beta1-(1.96*se1))
  df_var[6]<-exp(beta1+(1.96*se1))
  
  rrr<-(summary(model)$coefficients[10])
  df_var[7]<-exp(rrr)
  se_rrr<-(summary(model)$coefficients[10,2])
  df_var[8]<-exp(rrr-(1.96*se_rrr))
  df_var[9]<-exp(rrr+(1.96*se_rrr))
  
  df_var[10]<-summary(main)$deviance
  df_var[11]<-summary(main)$aic
  df_var[12]<-summary(model)$deviance
  df_var[13]<-summary(model)$aic
  
  df_var[14] <- level_desc
  
  return(df_var)
  
}