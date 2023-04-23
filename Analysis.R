# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
library(caret)
library(car)
library(cmprsk)
library(dplyr)
library(foreign)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(lava)
library(Matching)
library(mediation)
library(mice)
library(pec)
#install.packages("poLCA", dependencies = TRUE)
library(poLCA)
library(plyr)
library(prodlim)
library(reshape2)
library(rms)
library(riskRegression)
library(survey)
library(scales)
library(survminer)
library(survival)
library(splines)
library(timeROC)
library(tableone)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
Interpolation_weighted$HEI[Interpolation_weighted$HEI=="Quintile 1"|Interpolation_weighted$HEI=="Quintile 2"]<-"YES"
Interpolation_weighted$HEI[Interpolation_weighted$HEI=="Quintile 3"|Interpolation_weighted$HEI=="Quintile 4"||Interpolation_weighted$HEI=="Quintile 5"]<-"NO"
# >>>>> Section 16. Multiple interpolation <<<<< ####
{#* section 16.1 Multiple interpolation ####
  load(file="./data_eCM/Characters.Rdata")
  mice_data<-Characters[,10:30]
  #methods(mice)
  mice::md.pattern(mice_data)
  sapply(data, function(x) sum(is.na(mice_data)))
  miss <- function(x){sum(is.na(x))/length(x)*100}
  apply(mice_data,2,miss)
  init = mice(mice_data,maxit= 10, m=10,seed = 500) 
  stripplot(init)
  save(init,file="./data_eCM/mice_data.Rdata")
  load(file="./data_eCM/mice_data.Rdata")
  Interpolation_data <- cbind(Characters[,1:9],complete(init,10))
  load(file="./data_eCM/Characters.Rdata")
  Original_data<-Characters
  save(Original_data,file="./data_eCM/Original_data.Rdata")
  rownames(Original_data)<-Original_data$ID
  Original_data$ID<-NULL
  Complete_dat<-na.omit(Characters[,10:30])
  Complete_data<-cbind(Characters[rownames(Complete_dat),1:9],Complete_dat)
  save(Complete_data,file="./data_eCM/Complete_data.Rdata")
  save(Interpolation_data,file="./data_eCM/Interpolation_data.Rdata")
  
}
{#* section 16.2 Data restoration ####
  load(file="./data_eCM/Complete_data.Rdata")
  load(file="./data_eCM/Original_data.Rdata")
  load(file="./data_eCM/Interpolation_data.Rdata")
  {#** section 16.2.2 Original data ####
    #AGE
    table(Original_data$Age_status)
    Original_data$Age_status[Original_data$Age<45]<-"<45"
    Original_data$Age_status[Original_data$Age>=45&Original_data$Age<65]<-"[45,65)"
    Original_data$Age_status[Original_data$Age>=65]<-">=65"
    Original_data$Age_status<-factor(Original_data$Age_status,
                                levels = c("<45","[45,65)",">=65"))
    table(Original_data$Age_status)
    
    #Gender
    table(Original_data$Gender)
    Original_data$Gender<-factor(Original_data$Gender,
                            levels = c("Male","Female"))
    table(Original_data$Gender)
    
    #Race_ethnicity
    table(Original_data$Race_ethnicity)
    Original_data$Race_ethnicity<-factor(Original_data$Race_ethnicity,
                                    levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other_Race")) 
    table(Original_data$Race_ethnicity)
    
    #Education_levels
    table(Original_data$Education_levels)
    Original_data$Education_levels<-factor(Original_data$Education_levels,
                                      levels = c("Less_than_high_school","High_school_or_Equivalent","College_or_above")) 
    table(Original_data$Education_levels)
    
    # PIR 
    table(Original_data$PIR)
    Original_data$PIR<-factor(Original_data$PIR,
                         levels = c("(0, 1]","(1,4)","[4,inf)")) 
    table(Original_data$PIR)
    
    #Health_insurance
    table(Original_data$Health_insurance)
    Original_data$Health_insurance<-factor(Original_data$Health_insurance,
                                      levels = c("No_insurance","Public_insurance","Private_insurance")) 
    table(Original_data$Health_insurance)
    
    #SEI
    table(Original_data$SEI)
    Original_data$SEI<-factor(Original_data$SEI,
                                       levels = c("Unemployment","Lower","Upper")) 
    table(Original_data$SEI)
    
    #Smoking_status
    table(Original_data$Smoking_status)
    Original_data$Smoking_status<-factor(Original_data$Smoking_status,
                                    levels = c("Never_smoker","Former_smoker","Current_smoker")) 
    table(Original_data$Smoking_status)
    
    #Drinking_status
    table(Original_data$Drinking_status)
    Original_data$Drinking_status<-factor(Original_data$Drinking_status,
                                     levels = c("Nondrinker","Light/moderate_drinker","Heavier_drinker")) 
    table(Original_data$Drinking_status)
    
    #Physical_status
    table(Original_data$Physical_status)
    Original_data$Physical_status<-factor(Original_data$Physical_status,
                                     levels = c("Inactive","Insufficient","Recommended")) 
    table(Original_data$Physical_status)
    
    #HEI
    table(Original_data$HEI)
    Original_data$HEI<-factor(Original_data$HEI,
                         levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
    table(Original_data$HEI)
  
    #BMI_status
    table(Original_data$BMI_status)
    Original_data$BMI_status[Original_data$BMI<25]<-'(0,25)'
    Original_data$BMI_status[Original_data$BMI>=25&Original_data$BMI<30]<-'[25.0-30)'
    Original_data$BMI_status[Original_data$BMI>=30]<-'[30,inf)' 
    Original_data$BMI_status<-factor(Original_data$BMI_status,
                               levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Original_data$BMI_status)
    
    #T2D_status
    table(Original_data$T2D_status)
    Original_data$T2D_status<-factor(Original_data$T2D_status,
                                levels = c("No T2D","T2D")) 
    table(Original_data$T2D_status)
    
    #Cancer_status
    table(Original_data$Cancer_status)
    Original_data$Cancer_status<-factor(Original_data$Cancer_status,
                                   levels = c("NO","YES")) 
    table(Original_data$Cancer_status)
    Original_weighted<-Original_data
    save(Original_weighted,file="./data_eCM/Original_weighted.Rdata")
  }
  {#** section 16.2.1 Interpolation data ####
     #AGE
     table(Interpolation_data$Age_status)
     Interpolation_data$Age_status[Interpolation_data$Age<45]<-"<45"
     Interpolation_data$Age_status[Interpolation_data$Age>=45&Interpolation_data$Age<65]<-"[45,65)"
     Interpolation_data$Age_status[Interpolation_data$Age>=65]<-">=65"
     Interpolation_data$Age_status<-factor(Interpolation_data$Age_status,
                                      levels = c("<45","[45,65)",">=65"))
     table(Interpolation_data$Age_status)
     
     #Gender
     table(Interpolation_data$Gender)
     Interpolation_data$Gender<-factor(Interpolation_data$Gender,
                                  levels = c("Male","Female"))
     table(Interpolation_data$Gender)
     
     #Race_ethnicity
     table(Interpolation_data$Race_ethnicity)
     Interpolation_data$Race_ethnicity<-factor(Interpolation_data$Race_ethnicity,
                                          levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other_Race")) 
     table(Interpolation_data$Race_ethnicity)
     
     #Education_levels
     table(Interpolation_data$Education_levels)
     Interpolation_data$Education_levels<-factor(Interpolation_data$Education_levels,
                                            levels = c("Less_than_high_school","High_school_or_Equivalent","College_or_above")) 
     table(Interpolation_data$Education_levels)
     
     # PIR 
     table(Interpolation_data$PIR)
     Interpolation_data$PIR<-factor(Interpolation_data$PIR,
                               levels = c("(0, 1]","(1,4)","[4,inf)")) 
     table(Interpolation_data$PIR)
     
     #Health_insurance
     table(Interpolation_data$Health_insurance)
     Interpolation_data$Health_insurance<-factor(Interpolation_data$Health_insurance,
                                            levels = c("No_insurance","Public_insurance","Private_insurance")) 
     table(Interpolation_data$Health_insurance)
     
     #SEI
     table(Interpolation_data$SEI)
     Interpolation_data$SEI<-factor(Interpolation_data$SEI,
                               levels = c("Unemployment","Lower","Upper")) 
     table(Interpolation_data$SEI)
     
     #Smoking_status
     table(Interpolation_data$Smoking_status)
     Interpolation_data$Smoking_status<-factor(Interpolation_data$Smoking_status,
                                          levels = c("Never_smoker","Former_smoker","Current_smoker")) 
     table(Interpolation_data$Smoking_status)
     
     #Drinking_status
     table(Interpolation_data$Drinking_status)
     Interpolation_data$Drinking_status<-factor(Interpolation_data$Drinking_status,
                                           levels = c("Nondrinker","Light/moderate_drinker","Heavier_drinker")) 
     table(Interpolation_data$Drinking_status)
     
     #Physical_status
     table(Interpolation_data$Physical_status)
     Interpolation_data$Physical_status<-factor(Interpolation_data$Physical_status,
                                           levels = c("Inactive","Insufficient","Recommended")) 
     table(Interpolation_data$Physical_status)
     
     #HEI
     table(Interpolation_data$HEI)
     Interpolation_data$HEI<-factor(Interpolation_data$HEI,
                               levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
     table(Interpolation_data$HEI)
     
     #BMI_status
     table(Interpolation_data$BMI_status)
     Interpolation_data$BMI_status[Interpolation_data$BMI<25]<-'(0,25)'
     Interpolation_data$BMI_status[Interpolation_data$BMI>=25&Interpolation_data$BMI<30]<-'[25.0-30)'
     Interpolation_data$BMI_status[Interpolation_data$BMI>=30]<-'[30,inf)' 
     Interpolation_data$BMI_status<-factor(Interpolation_data$BMI_status,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
     table(Interpolation_data$BMI_status)
     
     #T2D_status
     table(Interpolation_data$T2D_status)

     table(Interpolation_data$T2D_status)
     
     #Cancer_status
     table(Interpolation_data$Cancer_status)
     Interpolation_data$Cancer_status<-factor(Interpolation_data$Cancer_status,
                                         levels = c("NO","YES")) 
     table(Interpolation_data$Cancer_status)
     Interpolation_weighted<-Interpolation_data
     save(Interpolation_weighted,file="./data_eCM/Interpolation_weighted.Rdata")
  }
  {#** section 16.2.2 Complete data ####
    #AGE
    table(Complete_data$Age_status)
    Complete_data$Age_status[Complete_data$Age<45]<-"<45"
    Complete_data$Age_status[Complete_data$Age>=45&Complete_data$Age<65]<-"[45,65)"
    Complete_data$Age_status[Complete_data$Age>=65]<-">=65"
    Complete_data$Age_status<-factor(Complete_data$Age_status,
                                          levels = c("<45","[45,65)",">=65"))
    table(Complete_data$Age_status)
    
    #Gender
    table(Complete_data$Gender)
    Complete_data$Gender<-factor(Complete_data$Gender,
                                      levels = c("Male","Female"))
    table(Complete_data$Gender)
    
    #Race_ethnicity
    table(Complete_data$Race_ethnicity)
    Complete_data$Race_ethnicity<-factor(Complete_data$Race_ethnicity,
                                              levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other_Race")) 
    table(Complete_data$Race_ethnicity)
    
    #Education_levels
    table(Complete_data$Education_levels)
    Complete_data$Education_levels<-factor(Complete_data$Education_levels,
                                                levels = c("Less_than_high_school","High_school_or_Equivalent","College_or_above")) 
    table(Complete_data$Education_levels)
    
    # PIR 
    table(Complete_data$PIR)
    Complete_data$PIR<-factor(Complete_data$PIR,
                                   levels = c("(0, 1]","(1,4)","[4,inf)")) 
    table(Complete_data$PIR)
    
    #Health_insurance
    table(Complete_data$Health_insurance)
    Complete_data$Health_insurance<-factor(Complete_data$Health_insurance,
                                                levels = c("No_insurance","Public_insurance","Private_insurance")) 
    table(Complete_data$Health_insurance)
    
    #SEI
    table(Complete_data$SEI)
    Complete_data$SEI<-factor(Complete_data$SEI,
                                   levels = c("Unemployment","Lower","Upper")) 
    table(Complete_data$SEI)
    
    #Smoking_status
    table(Complete_data$Smoking_status)
    Complete_data$Smoking_status<-factor(Complete_data$Smoking_status,
                                              levels = c("Never_smoker","Former_smoker","Current_smoker")) 
    table(Complete_data$Smoking_status)
    
    #Drinking_status
    table(Complete_data$Drinking_status)
    Complete_data$Drinking_status<-factor(Complete_data$Drinking_status,
                                               levels = c("Nondrinker","Light/moderate_drinker","Heavier_drinker")) 
    table(Complete_data$Drinking_status)
    
    #Physical_status
    table(Complete_data$Physical_status)
    Complete_data$Physical_status<-factor(Complete_data$Physical_status,
                                               levels = c("Inactive","Insufficient","Recommended")) 
    table(Complete_data$Physical_status)
    
    #HEI
    table(Complete_data$HEI)
    Complete_data$HEI<-factor(Complete_data$HEI,
                                   levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
    table(Complete_data$HEI)
    
    #BMI_status
    table(Complete_data$BMI_status)
    Complete_data$BMI_status[Complete_data$BMI<25]<-'(0,25)'
    Complete_data$BMI_status[Complete_data$BMI>=25&Complete_data$BMI<30]<-'[25.0-30)'
    Complete_data$BMI_status[Complete_data$BMI>=30]<-'[30,inf)' 
    Complete_data$BMI_status<-factor(Complete_data$BMI_status,
                                         levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Complete_data$BMI_status)
    
 
    
    #Cancer_status
    table(Complete_data$Cancer_status)
    Complete_data$Cancer_status<-factor(Complete_data$Cancer_status,
                                             levels = c("NO","YES")) 
    table(Complete_data$Cancer_status)
    Complete_weighted<-Complete_data
    save(Complete_weighted,file="./data_eCM/Complete_weighted.Rdata")
  }

}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 17 Processed based on reviewer comments ####
  load(file="./data_eCM/Original_weighted.Rdata")
  load(file="./data_eCM/Interpolation_weighted.Rdata")
  load(file="./data_eCM/Complete_weighted.Rdata")
  Interpolation_weighted$MORT_stat<-as.character(Interpolation_weighted$MORT_stat)
  Interpolation_weighted$MORT_stat[Interpolation_weighted$MORT_stat=="Alive"]<-0
  Interpolation_weighted$MORT_stat[Interpolation_weighted$MORT_stat=="Deceased"]<-1
  Interpolation_weighted$MORT_stat<-as.factor(Interpolation_weighted$MORT_stat)
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading=="CVD"]<-1
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading!="CVD"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading=="Cancer"]<-1
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading!="Cancer"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$ucod_leading=="DM"]<-1
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$ucod_leading!="DM"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Original_weighted$MORT_stat<-as.character(Original_weighted$MORT_stat)
  Original_weighted$MORT_stat[Original_weighted$MORT_stat=="Alive"]<-0
  Original_weighted$MORT_stat[Original_weighted$MORT_stat=="Deceased"]<-1
  Original_weighted$CVD_MORT_stat[Original_weighted$ucod_leading=="CVD"]<-1
  Original_weighted$CVD_MORT_stat[Original_weighted$ucod_leading!="CVD"&Original_weighted$MORT_stat==1]<-2
  Original_weighted$CVD_MORT_stat[Original_weighted$MORT_stat==0]<-0
  
  Original_weighted$Cancer_MORT_stat[Original_weighted$ucod_leading=="Cancer"]<-1
  Original_weighted$Cancer_MORT_stat[Original_weighted$ucod_leading!="Cancer"&Original_weighted$MORT_stat==1]<-2
  Original_weighted$Cancer_MORT_stat[Original_weighted$MORT_stat==0]<-0
  
  Original_weighted$DM_MORT_stat[Original_weighted$ucod_leading=="DM"]<-1
  Original_weighted$DM_MORT_stat[Original_weighted$ucod_leading!="DM"&Original_weighted$MORT_stat==1]<-2
  Original_weighted$DM_MORT_stat[Original_weighted$MORT_stat==0]<-0
  
  Complete_weighted$MORT_stat<-as.character(Complete_weighted$MORT_stat)
  Complete_weighted$MORT_stat[Complete_weighted$MORT_stat=="Alive"]<-0
  Complete_weighted$MORT_stat[Complete_weighted$MORT_stat=="Deceased"]<-1
  Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading=="CVD"]<-1
  Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading!="CVD"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$CVD_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading=="Cancer"]<-1
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading!="Cancer"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  Complete_weighted$DM_MORT_stat[Complete_weighted$ucod_leading=="DM"]<-1
  Complete_weighted$DM_MORT_stat[Complete_weighted$ucod_leading!="DM"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$DM_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  
  save(Original_weighted,file="./data_eCM/Original_weighted.Rdata")
  save(Interpolation_weighted,file="./data_eCM/Interpolation_weighted.Rdata")
  save(Complete_weighted,file="./data_eCM/Complete_weighted.Rdata")

  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 18 latent class analysis (Figure S3) ####
  load(file="./data_eCM/Original_weighted.Rdata")
  load(file="./data_eCM/Interpolation_weighted.Rdata")
  load(file="./data_eCM/Complete_weighted.Rdata")
  Interpolation_SES<-Interpolation_weighted[,c("Education_levels","PIR","Health_insurance","SEI")]
  Interpolation_SES$Education_levels<-as.numeric(Interpolation_SES$Education_levels)
  Interpolation_SES$PIR<-as.numeric(Interpolation_SES$PIR)
  Interpolation_SES$Health_insurance<-as.numeric(Interpolation_SES$Health_insurance)
  Interpolation_SES$SEI<-as.numeric(Interpolation_SES$SEI)
  set.seed(0)
  #M1 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 1, maxiter = 10000, nrep = 10, graph = TRUE)
  #M2 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 2, maxiter = 10000, nrep = 10, graph = TRUE)
  M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
  #M4 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 4, maxiter = 10000, nrep = 10, graph = TRUE)
 # M5 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 5, maxiter = 10000, nrep = 10, graph = F)
  Aic_bic<-as.data.frame(rbind(c(M1$aic,M1$bic),c(M2$aic,M2$bic),c(M3$aic,M3$bic),c(M4$aic,M4$bic),c(M5$aic,M5$bic)))
  colnames(Aic_bic)<-c("AIC","BIC")
  rownames(Aic_bic)<-c("nclass = 1","nclass = 2","nclass = 3","nclass = 4","nclass = 5")
  TableS1<-Aic_bic
  posterior <- data.frame(M3$posterior)
  posterior$label <- rownames(Interpolation_SES)
  posterior$class <- as.character(M3$predclass)
  write.table(TableS1,sep = ",",file ="./data_eCM/TableS1.csv")
  names(posterior)[1:2] <- c('class1_probabilities', 'class2_probabilities')
  Figure_S3A<-ggplot(posterior,max.overlaps = Inf) +
  geom_point(aes(class1_probabilities, class2_probabilities, color = class),size=2.5,alpha=0.5) +
  theme_bw()+  scale_color_nejm()+ scale_fill_nejm()+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  ggsave("./data_eCM/Figure_S3A.pdf",Figure_S3A, device = "pdf",width = 8, height = 6, units ="in",
         dpi = 300, limitsize = TRUE)
  #M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
  M3_probs <- melt(M3$probs, level = 2)
  Figure_S3B<-  ggplot(M3_probs,aes(x = value, y =L2 , fill = Var2)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
    facet_grid(Var1~.) +
    scale_fill_brewer(type = 'seq', palette = 'Red') +
    theme_bw() +
    labs(x = '', fill = 'probabilities') +
    guides(fill = guide_legend(reverse = TRUE))
  Figure_S3B
  ggsave("./data_eCM/Figure_S3B.pdf",Figure_S3B,device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  Interpolation_weighted$SES[M3$predclass==3]<-"high"
  Interpolation_weighted$SES[M3$predclass==2]<-"medium"
  Interpolation_weighted$SES[M3$predclass==1]<-"low"
  table(Interpolation_weighted$PIR,Interpolation_weighted$SES)
  Interpolation_weighted$SES<-factor(Interpolation_weighted$SES,
                             levels = c("low","medium","high"))
  save(Interpolation_weighted,file="./data_eCM/Interpolation_weighted.Rdata")
# +++++++++++================================+++++++++++ ####
# +++++++++++============Tables==============+++++++++++ ####
# +++++++++++================================+++++++++++ ####  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Multiple interpolation data (Table 1)  ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
         "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
         "BMI_status","T2D_status","Cancer_status")
VAR_ALL<-c("MORT_stat","WBC","CAL_mean","PPD_mean","DMFT","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
             "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HPL_status","HTN_status","CVD_status",
             "BMI","BMI_status","T2D_status","Cancer_status")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
Table1<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "./data_eCM/Table1_Rdata")  
{ #** section 19.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
  }
  load(file = "./data_eCM/Table1_Rdata")
  Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
  write.table(Table1,sep = ",",file ="./data_eCM/Table1.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 All-cause and cause-specific mortality (Table 2) ####  
  load(file="./data_eCM/Interpolation_weighted.Rdata")
  colnames(Interpolation_weighted)
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  { #* all model #####
    #all Crude
    model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis, design =rhcSvy)
    model1_all_result<-summary(model1_all)
    P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="All cause")
    result
    #all model1
    model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
    model2_all_result<-summary(model2_all)
    P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="All cause")
    result2
    result.all<-rbind(result,result2)
    #all model2
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-rbind(result.all,result3)
    result.all
  }
  
  { #* CVD model #####
    rhcSvy_DM_CVD<-rhcSvy
    #CVD Crude
    model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis, design =rhcSvy_DM_CVD)
    model1_CVD_result<-summary(model1_CVD)
    P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model1",'status'="CVD cause")
    result.CVD
    #CVD model1
    model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
    model2_CVD_result<-summary(model2_CVD)
    P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model2",'status'="CVD cause")
    result.CVD<-rbind(result.CVD,result2.CVD)
    result.CVD
    #CVD model2
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+BMI_status+HEI+Physical_status, design =rhcSvy_DM_CVD)
    model3_CVD_result<-summary(model3_CVD)
    P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model3",'status'="CVD cause")
    result.CVD<-rbind(result.CVD,result3.CVD)
    result.CVD
    
  }
  
  { #* Cancer model #####
    rhcSvy_DM_Cancer<-rhcSvy
    #Cancer Crude
    model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis, design =rhcSvy_DM_Cancer)
    model1_Cancer_result<-summary(model1_Cancer)
    P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'model'="model1",'status'="Cancer cause")
    result.Cancer
    #Cancer model1
    model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
    model2_Cancer_result<-summary(model2_Cancer)
    P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                 'P value' =P,'model'="model2",'status'="Cancer cause")
    result.Cancer<-rbind(result.Cancer,result2.Cancer)
    result.Cancer
    #Cancer model2
    model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
    model3_Cancer_result<-summary(model3_Cancer)
    P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                 'P value' =P,'model'="model3",'status'="Cancer cause")
    result.Cancer<-rbind(result.Cancer,result3.Cancer)
    result.Cancer
  }
  

  
  { #* Combine #####
    result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
    result.all.cause$HR<-round(result.all.cause$HR,2)
    result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
    result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
    result.all.cause$P.value<-round(result.all.cause$P.value,3)
    Table2<-result.all.cause
    write.table(Table2,sep = ",",file ="./data_eCM/Table2.csv")
  }
  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
  # >>>>> section 21 Stratified analyses (Table 3) ####  
  { #* Data Collation #####
    table(Interpolation_weighted$ucod_leading)
    load(file="./data_eCM/Interpolation_weighted.Rdata")
    colnames(Interpolation_weighted)
    ALL_Weight_plus<-Interpolation_weighted[,c("PD_diagnosis","MORT_stat","DM_MORT_stat","T2D_status","peryear",
                                               "Age_status","Gender","Race_ethnicity","SES",
                                               "BMI_status","Physical_status","Smoking_status","Drinking_status","HEI")]
    table(ALL_Weight_plus$Age_status,useNA = "ifany")
    ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age_status=="<45"]<-"<45"
    ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age_status=="[45,65)"|ALL_Weight_plus$Age_status==">=65"]<-">=45"
    
    ALL_Weight_plus$Age_sub<-factor(ALL_Weight_plus$Age_sub,
                                    levels = c("<45",">=45")) 
    table(ALL_Weight_plus$Age_sub)
    #RACE
    table(ALL_Weight_plus$Race_ethnicity)
    ALL_Weight_plus$Race_sub<-"Other_Race"
    ALL_Weight_plus$Race_sub[ALL_Weight_plus$Race_ethnicity=="Non-Hispanic White"]<-"Non-Hispanic white"
    table(ALL_Weight_plus$Race_sub)
    ALL_Weight_plus$Race_sub<-as.factor(ALL_Weight_plus$Race_sub)
    ALL_Weight_plus$Race_sub<-factor(ALL_Weight_plus$Race_sub,
                                     levels = c("Non-Hispanic white","Other_Race")) 
    table(ALL_Weight_plus$Race_sub)
    #BMI
    table(ALL_Weight_plus$BMI_status)
    ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI_status=="(0,25)"|ALL_Weight_plus$BMI_status=="[25.0-30)"]<-"<30"
    ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI_status=="[30,inf)"]<-">=30"
    ALL_Weight_plus$BMI_sub<-factor(ALL_Weight_plus$BMI_sub,
                                    levels = c("<30",">=30"))
    table(ALL_Weight_plus$BMI_sub)
    #smoke
    ALL_Weight_plus$Smoking_sub<-"Former/Current_smoker"
    ALL_Weight_plus$Smoking_sub[ALL_Weight_plus$Smoking_status=="Never_smoker"]<-"Never_smoker"
    ALL_Weight_plus$Smoking_sub<-factor(ALL_Weight_plus$Smoking_sub,
                                        levels = c("Never_smoker","Former/Current_smoker"))
    table(ALL_Weight_plus$Smoking_sub)
    
    #Drinking_status
    table(ALL_Weight_plus$Drinking_status)
    ALL_Weight_plus$Drinking_sub<-"drinker"
    ALL_Weight_plus$Drinking_sub[ALL_Weight_plus$Drinking_status=="Nondrinker"]<-"Nondrinker"
    ALL_Weight_plus$Drinking_sub<-factor(ALL_Weight_plus$Drinking_sub,
                                         levels = c("Nondrinker","drinker"))
    table(ALL_Weight_plus$Drinking_sub)
    #SES
    table(ALL_Weight_plus$SES)
    ALL_Weight_plus$SES_sub<-"medium/high"
    ALL_Weight_plus$SES_sub[ALL_Weight_plus$SES=="low"]<-"low"
    ALL_Weight_plus$SES_sub<-factor(ALL_Weight_plus$SES_sub,
                                    levels = c("low","medium/high"))
    table(ALL_Weight_plus$SES_sub)
    
    #HEI
    table(ALL_Weight_plus$HEI)
    ALL_Weight_plus$HEI_sub[ALL_Weight_plus$HEI=="Quintile 1"|ALL_Weight_plus$HEI=="Quintile 2"]<-"Quintile 1-2"
    ALL_Weight_plus$HEI_sub[ALL_Weight_plus$HEI=="Quintile 3"|ALL_Weight_plus$HEI=="Quintile 4"|
                              ALL_Weight_plus$HEI=="Quintile 5"]<-"Quintile 3-5"
    ALL_Weight_plus$HEI_sub<-factor(ALL_Weight_plus$HEI_sub,
                                    levels = c("Quintile 1-2","Quintile 3-5"))
    
    
    #Physical_status
    table(ALL_Weight_plus$Physical_status)
    ALL_Weight_plus$Physical_sub[ALL_Weight_plus$Physical_status=="Inactive"|ALL_Weight_plus$Physical_status=="Insufficient"]<-"Insufficient"
    ALL_Weight_plus$Physical_sub[ALL_Weight_plus$Physical_status=="Recommended"]<-"Recommended"
    ALL_Weight_plus$Physical_sub<-factor(ALL_Weight_plus$Physical_sub,
                                         levels = c("Insufficient","Recommended"))
    table(ALL_Weight_plus$Physical_sub)
    
    ALL_Weight_plus$sdmvpsu<-Interpolation_weighted$sdmvpsu
    ALL_Weight_plus$sdmvstra<-Interpolation_weighted$sdmvstra
    ALL_Weight_plus$weight<-Interpolation_weighted$weight
    
  }
  { #* AGE #####
    rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
    rhcSvy_45<-subset(rhcSvy,Age_sub=="<45")
    rhcSvy_80<-subset(rhcSvy,Age_sub==">=45")
    svytable(~T2D_status+Age_sub,rhcSvy_80)
    colnames(ALL_Weight_plus)
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+
                                Race_ethnicity+SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_45)
    Age45<-summary(MODEL_ALL_inter)
    P<-Age45[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Age45[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Age1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Age","group"="<45",'status'="all cause")
    Age1
    Age.all<-Age1
    Age.all
    #45-80
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+
                                Race_ethnicity+SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_80)
    Age80<-summary(MODEL_ALL_inter)
    P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Age3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Age","group"=">=45",'status'="all cause")
    Age3
    
    Age.all<-rbind(Age.all,Age3)
    Age.all
    
    #epiR
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_sub+PD_diagnosis*Age_sub+Gender+
                                Race_ethnicity+SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy)
    Age<-summary(MODEL_ALL_inter)
    P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    Age5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Age","group"="interaction",'status'="all cause")
    Age.all<-rbind(Age.all,Age5)
    Age.all
  }
  { #* RACE #####
    table(ALL_Weight_plus$Race_sub)
    rhcSvy_white<-subset(rhcSvy,Race_sub=="Non-Hispanic white")
    rhcSvy_Other<-subset(rhcSvy,Race_sub=="Other_Race")
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_white)
    Racewhite<-summary(MODEL_ALL_inter)
    P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Race1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="Race","group"="white",'status'="all cause")
    Race1
    Race.all<-Race1
    Race.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_Other)
    Racewhite<-summary(MODEL_ALL_inter)
    P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Race3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="Race","group"="Other",'status'="all cause")
    Race3
    Race.all<-rbind(Race.all,Race3)
    Race.all
    #epiR
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Race_sub+PD_diagnosis*Race_sub+Age_status+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy)
    Age<-summary(MODEL_ALL_inter)
    P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    Race5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Race","group"="interaction",'status'="all cause")
    Race.all<-rbind(Race.all,Race5)
    Race.all
  }
  {#* gender #####
    table(ALL_Weight_plus$Gender)
    rhcSvy_Female<-subset(rhcSvy,Gender=="Female")
    rhcSvy_Male<-subset(rhcSvy,Gender=="Male")
    
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_Female)
    Racewhite<-summary(MODEL_ALL_inter)
    P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Gender1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="Gender","group"="Female",'status'="all cause")
    Gender1

    Gender.all<-Gender1
    Gender.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy_Male)
    Racewhite<-summary(MODEL_ALL_inter)
    P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Gender3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="Gender","group"="Male",'status'="all cause")
    Gender3
    Gender.all<-rbind(Gender.all,Gender3)
    Gender.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+PD_diagnosis*Gender+Age_status+Race_ethnicity+
                                SES+
                                Smoking_status+Drinking_status+HEI+
                                BMI_status+Physical_status, design =rhcSvy)
    Age<-summary(MODEL_ALL_inter)
    P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    Gender5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="Gender","group"="interaction",'status'="all cause")
    Gender.all<-rbind(Gender.all,Gender5)
    Gender.all
  }
  {#* BMI #####
    
    colnames(ALL_Weight_plus)
    table(ALL_Weight_plus$BMI_sub)
    rhcSvy_low<-subset(rhcSvy,BMI_sub=="<30")
    rhcSvy_high<-subset(rhcSvy,BMI_sub==">=30")
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI
                              , design =rhcSvy_low)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    BMI1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="BMI","group"="<30",'status'="all cause")
    BMI1
    BMI.all<-BMI1
    BMI.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI, design =rhcSvy_high)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    BMI3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="BMI","group"=">=30",'status'="all cause")
    BMI3
    BMI.all<-rbind(BMI.all,BMI3)
    BMI.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+BMI_sub+PD_diagnosis*BMI_sub+Age_status+Race_ethnicity+Gender+
                                SES+
                                Smoking_status+Drinking_status+HEI, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    BMI5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="BMI","group"="interaction",'status'="all cause")
    BMI.all<-rbind(BMI.all,BMI5)
    BMI.all
  }
  {#* Smoking_sub #####
    colnames(ALL_Weight_plus)
    table(ALL_Weight_plus$Smoking_sub)
    rhcSvy_Current<-subset(rhcSvy,Smoking_sub=="Former/Current_smoker")
    rhcSvy_Never<-subset(rhcSvy,Smoking_sub=="Never_smoker")
    svytable(~DM_MORT_stat+Smoking_sub,rhcSvy_Current)
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                BMI_status+Physical_status+Drinking_status+HEI, design =rhcSvy_Current)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Smoke1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="Smoke","group"="Current",'status'="all cause")
    Smoke1

    Smoke.all<-Smoke1
    Smoke.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                BMI_status+Physical_status+Drinking_status+HEI, design =rhcSvy_Never)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Smoke3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="Smoke","group"="Never",'status'="all cause")
    Smoke3
    Smoke.all<-rbind(Smoke.all,Smoke3)
    Smoke.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Smoking_sub+PD_diagnosis*Smoking_sub+Race_ethnicity+Gender+
                                Age_status+SES+
                                BMI_status+Physical_status+Drinking_status+HEI, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    Smoke5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="Smoke","group"="interaction",'status'="all cause")
    Smoke.all<-rbind(Smoke.all,Smoke5)
    Smoke.all

  }
  {#* Drinking_sub #####
    colnames(ALL_Weight_plus)
    table(ALL_Weight_plus$Drinking_sub)
    rhcSvy_nondrinker<-subset(rhcSvy,Drinking_sub=='Nondrinker')

    rhcSvy_drinker<-subset(rhcSvy,Drinking_sub=='drinker')
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+BMI_status+Physical_status+HEI+Smoking_status, design =rhcSvy_nondrinker)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Drinking_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'subgroup'="Drinking_sub","group"="YES",'status'="all cause")
    Drinking_sub1
    Drinking_sub.all<-Drinking_sub1
    Drinking_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                BMI_status+Physical_status+HEI+Smoking_status, design =rhcSvy_drinker)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    Drinking_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'subgroup'="Drinking_sub","group"="NO",'status'="all cause")
    Drinking_sub3
  
    Drinking_sub.all<-rbind(Drinking_sub.all,Drinking_sub3)
    Drinking_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Drinking_sub+PD_diagnosis*Drinking_sub+Race_ethnicity+Gender+
                                Age_status+SES+BMI_status+Physical_status+HEI+Smoking_status, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    Drinking_sub5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'subgroup'="Drinking_sub","group"="interaction",'status'="all cause")
    Drinking_sub.all<-rbind(Drinking_sub.all,Drinking_sub5)
    Drinking_sub.all

  }
  {#* SES #####
    colnames(ALL_Weight_plus)
    table(ALL_Weight_plus$SES_sub)
    rhcSvy_low<-subset(rhcSvy,SES_sub=="low")
    rhcSvy_high<-subset(rhcSvy,SES_sub=="medium/high")
    svytable(~DM_MORT_stat+SES_sub,rhcSvy_low)
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                
                                BMI_status+Physical_status+Drinking_status+HEI+Smoking_status, design =rhcSvy_low)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    SES1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="low",'status'="all cause")
    SES1
    SES.all<-SES1
    SES.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                Smoking_status+
                                BMI_status+Physical_status+Drinking_status+HEI, design =rhcSvy_high)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    SES3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="high",'status'="all cause")
    SES3
   
    SES.all<-rbind(SES.all,SES3)
    SES.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+SES_sub+PD_diagnosis*SES_sub+Race_ethnicity+Gender+
                                Age_status+Smoking_status+
                                BMI_status+Physical_status+Drinking_status+HEI, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    SES5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="SES","group"="interaction",'status'="all cause")
    SES.all<-rbind(SES.all,SES5)
    SES.all

  }
  {#* HEI #####
    rhcSvy_HEI1<-subset(rhcSvy,HEI_sub=="Quintile 1-2")
    rhcSvy_HEI2<-subset(rhcSvy,HEI_sub=="Quintile 3-5")
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+BMI_status+Physical_status+Drinking_status+Smoking_status, design =rhcSvy_HEI1)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    HEI_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="YES",'status'="all cause")
    HEI_sub1
  
    HEI_sub.all<-HEI_sub1
    HEI_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                BMI_status+Physical_status+Drinking_status+Smoking_status, design =rhcSvy_HEI2)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    HEI_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="NO",'status'="all cause")
    HEI_sub3
    
    HEI_sub.all<-rbind(HEI_sub.all,HEI_sub3)
    HEI_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+HEI_sub+PD_diagnosis*HEI_sub+Race_ethnicity+Gender+
                                Age_status+SES+
                                BMI_status+Physical_status+Drinking_status+Smoking_status, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    HEI_sub5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="HEI_sub","group"="interaction",'status'="all cause")
    HEI_sub.all<-rbind(HEI_sub.all,HEI_sub5)
    HEI_sub.all

  }
  
  {#* PA #####
    table(ALL_Weight_plus$Physical_sub)
    rhcSvy_PA1<-subset(rhcSvy,Physical_sub=="Insufficient")
    rhcSvy_PA2<-subset(rhcSvy,Physical_sub=="Recommended")
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+BMI_status+Drinking_status+Smoking_status, design =rhcSvy_PA1)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    PA_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="PA_sub","group"="YES",'status'="all cause")
    PA_sub1
    
    PA_sub.all<-PA_sub1
    PA_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                                SES+
                                BMI_status+HEI+Drinking_status+Smoking_status, design =rhcSvy_PA2)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    PA_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="PA_sub","group"="NO",'status'="all cause")
    PA_sub3

    PA_sub.all<-rbind(PA_sub.all,PA_sub3)
    PA_sub.all
    MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+PA_sub+PD_diagnosis*PA_sub+Race_ethnicity+Gender+
                                Age_status+SES+
                                BMI_status+HEI+Drinking_status+Smoking_status, design =rhcSvy)
    MODEL<-summary(MODEL_ALL_inter)
    P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
    HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
    PA_sub5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="PA_sub","group"="interaction",'status'="all cause")
    PA_sub.all<-rbind(PA_sub.all,PA_sub5)
    PA_sub.all

  }
  
  
  all.subgroup<-rbind(Age.all,Race.all,Gender.all,BMI.all,Smoke.all,Drinking_sub.all,SES.all,HEI_sub.all,PA_sub.all)
  all.subgroup$HR<-round(all.subgroup$HR,2)
  all.subgroup$lower..95<-round(all.subgroup$lower..95,2)
  all.subgroup$upper..95<-round(all.subgroup$upper..95,2)
  all.subgroup$P.value<-round(all.subgroup$P.value,3)
  Table3<-all.subgroup
  write.table(Table3,sep = ",",file ="./data_eCM/Table3.csv")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative mortality rates (Table S2) ####  
load(file="./data_eCM/Interpolation_weighted.Rdata")
#Interpolation_weighted<-subset(Interpolation_weighted,Gender=="Male")
#PD_DM
PD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
PD_DM$year<-PD_DM$peryear
PD_DM$Pyear<-PD_DM$year*PD_DM$weight
PD_DM_death_all<-PD_DM[which(PD_DM$MORT_stat==1),]
PD_DM_death_CVD<-PD_DM[which(PD_DM$CVD_MORT_stat==1),]
PD_DM_death_Cancer<-PD_DM[which(PD_DM$Cancer_MORT_stat==1),]
PD_DM_death_DM<-PD_DM[which(PD_DM$DM_MORT_stat==1),]
PD_DM_Perseon_year_un<-sum(PD_DM$year)
PD_DM_Perseon_year_ad<-sum(PD_DM$Pyear)
PD_DM_Perseon<-sum(PD_DM$weight)
PD_DM_Perseon_un_ALL<-as.numeric(summary(PD_DM$MORT_stat==1)[3])
PD_DM_Perseon_un_CVD<-as.numeric(summary(PD_DM$CVD_MORT_stat==1)[3])
PD_DM_Perseon_un_Cancer<-as.numeric(summary(PD_DM$Cancer_MORT_stat==1)[3])
PD_DM_Perseon_un_DM<-as.numeric(summary(PD_DM$DM_MORT_stat==1)[3])

#noPD_DM
noPD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
noPD_DM$year<-noPD_DM$peryear
noPD_DM$Pyear<-noPD_DM$year*noPD_DM$weight
noPD_DM_death_all<-noPD_DM[which(noPD_DM$MORT_stat==1),]
noPD_DM_death_CVD<-noPD_DM[which(noPD_DM$CVD_MORT_stat==1),]
noPD_DM_death_Cancer<-noPD_DM[which(noPD_DM$Cancer_MORT_stat==1),]
noPD_DM_death_DM<-noPD_DM[which(noPD_DM$DM_MORT_stat==1),]
noPD_DM_Perseon_year_un<-sum(noPD_DM$year)
noPD_DM_Perseon_year_ad<-sum(noPD_DM$Pyear)
noPD_DM_Perseon<-sum(noPD_DM$weight)
noPD_DM_Perseon_un_ALL<-as.numeric(summary(noPD_DM$MORT_stat==1)[3])
noPD_DM_Perseon_un_CVD<-as.numeric(summary(noPD_DM$CVD_MORT_stat==1)[3])
noPD_DM_Perseon_un_Cancer<-as.numeric(summary(noPD_DM$Cancer_MORT_stat==1)[3])
noPD_DM_Perseon_un_DM<-as.numeric(summary(noPD_DM$DM_MORT_stat==1)[3])

#PD_ALL
PD_ALL<-PD_DM_Perseon_un_ALL*(1000/PD_DM_Perseon_year_un)
PD_ALL_UCL<-(PD_DM_Perseon_un_ALL+(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_LCL<-(PD_DM_Perseon_un_ALL-(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_Incidence<-paste0(round(PD_ALL,2)," (",round(PD_ALL_LCL,2),"-",round(PD_ALL_UCL,2),")")
PD_ALL_Incidence
#PD_CVD
PD_CVD<-PD_DM_Perseon_un_CVD*(1000/PD_DM_Perseon_year_un)
PD_CVD_UCL<-(PD_DM_Perseon_un_CVD+(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_LCL<-(PD_DM_Perseon_un_CVD-(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_Incidence<-paste0(round(PD_CVD,2)," (",round(PD_CVD_LCL,2),"-",round(PD_CVD_UCL,2),")")
PD_CVD_Incidence
#PD_Cancer
PD_Cancer<-PD_DM_Perseon_un_Cancer*(1000/PD_DM_Perseon_year_un)
PD_Cancer_UCL<-(PD_DM_Perseon_un_Cancer+(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_LCL<-(PD_DM_Perseon_un_Cancer-(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_Incidence<-paste0(round(PD_Cancer,2)," (",round(PD_Cancer_LCL,2),"-",round(PD_Cancer_UCL,2),")")
PD_Cancer_Incidence

#noPD_ALL
noPD_ALL<-noPD_DM_Perseon_un_ALL*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_UCL<-(noPD_DM_Perseon_un_ALL+(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_LCL<-(noPD_DM_Perseon_un_ALL-(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_Incidence<-paste0(round(noPD_ALL,2)," (",round(noPD_ALL_LCL,2),"-",round(noPD_ALL_UCL,2),")")

#noPD_CVD
noPD_CVD<-noPD_DM_Perseon_un_CVD*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_UCL<-(noPD_DM_Perseon_un_CVD+(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_LCL<-(noPD_DM_Perseon_un_CVD-(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_Incidence<-paste0(round(noPD_CVD,2)," (",round(noPD_CVD_LCL,2),"-",round(noPD_CVD_UCL,2),")")
#noPD_Cancer
noPD_Cancer<-noPD_DM_Perseon_un_Cancer*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_UCL<-(noPD_DM_Perseon_un_Cancer+(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_LCL<-(noPD_DM_Perseon_un_Cancer-(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_Incidence<-paste0(round(noPD_Cancer,2)," (",round(noPD_Cancer_LCL,2),"-",round(noPD_Cancer_UCL,2),")")

#EVENTS
#all.cause
Interpolation_weighted$T2D_status
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))

TableS1_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS1_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N", "Incidence Rate (95% CI)")
TableS1_3<-c("All-cause mortality",total.counts[1,1],noPD_ALL_Incidence,total.counts[1,2],PD_ALL_Incidence)
TableS1_4<-c("CVD mortality",total.counts[2,1],noPD_CVD_Incidence,total.counts[2,2],PD_CVD_Incidence)
TableS1_5<-c("Cancer mortality",total.counts[3,1],noPD_Cancer_Incidence,total.counts[3,2],PD_Cancer_Incidence)
TableS1<-as.data.frame(rbind(TableS1_1,TableS1_2,TableS1_3,TableS1_4,TableS1_5))
TableS1
write.table(TableS1,sep = ",",file ="./data_eCM/TableS2.csv")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 dose -response effect (Table S3) #### 
load(file="./data_eCM/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis_diagnosis<-as.character(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis[
  Interpolation_weighted$Periodontitis_diagnosis=="normal"|Interpolation_weighted$Periodontitis_diagnosis=="mild"
]<-"normal/mild"
table(Interpolation_weighted$Periodontitis)
Interpolation_weighted$Periodontitis<-factor(Interpolation_weighted$Periodontitis,
                                             levels = c("normal/mild","moderate","severe"))
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-result3
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.CVD <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-result3.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.Cancer <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-result3.Cancer
}


{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS3_1<-result.all.cause
  write.table(TableS3_1,sep = ",",file ="./data_eCM/TableS3_1.csv")
}
Interpolation_weighted$Periodontitis<-as.character(Interpolation_weighted$Periodontitis)
table(Interpolation_weighted$Periodontitis)
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="normal/mild"]<-0
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="moderate"]<-1
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="severe"]<-2
Interpolation_weighted$Periodontitis<-as.numeric(Interpolation_weighted$Periodontitis)

{#* P for trend ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  { #* all model #####
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-result3
    result.all
  }
  
  { #* CVD model #####
    rhcSvy_DM_CVD<-rhcSvy
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
    model3_CVD_result<-summary(model3_CVD)
    P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model3",'status'="CVD cause")
    result.CVD<-result3.CVD
    result.CVD
    
  }
  
  { #* Cancer model #####
    rhcSvy_DM_Cancer<-rhcSvy
    #Cancer model2
    model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
    model3_Cancer_result<-summary(model3_Cancer)
    P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                 'P value' =P,'model'="model3",'status'="Cancer cause")
    result.Cancer<-result3.Cancer
    result.Cancer
  }
  

  
  { #* Combine #####
    result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
    result.all.cause$HR<-round(result.all.cause$HR,2)
    result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
    result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
    result.all.cause$P.value<-round(result.all.cause$P.value,3)
    TableS3_2<-result.all.cause
    write.table(TableS3_2,sep = ",",file ="./data_eCM/TableS3_2.csv")
  }
}

table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$MORT_stat)

table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$CVD_MORT_stat)
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$Cancer_MORT_stat)
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_status=="NO")
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$Cancer_MORT_stat,Interpolation_weighted$Cancer_status=="NO")


table(Interpolation_weighted$Periodontitis,Interpolation_weighted$DM_MORT_stat)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#>>>>> section 24 Clasfication of quantile ####  
load(file="./data_eCM/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
quantile<-svyquantile(~CAL_mean+PPD_mean+DMFT, rhcSvy, c(.25,.5,.75))
quantile[["CAL_mean"]][1]
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean<=quantile[["CAL_mean"]][1]]<-"Quantile 1"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][1]&
                                    Interpolation_weighted$CAL_mean<= quantile[["CAL_mean"]][2]]<-"Quantile 2"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][2]&
                                      Interpolation_weighted$CAL_mean<= quantile[["CAL_mean"]][3]]<-"Quantile 3"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][3]]<-"Quantile 4"

table(Interpolation_weighted$CAL_quantile)
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean<=quantile[["PPD_mean"]][1]]<-"Quantile 1"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][1]&
                                      Interpolation_weighted$PPD_mean<= quantile[["PPD_mean"]][2]]<-"Quantile 2"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][2]&
                                      Interpolation_weighted$PPD_mean<= quantile[["PPD_mean"]][3]]<-"Quantile 3"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][3]]<-"Quantile 4"

table(Interpolation_weighted$PPD_quantile)
Interpolation_weighted$DMFT_quantile[Interpolation_weighted$DMFT<=quantile[["DMFT"]][1]]<-"Quantile 1"
Interpolation_weighted$DMFT_quantile[Interpolation_weighted$DMFT>quantile[["DMFT"]][1]&
                                      Interpolation_weighted$DMFT<=quantile[["DMFT"]][2]]<-"Quantile 2"
Interpolation_weighted$DMFT_quantile[Interpolation_weighted$DMFT>quantile[["DMFT"]][2]&
                                      Interpolation_weighted$DMFT<= quantile[["DMFT"]][3]]<-"Quantile 3"
Interpolation_weighted$DMFT_quantile[Interpolation_weighted$DMFT>quantile[["DMFT"]][3]]<-"Quantile 4"

table(Interpolation_weighted$DMFT_quantile)
save(Interpolation_weighted,file="./data_eCM/Interpolation_weighted.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 Sensitivity Analysis 1 CAL (Table 3-1) ####  
load(file="./data_eCM/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}


{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS4_1<-result.all.cause
}


{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD

  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                         CAL_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                         CAL_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  
}


{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS4_1<-result.all.cause
  write.table(TableS4_1,sep = ",",file ="./data_eCM/TableS4_1.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 Sensitivity Analysis 1 PPD (Table 3-2) ####  
load(file="./data_eCM/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)


rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS4_2<-result.all.cause
}
TableS4_2



{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS4_2<-result.all.cause
  write.table(TableS4_2,sep = ",",file ="./data_eCM/TableS4_2.csv")
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Sensitivity Analysis 1 DMFT (Table S5) ####  
colnames(Interpolation_weighted)


rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS5<-result.all.cause
}
  TableS5
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT_quantile+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT_quantile+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT_quantile+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS5<-result.all.cause
  write.table(TableS5,sep = ",",file ="./data_eCM/TableS5.csv")
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 peryear>=2 (Table S6) ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
Interpolation_weighted<-subset(Interpolation_weighted,peryear>=2)




rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}


{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S6<-result.all.cause
  write.table(Table_S6,sep = ",",file ="./data_eCM/Table_S6.csv")
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 omitted cancer patients (Table S7) ####  
load(file="./data_eCM/Interpolation_weighted.Rdata")
table(Interpolation_weighted$CVD_status)
Interpolation_weighted<-subset(Interpolation_weighted,CVD_status="NO",Cancer_status=="NO")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}


{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS7<-result.all.cause
  write.table(TableS7,sep = ",",file ="./data_eCM/TableS7.csv")
}

PD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
PD_DM$year<-PD_DM$peryear
PD_DM$Pyear<-PD_DM$year*PD_DM$weight
PD_DM_death_all<-PD_DM[which(PD_DM$MORT_stat==1),]
PD_DM_death_CVD<-PD_DM[which(PD_DM$CVD_MORT_stat==1),]
PD_DM_death_Cancer<-PD_DM[which(PD_DM$Cancer_MORT_stat==1),]
PD_DM_death_DM<-PD_DM[which(PD_DM$DM_MORT_stat==1),]
PD_DM_Perseon_year_un<-sum(PD_DM$year)
PD_DM_Perseon_year_ad<-sum(PD_DM$Pyear)
PD_DM_Perseon<-sum(PD_DM$weight)
PD_DM_Perseon_un_ALL<-as.numeric(summary(PD_DM$MORT_stat==1)[3])
PD_DM_Perseon_un_CVD<-as.numeric(summary(PD_DM$CVD_MORT_stat==1)[3])
PD_DM_Perseon_un_Cancer<-as.numeric(summary(PD_DM$Cancer_MORT_stat==1)[3])
PD_DM_Perseon_un_DM<-as.numeric(summary(PD_DM$DM_MORT_stat==1)[3])

#noPD_DM
noPD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
noPD_DM$year<-noPD_DM$peryear
noPD_DM$Pyear<-noPD_DM$year*noPD_DM$weight
noPD_DM_death_all<-noPD_DM[which(noPD_DM$MORT_stat==1),]
noPD_DM_death_CVD<-noPD_DM[which(noPD_DM$CVD_MORT_stat==1),]
noPD_DM_death_Cancer<-noPD_DM[which(noPD_DM$Cancer_MORT_stat==1),]
noPD_DM_death_DM<-noPD_DM[which(noPD_DM$DM_MORT_stat==1),]
noPD_DM_Perseon_year_un<-sum(noPD_DM$year)
noPD_DM_Perseon_year_ad<-sum(noPD_DM$Pyear)
noPD_DM_Perseon<-sum(noPD_DM$weight)
noPD_DM_Perseon_un_ALL<-as.numeric(summary(noPD_DM$MORT_stat==1)[3])
noPD_DM_Perseon_un_CVD<-as.numeric(summary(noPD_DM$CVD_MORT_stat==1)[3])
noPD_DM_Perseon_un_Cancer<-as.numeric(summary(noPD_DM$Cancer_MORT_stat==1)[3])
noPD_DM_Perseon_un_DM<-as.numeric(summary(noPD_DM$DM_MORT_stat==1)[3])

#PD_ALL
PD_ALL<-PD_DM_Perseon_un_ALL*(1000/PD_DM_Perseon_year_un)
PD_ALL_UCL<-(PD_DM_Perseon_un_ALL+(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_LCL<-(PD_DM_Perseon_un_ALL-(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_Incidence<-paste0(round(PD_ALL,2)," (",round(PD_ALL_LCL,2),"-",round(PD_ALL_UCL,2),")")
PD_ALL_Incidence
#PD_CVD
PD_CVD<-PD_DM_Perseon_un_CVD*(1000/PD_DM_Perseon_year_un)
PD_CVD_UCL<-(PD_DM_Perseon_un_CVD+(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_LCL<-(PD_DM_Perseon_un_CVD-(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_Incidence<-paste0(round(PD_CVD,2)," (",round(PD_CVD_LCL,2),"-",round(PD_CVD_UCL,2),")")
PD_CVD_Incidence
#PD_Cancer
PD_Cancer<-PD_DM_Perseon_un_Cancer*(1000/PD_DM_Perseon_year_un)
PD_Cancer_UCL<-(PD_DM_Perseon_un_Cancer+(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_LCL<-(PD_DM_Perseon_un_Cancer-(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_Incidence<-paste0(round(PD_Cancer,2)," (",round(PD_Cancer_LCL,2),"-",round(PD_Cancer_UCL,2),")")
PD_Cancer_Incidence

#noPD_ALL
noPD_ALL<-noPD_DM_Perseon_un_ALL*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_UCL<-(noPD_DM_Perseon_un_ALL+(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_LCL<-(noPD_DM_Perseon_un_ALL-(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_Incidence<-paste0(round(noPD_ALL,2)," (",round(noPD_ALL_LCL,2),"-",round(noPD_ALL_UCL,2),")")

#noPD_CVD
noPD_CVD<-noPD_DM_Perseon_un_CVD*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_UCL<-(noPD_DM_Perseon_un_CVD+(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_LCL<-(noPD_DM_Perseon_un_CVD-(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_Incidence<-paste0(round(noPD_CVD,2)," (",round(noPD_CVD_LCL,2),"-",round(noPD_CVD_UCL,2),")")
#noPD_Cancer
noPD_Cancer<-noPD_DM_Perseon_un_Cancer*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_UCL<-(noPD_DM_Perseon_un_Cancer+(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_LCL<-(noPD_DM_Perseon_un_Cancer-(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_Incidence<-paste0(round(noPD_Cancer,2)," (",round(noPD_Cancer_LCL,2),"-",round(noPD_Cancer_UCL,2),")")

#EVENTS
#all.cause
Interpolation_weighted$T2D_status
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))

TableS1_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS1_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N", "Incidence Rate (95% CI)")
TableS1_3<-c("All-cause mortality",total.counts[1,1],noPD_ALL_Incidence,total.counts[1,2],PD_ALL_Incidence)
TableS1_4<-c("CVD mortality",total.counts[2,1],noPD_CVD_Incidence,total.counts[2,2],PD_CVD_Incidence)
TableS1_5<-c("Cancer mortality",total.counts[3,1],noPD_Cancer_Incidence,total.counts[3,2],PD_Cancer_Incidence)
TableS1<-as.data.frame(rbind(TableS1_1,TableS1_2,TableS1_3,TableS1_4,TableS1_5))
TableS1

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Original data (Table S8)  ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
load(file="./data_eCM/Original_weighted.Rdata")
Original_weighted$SES<-Interpolation_weighted$SES
table(Original_weighted$PD_diagnosis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Original_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("Age_status","Gender","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI","BMI_status"
       )
VAR_ALL<-c("CAL_mean","PPD_mean","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
           "BMI","BMI_status")
{ #** OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
TableS8<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(TableS8,file = "./data_eCM/TableS8_Rdata")  
{ #** t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "./data_eCM/TableS8_Rdata")
TableS8<-merge(TableS8,test_data,by="Covariates",all = T)
write.table(TableS8,sep = ",",file ="./data_eCM/TableS8.csv")  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Original data HRs (Table S9) ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
load(file="./data_eCM/Original_weighted.Rdata")
Original_weighted$SES<-Interpolation_weighted$SES
Interpolation_weighted<-Original_weighted
table(Original_weighted$sdmvpsu)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S9<-result.all.cause
  write.table(Table_S9,sep = ",",file ="./data_eCM/Table_S9.csv")
}
PD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
PD_DM$year<-PD_DM$peryear
PD_DM$Pyear<-PD_DM$year*PD_DM$weight
PD_DM_death_all<-PD_DM[which(PD_DM$MORT_stat==1),]
PD_DM_death_CVD<-PD_DM[which(PD_DM$CVD_MORT_stat==1),]
PD_DM_death_Cancer<-PD_DM[which(PD_DM$Cancer_MORT_stat==1),]
PD_DM_death_DM<-PD_DM[which(PD_DM$DM_MORT_stat==1),]
PD_DM_Perseon_year_un<-sum(PD_DM$year)
PD_DM_Perseon_year_ad<-sum(PD_DM$Pyear)
PD_DM_Perseon<-sum(PD_DM$weight)
PD_DM_Perseon_un_ALL<-as.numeric(summary(PD_DM$MORT_stat==1)[3])
PD_DM_Perseon_un_CVD<-as.numeric(summary(PD_DM$CVD_MORT_stat==1)[3])
PD_DM_Perseon_un_Cancer<-as.numeric(summary(PD_DM$Cancer_MORT_stat==1)[3])
PD_DM_Perseon_un_DM<-as.numeric(summary(PD_DM$DM_MORT_stat==1)[3])

#noPD_DM
noPD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
noPD_DM$year<-noPD_DM$peryear
noPD_DM$Pyear<-noPD_DM$year*noPD_DM$weight
noPD_DM_death_all<-noPD_DM[which(noPD_DM$MORT_stat==1),]
noPD_DM_death_CVD<-noPD_DM[which(noPD_DM$CVD_MORT_stat==1),]
noPD_DM_death_Cancer<-noPD_DM[which(noPD_DM$Cancer_MORT_stat==1),]
noPD_DM_death_DM<-noPD_DM[which(noPD_DM$DM_MORT_stat==1),]
noPD_DM_Perseon_year_un<-sum(noPD_DM$year)
noPD_DM_Perseon_year_ad<-sum(noPD_DM$Pyear)
noPD_DM_Perseon<-sum(noPD_DM$weight)
noPD_DM_Perseon_un_ALL<-as.numeric(summary(noPD_DM$MORT_stat==1)[3])
noPD_DM_Perseon_un_CVD<-as.numeric(summary(noPD_DM$CVD_MORT_stat==1)[3])
noPD_DM_Perseon_un_Cancer<-as.numeric(summary(noPD_DM$Cancer_MORT_stat==1)[3])
noPD_DM_Perseon_un_DM<-as.numeric(summary(noPD_DM$DM_MORT_stat==1)[3])

#PD_ALL
PD_ALL<-PD_DM_Perseon_un_ALL*(1000/PD_DM_Perseon_year_un)
PD_ALL_UCL<-(PD_DM_Perseon_un_ALL+(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_LCL<-(PD_DM_Perseon_un_ALL-(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_Incidence<-paste0(round(PD_ALL,2)," (",round(PD_ALL_LCL,2),"-",round(PD_ALL_UCL,2),")")
PD_ALL_Incidence
#PD_CVD
PD_CVD<-PD_DM_Perseon_un_CVD*(1000/PD_DM_Perseon_year_un)
PD_CVD_UCL<-(PD_DM_Perseon_un_CVD+(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_LCL<-(PD_DM_Perseon_un_CVD-(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_Incidence<-paste0(round(PD_CVD,2)," (",round(PD_CVD_LCL,2),"-",round(PD_CVD_UCL,2),")")
PD_CVD_Incidence
#PD_Cancer
PD_Cancer<-PD_DM_Perseon_un_Cancer*(1000/PD_DM_Perseon_year_un)
PD_Cancer_UCL<-(PD_DM_Perseon_un_Cancer+(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_LCL<-(PD_DM_Perseon_un_Cancer-(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_Incidence<-paste0(round(PD_Cancer,2)," (",round(PD_Cancer_LCL,2),"-",round(PD_Cancer_UCL,2),")")
PD_Cancer_Incidence

#noPD_ALL
noPD_ALL<-noPD_DM_Perseon_un_ALL*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_UCL<-(noPD_DM_Perseon_un_ALL+(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_LCL<-(noPD_DM_Perseon_un_ALL-(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_Incidence<-paste0(round(noPD_ALL,2)," (",round(noPD_ALL_LCL,2),"-",round(noPD_ALL_UCL,2),")")

#noPD_CVD
noPD_CVD<-noPD_DM_Perseon_un_CVD*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_UCL<-(noPD_DM_Perseon_un_CVD+(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_LCL<-(noPD_DM_Perseon_un_CVD-(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_Incidence<-paste0(round(noPD_CVD,2)," (",round(noPD_CVD_LCL,2),"-",round(noPD_CVD_UCL,2),")")
#noPD_Cancer
noPD_Cancer<-noPD_DM_Perseon_un_Cancer*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_UCL<-(noPD_DM_Perseon_un_Cancer+(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_LCL<-(noPD_DM_Perseon_un_Cancer-(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_Incidence<-paste0(round(noPD_Cancer,2)," (",round(noPD_Cancer_LCL,2),"-",round(noPD_Cancer_UCL,2),")")

#EVENTS
#all.cause
Interpolation_weighted$T2D_status
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))

TableS1_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS1_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N", "Incidence Rate (95% CI)")
TableS1_3<-c("All-cause mortality",total.counts[1,1],noPD_ALL_Incidence,total.counts[1,2],PD_ALL_Incidence)
TableS1_4<-c("CVD mortality",total.counts[2,1],noPD_CVD_Incidence,total.counts[2,2],PD_CVD_Incidence)
TableS1_5<-c("Cancer mortality",total.counts[3,1],noPD_Cancer_Incidence,total.counts[3,2],PD_Cancer_Incidence)
TableS1<-as.data.frame(rbind(TableS1_1,TableS1_2,TableS1_3,TableS1_4,TableS1_5))
TableS1


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 31 Complete data (Table S10)   ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
load(file="./data_eCM/Complete_weighted.Rdata")
options( survey.lonely.psu = "adjust" )
Interpolation_weighted$ID<-rownames(Interpolation_weighted)
Complete_weighted$ID<-rownames(Complete_weighted)
SES<-Interpolation_weighted[,c("ID","SES")]
Complete_weightedSES<-merge(Complete_weighted,SES,by = "ID",all.x = T)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Complete_weightedSES,strata=~sdmvstra,weights = ~ weight)
var<-c("Age_status","Gender","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI","BMI_status"
)
VAR_ALL<-c("CAL_mean","PPD_mean","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
           "BMI","BMI_status")

{ #** OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
TableS10<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(TableS10,file = "./data_eCM/TableS10_Rdata")  
{ #** t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "./data_eCM/TableS10_Rdata")
TableS10<-merge(TableS10,test_data,by="Covariates",all = T)
write.table(TableS10,sep = ",",file ="./data_eCM/TableS10.csv")  







# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 32 Complete data HRs (Table S11) ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
load(file="./data_eCM/Complete_weighted.Rdata")
Interpolation_weighted$ID<-rownames(Interpolation_weighted)
Complete_weighted$ID<-rownames(Complete_weighted)
SES<-Interpolation_weighted[,c("ID","SES")]
Complete_weightedSES<-merge(Complete_weighted,SES,by = "ID",all.x = T)
Interpolation_weighted<-Complete_weightedSES
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Complete_weightedSES,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_status+Physical_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S11<-result.all.cause
  write.table(Table_S11,sep = ",",file ="./data_eCM/Table_S11.csv")
}
Interpolation_weighted<-Complete_weightedSES
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))




# +++++++++++================================+++++++++++ ####
# +++++++++++============Figures=============+++++++++++ ####
# +++++++++++================================+++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 33 Kaplan-Meier (Figure 2) ####
  load(file="./data_eCM/Interpolation_weighted.Rdata")
  { #* ALL Mortality Figure 2A ####
    #install.packages("ggthemes")
    library("ggthemes")
    FIT_ALL_PD<-survfit(Surv(peryear, MORT_stat==1) ~ PD_diagnosis, Interpolation_weighted)
    Figure_2A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                          ncensor.plot=F,risk.table = T,
                          break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                          xlab ="Time in Year",  pval = F, legend.title = "",ggtheme =  theme_calc())
    Figure_2A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                          ncensor.plot=F,risk.table = T,
                          break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                          xlab ="Time in Year", pval = , legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),
                                                                                           #panel.grid.major.y = element_line(color="black",size = 0.5),
                                                                                           panel.border = element_rect(fill=NA,color="black", size=0.7)))
    Figure_2A
    install.packages("eoffice")
    library(eoffice)
    topptx(Figure_2A ,"temp.pptx")
    
    ggsave("./data_eCM/Figure_2A1.pdf",  device = "pdf", width = 8, height = 6, units ="in",
           dpi = 600, limitsize = TRUE)
  } 
  

  { #* CIF Figure 2D ##### 

    load(file="./data_eCM/Interpolation_weighted.Rdata")
    Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==0]<-"0"
    Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==1]<-"3"
    Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$CVD_MORT_stat==1]<-"1"
    Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$Cancer_MORT_stat==1]<-"2"
    CIF<-cuminc(Interpolation_weighted$peryear,Interpolation_weighted$ALL_MORT_stat,Interpolation_weighted$PD_diagnosis,0,strata=Interpolation_weighted$sdmvstra,rho =Interpolation_weighted$weight) 
    #CIF<-cuminc(ftime=time,fstatus=cause,group=stage,cencode=0)
    
    CIF #print(CIF)
    NM_periodontitis_CVD<- as.data.frame(CIF[["No/Mild periodontitis 1"]])
    NM_periodontitis_CVD$type<-"No/Mild periodontitis 1"
    NM_periodontitis_CVD$group<-"CVD"
    NM_periodontitis_CVD$line<-"No/Mild periodontitis"
    MS_periodontitis_CVD<-as.data.frame(CIF[["Moderate/Severe periodontitis 1"]])
    MS_periodontitis_CVD$type<-"Moderate/Severe periodontitis 1"
    MS_periodontitis_CVD$group<-"CVD"
    MS_periodontitis_CVD$line<-"Moderate/Severe periodontitis"
    NM_periodontitis_Cancer<-as.data.frame(CIF[["No/Mild periodontitis 2"]])
    NM_periodontitis_Cancer$type<-"No/Mild periodontitis 2"
    NM_periodontitis_Cancer$group<-"Cancer"
    NM_periodontitis_Cancer$line<-"No/Mild periodontitis"
    MS_periodontitis_Cancer<-as.data.frame(CIF[["Moderate/Severe periodontitis 2"]])
    MS_periodontitis_Cancer$type<-"Moderate/Severe periodontitis 2"
    MS_periodontitis_Cancer$group<-"Cancer"
    MS_periodontitis_Cancer$line<-"Moderate/Severe periodontitis"
    NM_periodontitis_Other<-as.data.frame(CIF[["No/Mild periodontitis 3"]])
    NM_periodontitis_Other$type<-"No/Mild periodontitis 3"
    NM_periodontitis_Other$group<-"Others"
    NM_periodontitis_Other$line<-"No/Mild periodontitis"
    MS_periodontitis_Other<-as.data.frame(CIF[["Moderate/Severe periodontitis 3"]])
    MS_periodontitis_Other$type<-"Moderate/Severe periodontitis 3"
    MS_periodontitis_Other$line<-"Moderate/Severe periodontitis"
    MS_periodontitis_Other$group<-"Others"
    data<-rbind(NM_periodontitis_CVD,MS_periodontitis_CVD,NM_periodontitis_Cancer,MS_periodontitis_Cancer,
                NM_periodontitis_Other,MS_periodontitis_Other)
    #rename
    Figure_2D<- ggplot(data, aes(x=time, y=est, group=type,colour=group)) + geom_line(aes(linetype=line), size=1)+ theme_bw(base_size = 12)+
      
       theme(panel.background = element_rect(fill="white"),
             panel.grid.major.y = element_line(color="black",size = 0.5),
             panel.border = element_rect(fill=NA,color="black", size=0.7),
         axis.text = element_text(size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
      scale_color_manual(values=c("#0073C2FF", "#EFC000FF", "#CD534CFF","#8686864C"))+ 
      ylab("Cumulative incidence function") + xlab("Time in year") 
    Figure_2D
    ggsave("./data_eCM/Figure_2D.pdf",Figure_2D,device = "pdf", width = 12, height = 6, units ="in",
           dpi = 600, limitsize = TRUE)
  }
  mypal <- pal_jco("default", alpha = 0.9)(9)
  mypal
  show_col(mypal)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 RCS for CAL and PPD (Figure S4) ####
  if(dev.cur() > 1) dev.off()
setwd("./data_eCM")
load(file="./data_eCM/Interpolation_weighted.Rdata")
RCS <- Interpolation_weighted
{#* All cause mortarity ####
  {#** CAL_mean ####
    dev.off()
    options(datadist="ddist")
    ddist<-datadist(RCS)
    pdf("Figure_S4A.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(CAL_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(CAL_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.59
   
    ddist$limits$BMI[2]<-refvalue
    
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#f99500"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 0.8, paste0("Ref value = ","1.59")) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    colnames(RCS)
    setwd("./data_eCM")
    RCS <- Interpolation_weighted
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    dev.off()
    pdf("Figure_S4D.pdf",width=6,height=5)
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(PPD_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.50
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#00adf2"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.4, 0.8, paste0("Ref value = ","1.50")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }

}
{#* CVD mortarity ####
  RCS<- Interpolation_weighted
  RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
  RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
  {#** CAL_mean ####
    dev.off()
    pdf("Figure_S4B.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    for (i in 3:7) {
      fit <- cph(Surv(peryear,CVD_MORT_stat) ~ rcs(CAL_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(CAL_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.56
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#f99500"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.7, 0.7, paste0("Ref value = ","1.56")) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    RCS<-Interpolation_weighted
    RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
    RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
    pdf("Figure_S4E.pdf",width=6,height=5)
    setwd("./data_eCM")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    colnames(RCS)
    RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,CVD_MORT_stat) ~ rcs(PPD_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,CVD_MORT_stat)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.50
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#00adf2"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.4, 1.1, paste0("Ref value = ","1.50")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }

}
{#* Cancer mortarity ####
  RCS<-Interpolation_weighted
  RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
  RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
  {#** CAL_mean ####
    pdf("Figure_S4C.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.60
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#f99500"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.4, 0.7, paste0("Ref value = ","1.60")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    RCS<-subset(Interpolation_weighted,Cancer_status=="NO")
    colnames(RCS)
    setwd("./data_eCM")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Figure_S4F.pdf",width=6,height=5)

    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_status+Physical_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.53
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#00adf2"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.2, 0.8, paste0("Ref value = ","1.53")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}






# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section  mediation(Table ?) ####
load(file="./data_eCM/PD_III.Rdata")
load(file="./data_eCM/PD_CON1.Rdata")
load(file="./data_eCM/PD_CON2.Rdata")
PD_III$ID<-paste0("NHANES_III_",PD_III$SEQN)
PD_CON1$ID<-paste0("NHANES_CON1_",PD_CON1$SEQN)
PD_CON2$ID<-paste0("NHANES_CON2_",PD_CON2$SEQN)
PD<-rbind(PD_III,PD_CON1,PD_CON2)
load(file="./data_eCM/Covariates_III.Rdata")
load(file="./data_eCM/Covariates_CON1.Rdata")
load(file="./data_eCM/Covariates_CON2.Rdata")
Covariates_III$ID<-paste0("NHANES_III_",Covariates_III$SEQN)
Covariates_CON1$ID<-paste0("NHANES_CON1_",Covariates_CON1$SEQN)
Covariates_CON2$ID<-paste0("NHANES_CON2_",Covariates_CON2$SEQN)
Covariates<-rbind(Covariates_III,Covariates_CON1,Covariates_CON2)
PD_Covariates<-merge(PD,Covariates,by = "ID",all.x = T)

load(file="./data_eCM/WBC_III.Rdata")
load(file="./data_eCM/WBC_CON1.Rdata")
load(file="./data_eCM/WBC_CON2.Rdata")
WBC_III$ID<-paste0("NHANES_III_",WBC_III$SEQN)
WBC_CON1$ID<-paste0("NHANES_CON1_",WBC_CON1$SEQN)
WBC_CON2$ID<-paste0("NHANES_CON2_",WBC_CON2$SEQN)
WBC<-rbind(WBC_III,WBC_CON1,WBC_CON2)
PD_WBC<-merge(PD_Covariates,WBC,by = "ID",all.x = T)
load(file="./data_eCM/CRP.Rdata")
PD_CRP<-merge(PD_WBC,CRP,by = "ID",all.x = T)
load(file="./data_eCM/Fb.Rdata")
PD_Fb<-merge(PD_CRP,Fb,by = "ID",all.x = T)
load(file="./data_eCM/MORT_III.Rdata")
load(file="./data_eCM/MORT_CON1.Rdata")
load(file="./data_eCM/MORT_CON2.Rdata")
MORT_III$ID<-paste0("NHANES_III_",MORT_III$SEQN)
MORT_CON1$ID<-paste0("NHANES_CON1_",MORT_CON1$SEQN)
MORT_CON2$ID<-paste0("NHANES_CON2_",MORT_CON2$SEQN)
MORT<-rbind(MORT_III,MORT_CON1,MORT_CON2)
PD_MORT<-merge(PD_Fb,MORT,by = "ID",all.x = T)
PD_MORT$SEQN.x<-NULL
PD_MORT$SEQN.y<-NULL
table(PD_MORT$MORT_stat)

load(file="./data_eCM/Weight_III.Rdata")
load(file="./data_eCM/Weight_CON1.Rdata")
load(file="./data_eCM/Weight_CON2.Rdata")
Weight_III$ID<-paste0("NHANES_III_",Weight_III$SEQN)
Weight_CON1$ID<-paste0("NHANES_CON1_",Weight_CON1$SEQN)
Weight_CON2$ID<-paste0("NHANES_CON2_",Weight_CON2$SEQN)
Weight<-rbind(Weight_III,Weight_CON1,Weight_CON2)
PD_Weight<-merge(PD_Fb,Weight,by = "ID",all.x = T)
PD_Weight$SEQN.x<-NULL
PD_Weight$SEQN.y<-NULL
PD_Weight$Age[PD_Weight$Age>=80]<-80
PD_Weight$Age_status[PD_Weight$Age<45]<-"<45"
PD_Weight$Age_status[PD_Weight$Age>=45&PD_Weight$Age<65]<-"[45,65)"
PD_Weight$Age_status[PD_Weight$Age>=65]<-">=65"
PD_Weight$Age_status<-factor(PD_Weight$Age_status,
                            levels = c("<45","[45,65)",">=65"))
PD_Weight$PD_diagnosis[PD_Weight$PD_diagnosis=="Moderate/Severe periodontitis"]<-1
PD_Weight$PD_diagnosis[PD_Weight$PD_diagnosis=="No/Mild periodontitis"]<-0
PD_Weight$PD_diagnosis<-as.factor(PD_Weight$PD_diagnosis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =PD_Weight,strata=~sdmvstra,weights = ~ weight)
b<-svyglm(WBC ~PD_diagnosis+Age_status+Gender+Race_ethnicity+
            Smoking_status+Drinking_status+HEI+BMI_status,design =rhcSvy)
summary(b)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
load(file="./data_eCM/Interpolation_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
quantile<-svyquantile(~peryear, rhcSvy, c(0.5))
svyQuantile(~peryear,rhcSvy,C(0.5),ci = true)
