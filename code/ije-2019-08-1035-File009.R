#############################################################################
# TRIAL EMULATION: SURGERY WITHIN 6 MONTHS AMONG OLDER LUNG CANCER PATIENTS

# Author: Clemence Leyrat (from Camille Maringe's Stata code)
# Content: recoding the outcome and follow-up time for our emulated trial
#           creating the censoring indicators and time uncensored

#############################################################################


#Packages
library(survival) # For survival analysis
library(boot)
library(here)

tab<-read.csv(here("ije-2019-08-1035-File008.csv"),sep=",",header=T)
###############################################################################################
# VARIABLES

#id: patient identifier
#fup_obs: observed follow-up time (time to death or 1 year if censored alive)
#death: observed event of interest (all-cause death) 1: dead, 0:alive
#timetosurgery: time to surgery (NA if no surgery)
#surgery: observed treatment 1 if the patient received surgery within 6 month, 0 otherwise
#age: age at diagnosis
#sex: patient's sex
#perf: performance status at diagnosis
#stage: stage at diagnosis
#deprivation: deprivation score
#charlson: Charlson's comorbidity index
#emergency: route to diagnosis

#Dataset: these variables are in a dataframe called "tab" (unavailable)

###############################################################################################
  
  
  

   


  
  #Note: Letters A, B... refer to the scenarios identified in our graph
  #We will create two new variables:
      # - fup: the follow-up time in the emulated trial (which can be different from the observed follow-up time)
      # - outcome: the outcome in the emulated trial (which can be different from the observed follow-up time)
  
 #THESE VARIABLES WILL BE THE OUTCOME AND FOLLOW UP TIME IN THE OUTCOME MODEL

    #########################################################################################################################################
    #Arm "Control": no treatment within 6 months
    tab_control<-tab  # We create a first copy of the dataset: "clones" assigned to the control (no surgery) arm
    tab_control$arm<-"Control"
    
    #Case 1: Patients receive surgery within 6 months (scenarii A to E): 
    #they are still alive and followed-up until surgery
    tab_control$outcome[tab_control$surgery==1 & tab_control$timetosurgery <=182.62]<-0
    
    tab_control$fup[tab_control$surgery==1
                    & tab_control$timetosurgery <=182.62]<-tab_control$timetosurgery[tab_control$surgery==1
                                                                                      & tab_control$timetosurgery <=182.62]
    
    #Case 2: Patients do not receive surgery within 6 months (either no surgery or surgery after 6 months): 
    #p (scenarii F to M)
    tab_control$outcome[tab_control$surgery==0 
                        | (tab_control$surgery==1 
                          & tab_control$timetosurgery >182.62)]<-  tab_control$death[tab_control$surgery==0 
                                                                                     | (tab_control$surgery==1 
                                                                                        & tab_control$timetosurgery >182.62)]
    tab_control$fup[tab_control$surgery==0 
                    | (tab_control$surgery==1 
                       & tab_control$timetosurgery >182.62)]<-  tab_control$fup_obs[tab_control$surgery==0 
                                                                                      | (tab_control$surgery==1 
                                                                                         & tab_control$timetosurgery >182.62)]
    
    #########################################################################################################################################

  

    #########################################################################################################################################
    #Arm "Surgery": Surgery within 6 months
    tab_surgery<-tab # We create a second copy of the dataset: "clones" assigned to the surgery arm
    tab_surgery$arm<-"Surgery"
    
    #Case 1: Patients receive surgery within 6 months (scenarii A to E): 
    #we keep their observed outcomes and follow-up times
    tab_surgery$outcome[tab_surgery$surgery==1
                        & tab_surgery$timetosurgery <=182.62]<-tab_surgery$death[tab_surgery$surgery==1
                                                                                 & tab_surgery$timetosurgery <=182.62]
    
    tab_surgery$fup[tab_surgery$surgery==1
                    & tab_surgery$timetosurgery <=182.62]<-tab_surgery$fup_obs[tab_surgery$surgery==1
                                                                                 & tab_surgery$timetosurgery <=182.62]
    
    #Case 2: Patients die or are lost to follow-up before 6 months (scenarii K and L) without having surgery: 
    #we keep their observed outcomes and follow-up times
    tab_surgery$outcome[tab_surgery$surgery==0 
                        & tab_surgery$fup_obs <=182.62]<-tab_surgery$death[tab_surgery$surgery==0 
                                                                          & tab_surgery$fup_obs <=182.62]
    
    tab_surgery$fup[tab_surgery$surgery==0 
                    & tab_surgery$fup_obs <=182.62]<-tab_surgery$fup_obs[tab_surgery$surgery==0 
                                                                          & tab_surgery$fup_obs <=182.62]
    
    #Case 3: Patients do not receive surgery within 6 months and are still alive or 
    #at risk at 6 months (scenarii F-J and M): 
    # they are considered alived and their follow-up time is 6 months
    tab_surgery$outcome[(tab_surgery$surgery==0 & tab_surgery$fup_obs >182.62)
                        | (tab_surgery$surgery==0  & tab_surgery$timetosurgery >182.62)]<-0
    
    tab_surgery$fup[(tab_surgery$surgery==0 & tab_surgery$fup_obs >182.62)
                    | (tab_surgery$surgery==0 & tab_surgery$timetosurgery >182.62)]<-182.62
    
    #########################################################################################################################################
    

  
  
  
  #################################################################
  # STEP1-CLONING: CENSORING STATUS AND FOLLOW-UP TIME UNCENSORED
  #################################################################
  
  
  #Note: Letters A, B... refer to the scenarios identified in our graph
  #We will create two new variables:
  # - fup_uncensored: the follow-up time uncensored in the trial arm (can be shorter than the follow-up time in the outcome model)
  # - censoring: a binary variable indicating wheter the patient was censored in a given arm (either because they receive surgery 
  #              in the control arm or they didn't receive surgery in the surgery arm)
  
  #THESE VARIABLES WILL BE THE OUTCOME AND FOLLOW UP TIME IN THE WEIGHT MODEL
  
  
    #########################################################################################################################################
    #Arm "Control": no treatment within 6 months
   
    #Case 1: Patients receive surgery within 6 months (scenarii A to E): 
    #they are censored in the control group at time of surgery
    tab_control$censoring[tab_control$surgery==1 & tab_control$timetosurgery <=182.62]<-1
    
    tab_control$fup_uncensored[tab_control$surgery==1 
                    & tab_control$timetosurgery <=182.62]<-(tab_control$timetosurgery[tab_control$surgery==1 
                                                                                   & tab_control$timetosurgery <=182.62])
    
    #Case 2: Patients die or are lost to follow-up before 6 months (scenarii K and L): 
    #we keep their follow-up time but they are uncensored
    tab_control$censoring[tab_control$surgery==0 & tab_control$fup_obs <=182.62]<-0
    
    tab_control$fup_uncensored[tab_control$surgery==0 & 
                                 tab_control$fup_obs <=182.62]<-tab_control$fup_obs[tab_control$surgery==0 & 
                                                                                        tab_control$fup_obs <=182.62]
    
    #Case 3: Patients do not receive surgery within 6 months and are still alive or 
    #at risk at 6 months (scenarii F-J and M): 
    # they are considered uncensored and their follow-up time is 6 months
    tab_control$censoring[(tab_control$surgery==0 & tab_control$fup_obs >182.62)
                        | (tab_control$surgery==1  & tab_control$timetosurgery >182.62)]<-0
    
    tab_control$fup_uncensored[(tab_control$surgery==0 & tab_control$fup_obs >182.62)
                               | (tab_control$surgery==1  & tab_control$timetosurgery >182.62)]<- 182.62
    
  
    
    #########################################################################################################################################
    
    
    #########################################################################################################################################
    #Arm "Surgery": Surgery within 6 months
    
    #Case 1: Patients receive surgery within 6 months (scenarii A to E): 
    # they are uncensored in the surgery arm and remain at risk of 
    # censoring until time of surgery
    tab_surgery$censoring[tab_surgery$surgery==1 & tab_surgery$timetosurgery <=182.62]<-0
    
    tab_surgery$fup_uncensored[tab_surgery$surgery==1 
                               & tab_surgery$timetosurgery <=182.62]<-(tab_surgery$timetosurgery[tab_surgery$surgery==1 
                                                                                              & tab_surgery$timetosurgery <=182.62])
    
    #Case 2: Patients die or are lost to follow-up before 6 months (scenarii K and L): 
    #we keep their follow-up times but they are uncensored
    tab_surgery$censoring[tab_surgery$surgery==0 & tab_surgery$fup_obs <=182.62]<-0
    
    tab_surgery$fup_uncensored[tab_surgery$surgery==0 
                               & tab_surgery$fup_obs <=182.62]<-tab_surgery$fup_obs[tab_surgery$surgery==0  
                                                                                     & tab_surgery$fup_obs <=182.62]
    
    #Case 3: Patients do not receive surgery within 6 months and are still alive 
    #or at risk at 6 months (scenarii F-J and M): 
    # they are considered censored and their follow-up time is 6 months
    tab_surgery$censoring[(tab_surgery$surgery==0 & tab_surgery$fup_obs >182.62)
                          | (tab_surgery$surgery==1  & tab_surgery$timetosurgery >182.62)]<-1
    
    tab_surgery$fup_uncensored[(tab_surgery$surgery==0 & tab_surgery$fup_obs >182.62)
                               | (tab_surgery$surgery==1  & tab_surgery$timetosurgery >182.62)]<-182.62
    
    
    #########################################################################################################################################
    
  
    
    #Each patient appears twice in this dataset (a clone in each treatment arm)
    #Combining the two datasets (Control and Surgery)
    tab<-rbind(tab_control, tab_surgery)
    
    
    
    # Bootstrap function
    fboot <- function(tab, indices) {
      t<-tab[tab$arm=="Control",]
      t1<-tab[tab$arm=="Surgery",]
      tab0 <- t[indices,] # allows boot to select sample
      select<-tab0$id
      tab1<-t1[t1$id %in% select,] 
      tab<-rbind(tab0,tab1)

 
      ####################################################
      #STEP 2-SPLITTING THE DATASET AT EACH TIME OF EVENT
      ####################################################
      
      
      #Dataframe containing the time of events and an ID for the times of events
      t_events<-sort(unique(tab$fup))
      times<-data.frame("tevent"=t_events,"ID_t"=seq(1:length(t_events)))
      
      
      ####################################
      # ArRM "Surgery" FIRST
      ####################################
      
      
      tab_s<-tab[tab$arm=="Surgery",]
      
      #Creation of the entry variable (Tstart, 0 for everyone)
      tab_s$Tstart<-0
    
      #Splitting the dataset at each time of event until the event happens and sorting it
      data.long<-survSplit(tab_s, cut=t_events, end="fup", 
                           start="Tstart", event="outcome",id="ID") 
      data.long<-data.long[order(data.long$ID,data.long$fup),] 
      
      #Splitting the original dataset at each time of event and sorting it
      #until censoring happens. This is to have the censoring status at each time of event 
      data.long.cens<-survSplit(tab_s, cut=t_events, end="fup", 
                                start="Tstart", event="censoring",id="ID") 
      data.long.cens<-data.long.cens[order(data.long.cens$ID,data.long.cens$fup),] 
      
      #Replacing the censoring variable in data.long by the censoring variable obtained
      # in the second split dataset
      data.long$censoring<-data.long.cens$censoring
      
      #Creating Tstop (end of the interval) 
      data.long$Tstop<-data.long$fup
      
      #Merge and sort
      data.long<-merge(data.long,times,by.x="Tstart",by.y="tevent",all.x=T)
      data.long<-data.long[order(data.long$ID,data.long$fup),] 
      data.long$ID_t[is.na(data.long$ID_t)]<-0
      
    
      
      ####################################
      # ARM "No Surgery" NOW
      ###################################
      
      
      tab_c<-tab[tab$arm=="Control",]
      
      #Creation of the entry variable (Tstart, 0 for everyone)
      tab_c$Tstart<-0
      
      
      #Splitting the dataset first at each time of event
      #until the event happens 
      data.long2<-survSplit(tab_c, cut=t_events, end="fup", 
                            start="Tstart", event="outcome",id="ID") 
      data.long2<-data.long2[order(data.long2$ID,data.long2$fup),] 
      
      #Splitting the original dataset at each time of event
      #until censoring happens 
      data.long.cens2<-survSplit(tab_c, cut=t_events, end="fup", 
                                 start="Tstart", event="censoring",id="ID") 
      data.long.cens2<-data.long.cens2[order(data.long.cens2$ID,data.long.cens2$fup),] 
      
      
      #Replacing the censoring variable in data.long by the censoring variable obtained
      # in the second split dataset
      data.long2$censoring<-data.long.cens2$censoring
      
      #Creating Tstop (end of the interval)
      data.long2$Tstop<-data.long2$fup
    
      #Merge and sort
      data.long2<-merge(data.long2,times,by.x="Tstart",by.y="tevent",all.x=T)
      data.long2<-data.long2[order(data.long2$ID,data.long2$fup),] 
      data.long2$ID_t[is.na(data.long2$ID_t)]<-0
      
      #Final dataset
      data<-rbind(data.long,data.long2)
      data_final<-merge(data,times,by="ID_t",all.x=T)
      data_final<-data_final[order(data_final$ID,data_final$fup),]
    
      
      ############################################
      #STEP 3- ESTIMATING THE CENSORING WEIGHTS
      ############################################
      
      #######################################################################################################################
      # Arm "Surgery" first
      
      data.long<-data_final[data_final$arm=="Surgery",]
      
      ###########################
      # STEP 1: censoring model
      ###########################
   
  
      #Cox model
      ms_cens<-coxph(Surv(Tstart, Tstop, censoring)~age+sex+emergency+
                       stage+deprivation+charlson+perf, ties="efron", data=data.long) #We can also includes their interactions
  
      
      ###########################################################
      # Estimating the probability of remaining uncensored
      ###########################################################
      
      #Design matrix
      design_mat<-as.matrix(data.long[,c("age","sex","emergency",
                                         "stage","deprivation",
                                         "charlson","perf")])
      #Vector of regression coefficients
      beta<-coef(ms_cens)
      
      #Calculation of XB (linear combineation of the covariates)
      data.long$lin_pred<-design_mat%*%beta
      
      #Estimating the cumulative hazard (when covariates=0)
      dat.base<-data.frame(basehaz(ms_cens,centered=F))
      names(dat.base)<-c("hazard","t")
      dat.base<-unique(merge(dat.base,times,by.x="t",by.y="tevent",all.x=T))
      
      
      #Merging and reordering the dataset
      data.long<-merge(data.long,dat.base,by="ID_t",all.x=T)
      data.long<-data.long[order(data.long$id,data.long$fup),]
      data.long$hazard<-ifelse(is.na(data.long$hazard),0,data.long$hazard)
      
      
      #Estimating the probability of remaining uncensored at each time of event
      data.long$P_uncens<-exp(-(data.long$hazard)*exp(data.long$lin_pred))  
      
      
      #############################
      # Computing IPC weights
      #############################
      
      #Weights are the inverse of the probability of remaining uncensored
      data.long$weight_Cox<-1/data.long$P_uncens
      ####################################################################################################################
      
      ####################################
      # Arm "No Surgery" now
      
      data.long2<-data_final[data_final$arm=="Control",]
      
      ###########################
      # STEP 1: censoring model
      ###########################
  
      
      #Cox model
      ms_cens2<-coxph(Surv(Tstart, Tstop, censoring)~age+sex+emergency+
                        stage+deprivation+charlson+perf, ties="efron", data=data.long2)
      summary(ms_cens2)
      
      
      #As expected from the standardised differences, all the variables except sex and CM
      # are predictor of censoring. Note interaction arm*CM for censoring
      
      
      
      ###########################################################
      # STEP 2: estimate the probability of remaining uncensored
      ###########################################################
      
      #Design matrix
      design_mat2<-as.matrix(data.long2[,c("age","sex","emergency",
                                           "stage","deprivation",
                                           "charlson","perf")])
      #Vector of regression coefficients
      beta2<-coef(ms_cens2)
      
      #Calculation of XB (linear combineation of the covariates)
      data.long2$lin_pred<-design_mat2%*%beta2
      
      #Estimating the cumulative hazard (when covariates=0)
      dat.base2<-data.frame(basehaz(ms_cens2,centered=F))
      names(dat.base2)<-c("hazard","t")
      
      
      dat.base2<-unique(merge(dat.base2,times,by.x="t",by.y="tevent",all.x=T))
      
      #Merging and reordering the dataset
      data.long2<-merge(data.long2,dat.base2,by="ID_t",all.x=T)
      data.long2<-data.long2[order(data.long2$id,data.long2$fup),]
      data.long2$hazard<-ifelse(is.na(data.long2$hazard),0,data.long2$hazard)
      
      
      #Estimating the probability of remaining uncensored at each time of event
      data.long2$P_uncens<-exp(-(data.long2$hazard)*exp(data.long2$lin_pred))
      
      
      
      #############################
      # Computing the IPC weights
      #############################
      
      #Weights are the inverse of the probability of remaining uncensored
      data.long2$weight_Cox<-1/data.long2$P_uncens
      data.long2$weight_Cox[data.long2$ID_t==0]<-1
      
      data.long.Cox<-rbind(data.long,data.long2)
      
      
      
      ############################################
      #STEP 3- ESTIMATING THE SURVIVOR FUNCTION
      ############################################
      
      ##################################################
      # Emulated trial with Cox weights (Kaplan-Meier)
      ##################################################
      
      emul_Cox_s <- survfit(Surv(Tstart, Tstop, outcome) ~ 1,
                            data=data.long.Cox[data.long.Cox$arm=="Surgery",],weights = weight_Cox)
      S1<-min(emul_Cox_s$surv) #1 year survival in the surgery group
      fit.tableM <- summary(emul_Cox_s, rmean=365)$table
      RMST1 <- fit.tableM["*rmean"] # Estimated RMST in the surgery group
      
      emul_Cox_c <- survfit(Surv(Tstart, Tstop, outcome) ~ 1,
                            data=data.long.Cox[data.long.Cox$arm=="Control",],weights = weight_Cox)
      S0<-min(emul_Cox_c$surv) #1 year survival in the control group
      fit.tableM2 <- summary(emul_Cox_c, rmean=365)$table
      RMST0 <- fit.tableM2["*rmean"] # Estimated RMST in the control group
      
      Diff_surv<-S1-S0 #Difference in 1 year survival
      Diff_RMST<-RMST1-RMST0 #Difference in RMST
      
      ##################################################
      # Emulated trial with Cox weights (Cox model)
      ##################################################
      
      Cox_w <- coxph(Surv(Tstart,Tstop, outcome) ~ arm,
                     data=data.long.Cox, weights=weight_Cox)
      HR<-exp(Cox_w$coefficients) #Hazard ratio
      
  
      res<-c(Diff_surv,Diff_RMST,HR)
      names(res)<-c("Diff_surv","Diff_RMST","HR")
  
      return(res)
    
    }
    

  ##############################################
  # CALLING THE BOOTSTRAP FUNCTION
  ##############################################
    
    # Bootstrapping with 1000 replications
    results <- boot(data=tab, statistic=fboot,R=100)
    results
    # 95% confidence intervals for each measure
    boot.ci(results,type="norm",index=1) #Difference in survival
    boot.ci(results,type="norm",index=2) #Difference in RMST
    boot.ci(results,type="norm",index=3) #HR
    
