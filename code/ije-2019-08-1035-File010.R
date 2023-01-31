tab0<-read.csv("H:/Research/CSG/Trial_emulation/Code paper/Fake data/Data_Rcode.csv", sep=",",header=T)

tab<-unique(tab0[,])

#New fake ID, but we keep the distribution of surgery, vital status and time to surgery + 5 of the 7 covariates
set.seed(123)
id<-paste(rep("P",2309),sample(1000:9999, size=2309, replace=FALSE),sep="")
tab_fake0<-data.frame("id"=id)
tab_fake0<-cbind(tab_fake0,tab[,c("surgery", "timetosurgery", "death",
                                   "sex","charlson","perf","stage","emergency")])
        )

#Now we split by exposure
tab_fake_s<-tab_fake0[tab_fake0$surgery==1,]
tab_fake_c<-tab_fake0[tab_fake0$surgery==0,]

tab_s<-tab[tab$surgery==1,]
tab_c<-tab[tab$surgery==0,]

#If patients don't die, fup=1 year
tab_fake_s$fup_obs[tab_fake_s$death==0]<-365.24
tab_fake_c$fup_obs[tab_fake_c$death==0]<-365.24

#Fup time for people dying in the control group
tab_fake_c$fup_obs[tab_fake_c$death==1]<-sample(tab_c$fup_obs[tab_c$death==1],
                                                size=length(tab_c$fup_obs[tab_c$death==1]),replace=F)

#Fup time for people dying in the surgery group has to be longer than the time to surgery
for (i in (1:nrow(tab_fake_s))){
  fup<-0
  if (tab_fake_s$death[i]==1){
    while (fup<=tab_fake_s$timetosurgery[i]){
      fup<-sample(tab_s$fup_obs[tab_s$death==1], size=1, replace=T)
    }
    tab_fake_s$fup_obs[i]<-fup}
}


#We shuffle the values per group for age and deprivation
set.seed(345)
tab_fake_s$age<-sample(tab_s$age,size=nrow(tab_s),replace=F)
tab_fake_s$deprivation<-sample(tab_s$deprivation,size=nrow(tab_s),replace=F)

tab_fake_c$age<-sample(tab_c$age,size=nrow(tab_c),replace=F)
tab_fake_c$deprivation<-sample(tab_c$deprivation,size=nrow(tab_c),replace=F)

tab_fake<-rbind(tab_fake_c,tab_fake_s)

sample_id<-sample(tab_fake$id,size=200,replace=F)

tab_fake_final<-tab_fake[tab_fake$id %in% sample_id,]
tab_fake_final$timetosurgery[tab_fake_final$timetosurgery==0]<-1

write.csv(tab_fake_final,"H:/Research/CSG/Trial_emulation/Code paper/Fake data/FakeData_Rcode.csv",row.names=F)