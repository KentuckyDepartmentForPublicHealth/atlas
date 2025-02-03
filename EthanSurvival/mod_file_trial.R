## Survial model trial

#################PKG Loading######
library(survival)
library(survminer) 
library(ggplot2) 
##################################


###basing first models on Mistry's Prior work to get a working sample
##general formula was Survfit(as.numeric(survtime,status)~strata)
##also before data used was subsetted based upon a specific diagnosis


###########Data loading#########
 load("~/Desktop/Survival Intern Project/Survival/atlas/dat/atlasDataClean.RData")

################################

##########Model Creation#######
#creating first model replicating mistrys model for sake of simplicity for working example
#will extend from this point once working
 

#d<-read.csv("Atlas_data.csv", header=T)
#d<-d[d$Final.diagnosis=="PA"|d$Final.diagnosis=="PXA"|d$Final.diagnosis=="Angiocentric glioma"|d$Final.diagnosis=="Germ Cell Tumor",]
#fit<- survfit(Surv(as.numeric(Overall.survival..months.), Vital.status..1.dead..0.alive.) ~ Final.diagnosis, data = d)
#pa<-ggsurvplot(fit, risk.table = F, legend='none', xlab="Time (Months)")$plot+
 # annotate("text", x=45, y=.75, label= "PA (n=21)",hjust = 0)+
 # annotate("text", x=45, y=.15, label= "PXA (n=3)",hjust = 0)+
# annotate("text", x=45, y=1, label= "GCT (n=6)",hjust = 0)

####Getting complete cases for survival######
atlasDataClean_2 <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA")

#surv model
fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ grade , 
               data = atlasDataClean_2)



ggsurvplot(fit, risk.table = F, legend='right', xlab="Time (Months)")
