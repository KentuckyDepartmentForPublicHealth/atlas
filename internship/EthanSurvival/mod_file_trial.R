## Survial model trial

#################PKG Loading######
library(survival)
library(survminer) 
library(ggplot2) 
library(dplyr)
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
atlasDataClean <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA")

#surv model
fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ grade , 
               data = atlasDataClean)



ggsurvplot(
  fit,
  data = atlasDataClean,
  risk.table = TRUE)


####creating my own model, looking at survival stratified by grade w in those w diagnosis
###CNS #####
atlasDataClean_sub <- subset(atlasDataClean, atlasDataClean$diagnosis == "CNS")

fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ grade , 
               data = atlasDataClean)
ggsurvplot(fit, risk.table = F, legend='right', xlab="Time (Months)", title = "Survival (Months) 
           Among CNS Diagnosis Stratified by Grade")









############TRYING ALTERNATIVE METHOD FOR PLOTTING KP CURVES##################
####converting surv obj to df allows for more graph control/customization easily via
###using ggplot2 directly#################

# Example survival model (stratified by 4 groups)
fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ grade , 
               data = atlasDataClean) 

# Convert survfit object to tidy data
surv_df <- as.data.frame(summary(fit)[c("time", "surv", "strata", "lower", "upper")])

# Rename columns for convenience
colnames(surv_df) <- c("time", "survival", "group", "lower", "upper")



# Create the Kaplan-Meier plot using ggplot
ggplot(surv_df, aes(x = time, y = survival, color = group, fill = group)) +
  geom_step(size = 1.2) +  # Kaplan-Meier survival curve
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(
    title = "Survival Probability (Months) ",
    x = "Time (Months)",
    y = "Survival Probability",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )


###median surv time######
##for whole survival table 
summary(fit)$table


## for just median survival times 
summary(fit)$table[, "median"]





######COX PH FOR HRs####
cox_model <- coxph(Surv(time = as.numeric(survivalMonths), event = mortality) ~ grade, 
                   data = atlasDataClean)
summary(cox_model)

hr_values <- exp(coef(cox_model))

# View the HRs
print(hr_values)

###R treats first categorical level as ref group, so HRs are against ref grade of 3




#########################################################################################
#NEW MOD BELOW- CHECKING STRATA OF AGE GROUPS 20-40,40-60,60-80,80+

fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ ageGroup , 
               data = atlasDataClean)

ggsurvplot(fit, risk.table = T, legend='right', xlab="Time (Months)")

cox_mod <- coxph(Surv(time = as.numeric(survivalMonths), event = mortality) ~ ageGroup, 
                 data = atlasDataClean)
print(exp(coef(cox_mod)))
##When plotted its visually cluttered due to 8 groups, maybe add option to select certain groups? 

####CHANGING REF GROUP, provides more meaningful output        b#######
# Sample data
atlasDataClean$ageGroup <- factor(atlasDataClean$ageGroup)
# Change the reference group to "ageGroup40-60YRS"
atlasDataClean$ageGroup <- relevel(atlasDataClean$ageGroup, ref = "40-60YRS")
fit <- survfit(Surv(time = as.numeric(survivalMonths), event = mortality) ~ ageGroup , 
               data = atlasDataClean)
cox_mod <- coxph(Surv(time = as.numeric(survivalMonths), event = mortality) ~ ageGroup, 
                 data = atlasDataClean)
print(exp(coef(cox_mod)))


