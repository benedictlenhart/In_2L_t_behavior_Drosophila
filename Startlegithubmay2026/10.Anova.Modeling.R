#bind together some different data
library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)
library(readxl)
library(foreach)

#activity and startle response data
mindata = readRDS("activity.startle.phenotypesFINAL")
#sleep data
sleepdata = readRDS("sleepdataFINAL")
#position near food data
phendata = readRDS("positionsFINAL")

#make column for unique days
mindata = mindata %>% 
  rename("experiment"="exp",
         "temperature"="temp",
         "inversion_genotype"="inv.st") 
mindata[, day := fifelse(
  grepl("DAY", flyidfull),
  paste(experiment, str_extract(flyidfull, "(?<=DAY)\\d+"), sep = "_"),
  paste(experiment, str_extract(flyidfull, "(?<=Day)\\d+"), sep = "_")
)]

# sleepdata: handles both "1_Fly#1-DAY1" and "Fly_1_Day2" formats

sleepdata[, day := fifelse(
  grepl("DAY", flyid.total),
  paste(exp, str_extract(flyid.total, "(?<=DAY)\\d+"), sep = "_"),
  paste(exp, str_extract(flyid.total, "(?<=Day)\\d+"), sep = "_")
)]

# phendata: handles both "1_Fly#1-DAY1" and "Fly_1_Day2" formats
phendata[, day := fifelse(
  grepl("DAY", flyid),
  paste(exp, str_extract(flyid, "(?<=DAY)\\d+"), sep = "_"),
  paste(exp, str_extract(flyid, "(?<=Day)\\d+"), sep = "_")
)]


###################3
##anova statistiscs#
######################3
#we want to make anova models for each of the three traits represented here. 
library(lme4)

library(xtable)


#for each trait, try a sex-specific anova, followed by model comparision of null, inversion, inversion + temp, inversion +temp + interaction. 
#sex model 
nullmodel = lmer(mean.duration ~  1 + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 

Inversion_model = lmer(mean.duration ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 
Sex_model = lmer(mean.duration ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
Interaction_model= lmer(mean.duration ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
#we want to combine the results from each table

results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
r3 = as.data.table(results)
print(xtable(results), type = "html")
#temp models
nullmodel = lmer(mean.duration ~  1 + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Inversion_model = lmer(mean.duration ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Temperature_model = lmer(mean.duration ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)
Interaction_model= lmer(mean.duration ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)

#we want to combine the results from each table

results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t3 = as.data.table(results)
print(xtable(results), type = "html")
##############################################
#magnitude speed change

#sex model 
nullmodel = lmer(mean.peak ~  1 + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 
Inversion_model = lmer(mean.peak ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 
Sex_model = lmer(mean.peak ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
Interaction_model= lmer(mean.peak ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)

print(xtable(results), type = "html")
r4 = as.data.table(results)
#temp models
nullmodel = lmer(mean.peak ~  1 + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Inversion_model = lmer(mean.peak ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Temperature_model = lmer(mean.peak ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)
Interaction_model= lmer(mean.peak ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)
results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t4 = as.data.table(results)
print(xtable(results), type = "html")
##############################################
#speed at base



#sex model 
nullmodel = lmer(mean.base ~  1 + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 
Inversion_model = lmer(mean.base ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T) 
Sex_model = lmer(mean.base ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
Interaction_model= lmer(mean.base ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = mindata[temperature == 25], REML = T)
results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
r2 = as.data.table(results)
print(xtable(results), type = "html")
#temp models
nullmodel = lmer(mean.base ~  1 + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Inversion_model = lmer(mean.base ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T) 
Temperature_model = lmer(mean.base ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)
Interaction_model= lmer(mean.base ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = mindata[sex == "female"], REML = T)

results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t2 = as.data.table(results)
print(xtable(results), type = "html")
##############################################
#now for activity
sleepstat = sleepdata %>% 
  mutate(`Sleep Duration` =  `Sleep Duration`) %>% 
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st")



#sex model 
nullmodel = lmer(`Sleep Duration` ~  1 + (1| experiment) + (1|experiment:day), data = sleepstat[temperature == 25], REML = T) 
Inversion_model = lmer(`Sleep Duration` ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = sleepstat[temperature == 25], REML = T) 
Sex_model = lmer(`Sleep Duration` ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = sleepstat[temperature == 25], REML = T)
Interaction_model= lmer(`Sleep Duration` ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = sleepstat[temperature == 25], REML = T)
results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
r1 = as.data.table(results)
print(xtable(results), type = "html")
#temp models
nullmodel = lmer(`Sleep Duration` ~  1 + (1| experiment) + (1|experiment:day), data = sleepstat[sex == "female"], REML = T) 
Inversion_model = lmer(`Sleep Duration` ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = sleepstat[sex == "female"], REML = T) 
Temperature_model = lmer(`Sleep Duration` ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = sleepstat[sex == "female"], REML = T)
Interaction_model= lmer(`Sleep Duration` ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = sleepstat[sex == "female"], REML = T)

results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t1 = as.data.table(results)
print(xtable(results), type = "html")
##############################################
#foraging stats
phenstats = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>% #arcsin square root trasnorm proportions for use in t test
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st") %>% 
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 1) %>% 
  as.data.table(.)#remove zeros or 1s



#sex model 
nullmodel = lmer(f.ratio ~  1 + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T) 
Inversion_model = lmer(f.ratio ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T) 
Sex_model = lmer(f.ratio ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T)
Interaction_model= lmer(f.ratio ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T)
results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
r5 = as.data.table(results)
print(xtable(r7), type = "html")
#temp models
nullmodel = lmer(f.ratio ~  1 + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T) 
Inversion_model = lmer(f.ratio ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T) 
Temperature_model = lmer(f.ratio ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T)
Interaction_model= lmer(f.ratio ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T)

results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t5 = as.data.table(results)
print(xtable(t6), type = "html")
########################
#away from frood

phenstats = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>% #arcsin square root trasnorm proportions for use in t test
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st") %>% 
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 8) %>% 
  as.data.table(.)#remove zeros or 1s



#sex model 
nullmodel = lmer(f.ratio ~  1 + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T) 
Inversion_model = lmer(f.ratio ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T) 
Sex_model = lmer(f.ratio ~  inversion_genotype+sex + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T)
Interaction_model= lmer(f.ratio ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment) + (1|experiment:day), data = phenstats[temperature == 25], REML = T)
results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
r6 = as.data.table(results)
print(xtable(r7), type = "html")
#temp models
nullmodel = lmer(f.ratio ~  1 + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T) 
Inversion_model = lmer(f.ratio ~  inversion_genotype + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T) 
Temperature_model = lmer(f.ratio ~  inversion_genotype+temperature + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T)
Interaction_model= lmer(f.ratio ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment) + (1|experiment:day), data = phenstats[sex == "female"], REML = T)

results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
t6 = as.data.table(results)
print(xtable(t7), type = "html")
# ####################################33
# #magnitude change in activity rate
# 
# actSR = activitySR %>% 
#   rename("temperature"="temp",
#          "experiment"="exp",
#          "inversion_genotype"="inv.st")  
# 
# #sex model 
# nullmodel = lmer(mag ~  1 + (1| experiment), data = actSR[temperature == 25], REML = T) 
# Inversion_model = lmer(mag ~  inversion_genotype + (1| experiment), data = actSR[temperature == 25], REML = T) 
# Sex_model = lmer(mag ~  sex + (1| experiment), data = actSR[temperature == 25], REML = T)
# Interaction_model= lmer(mag ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment), data = actSR[temperature == 25], REML = T)
# results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
# results = anova(nullmodel, Sex_model)
# r6 = as.data.table(results)
# print(xtable(results), type = "html")
# #temp models
# nullmodel = lmer(mag ~  1 + (1| experiment), data = actSR[sex == "female"], REML = T) 
# Inversion_model = lmer(mag ~  inversion_genotype + (1| experiment), data = actSR[sex == "female"], REML = T) 
# Temperature_model = lmer(mag ~  inversion_genotype+temperature + (1| experiment), data = actSR[sex == "female"], REML = T)
# Interaction_model= lmer(mag ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment), data = actSR[sex == "female"], REML = T)
# 
# results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
# t6 = as.data.table(results)
# print(xtable(results), type = "html")
# ###########################################3
# #duration of elevated activity rate
# #sex model 
# nullmodel = lmer(duration ~  1 + (1| experiment), data = actSR[temperature == 25], REML = T) 
# Inversion_model = lmer(duration ~  inversion_genotype + (1| experiment), data = actSR[temperature == 25], REML = T) 
# Sex_model = lmer(duration ~  inversion_genotype+sex + (1| experiment), data = actSR[temperature == 25], REML = T)
# Interaction_model= lmer(duration ~  inversion_genotype + sex +  inversion_genotype * sex + (1| experiment), data = actSR[temperature == 25], REML = T)
# results = anova(nullmodel, Inversion_model, Sex_model, Interaction_model)
# r5 = as.data.table(results)
# print(xtable(results), type = "html")
# #temp models
# nullmodel = lmer(duration ~  1 + (1| experiment), data = actSR[sex == "female"], REML = T) 
# Inversion_model = lmer(duration ~  inversion_genotype + (1| experiment), data = actSR[sex == "female"], REML = T) 
# Temperature_model = lmer(duration ~  inversion_genotype+temperature + (1| experiment), data = actSR[sex == "female"], REML = T)
# Interaction_model= lmer(duration ~  inversion_genotype +temperature +  inversion_genotype * temperature + (1| experiment), data = actSR[sex == "female"], REML = T)
# results = anova(nullmodel, Inversion_model, Temperature_model, Interaction_model)
# t5 = as.data.table(results)
# print(xtable(results), type = "html")

#get data table for sex modeling
sex.table = rbind(r1, r2, r3, r4,  r5, r6)
Trait = rep(c("Sleep","Base Speed","Startle Duration", "Startle Magnitude",  "Prop_nearfood","Prop_farfood"), each = 4)
Model = rep(c("Null Model","Inversion Model","Sex Model","Interaction Model"), times = 6)
sex.table = cbind(Trait, Model, sex.table)
library(openxlsx)
write.xlsx(sex.table, file = "sex.models.xlsx", sheetName = "Sheet1", overwrite = TRUE)
print(xtable(sex.table), type = "html")

temp.table = rbind(t1, t2, t3, t4,  t5, t6)

Trait = rep(c("Sleep","Base Speed","Startle Duration", "Startle Magnitude",  "Prop_nearfood","Prop_farfood"), each = 4)
Model = rep(c("Null Model","Inversion Model","Temp Model","Interaction Model"), times = 6)
temp.table = cbind(Trait, Model, temp.table)

write.xlsx(temp.table, file = "temp.models.xlsx", sheetName = "Sheet1", overwrite = TRUE)
print(xtable(sex.table), type = "html")