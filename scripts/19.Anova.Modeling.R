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

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/")
exp7data = readRDS("exp7data")
exp6data = readRDS("exp6data.2")

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")


#saveRDS(mergedata4, "simplephenodata")
#merge in modeling data for lrc
exp10data = readRDS("simplephenodata")
setwd("C:/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/dartdata")
sleepdata = readRDS("sleepdatacompiledalltemp")
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Feb_2025_objects/")


phendata = as.data.table(readRDS("fullfeed"))
# exp10data %>% 
#   group_by(hour) %>% 
#   summarize(mean.act = mean(activity)) %>% 
#   ggplot(
#     aes( x = hour, y = mean.act)
#   ) + geom_point()
# 
# ggplot(exp10data, aes(hour, activity)) + geom_point()
exp10fixed = exp10data %>% 
  select(flyid, temp, intensity, inv.st, genotype,base.act,duration, scaled.act ) %>% 
  mutate(inv.st = case_when(grepl("K1.S", inv.st) == T ~ "K3.Standard",
                            grepl("K2.I", inv.st) == T ~ "K1.Inverted",
                            grepl("K3.het", inv.st) == T ~ "K2.Inv_Std")) %>% 
  mutate(exp = 10,
         sex = "female")
#fix exp7 and exp 6 data
# exp6fixed = dcast(exp6data, flyid + sex + inv.st + geno ~ phenotypes, value.var = "value")
exp6fixed = exp6data %>% 
  #filter(sex == "female") %>% 
  select(-flyid) %>% 
  mutate(inv.st = case_when(grepl("K3.Homozygous.Standard", inv.st) == T ~ "K3.Standard",
                            grepl("K1.Homozygous-Inverted", inv.st) == T ~ "K1.Inverted",
                            grepl("K2.Heterozygous", inv.st) == T ~ "K2.Inv_Std")) %>% 
  rename("base.act" = "basal.act",
         "genotype" = "geno",
         "flyid" = "flyid.total") %>% 
  select(flyid, inv.st, genotype, base.act, scaled.act, duration, temp, intensity,sex) %>% 
  # mutate(temp = 25) %>% 
  # mutate(intensity = 100) %>% 
  mutate(exp = 6)
#mergedata4 = na.omit(moddata)
# exp7fixed = dcast(exp7data, flyid + inv.st + genotype + temp ~ phenotypes, value.var = "value")
exp7fixed = exp7data %>% 
  
  rename("base.act" = "basal.act") %>% 
  select(flyid, inv.st, temp,genotype, base.act, scaled.act, duration, temp, intensity) %>% 
  mutate(inv.st = case_when(grepl("homozygous.standard", inv.st) == T ~ "K3.Standard",
                            grepl("homozygous.inverted", inv.st) == T ~ "K1.Inverted",
                            grepl("heterozygous", inv.st) == T ~ "K2.Inv_Std")) %>% 
  mutate(exp = 7,
         sex = "female")
#try merging
talldata = rbind(exp10fixed, exp6fixed, exp7fixed)



talldata$inv.st = gsub("K1", "interi", talldata$inv.st)
talldata$inv.st = gsub("K3", "inters", talldata$inv.st)


talldata$inv.st = gsub("interi", "K3", talldata$inv.st)
talldata$inv.st = gsub("inters", "K1", talldata$inv.st)

mindata = talldata%>%
  mutate(intensity = as.numeric(as.character(intensity))) %>%
  mutate(stimili = case_when( intensity < 40 ~ "Low Intensity",
                              T ~ "High Intensity")) %>% 
  filter(stimili == "High Intensity") %>% 
  mutate(flyidfull = paste(flyid,exp, sep = "_")) %>%
  group_by(flyidfull,exp,temp, stimili,inv.st,sex) %>%
  summarise(mean.duration = mean(duration),
            mean.peak = mean(scaled.act),
            mean.base = mean(base.act)) %>%
  as.data.table(.)

###################3
##anova statistiscs#
######################3
#we want to make anova models for each of the three traits represented here. 
library(lme4)
 library(lmerTest)

library(xtable)
mindata = mindata %>% 
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st")
dur.model = lmer(mean.duration ~ sex + temperature + inversion_genotype + (1| experiment), data = mindata, REML = T) 
# #try just a sex test model
# dur.model = lmer(mean.duration ~ sex  + inv.st + (1| exp), data = mindata[temp == 25], REML = T) 
# dur.model = lmer(mean.duration ~ temp + inv.st + (1| exp), data = mindata[sex == "female"], REML = T) 
results = anova(dur.model)
print(xtable(results), type = "html")

mag.model = lmer(mean.peak ~ sex  + inversion_genotype + temperature + (1| experiment), data = mindata, REML = T) 

result = anova(mag.model)
print(xtable(result), type = "html")

speed.model = lmer(mean.base ~ sex  + inversion_genotype + temperature + (1| experiment), data = mindata, REML = T) 

result = anova(speed.model)
print(xtable(result), type = "html")
#now for activity
sleepstat = sleepdata %>% 
  mutate(`Sleep Duration` = 60 - `Sleep Duration`) %>% 
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st")
activity.model = lmer(`Sleep Duration` ~ sex  + inversion_genotype + temperature + (1| experiment), data = sleepstat, REML = T) 

result = anova(activity.model)
print(xtable(result), type = "html")
#############################
#near food
phenstats = phendata %>% 
  mutate(f.ratio = f.ratio + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
    f.ratio = asin(sqrt(f.ratio)) 
    )%>% #arcsin square root trasnorm proportions for use in t test
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st") %>% 
  filter(! f.ratio %in% c("-Inf","Inf"))#remove zeros or 1s

for.model = lmer(f.ratio ~ sex  + inversion_genotype + temperature + (1| experiment), data = phenstats, REML = T) 

result = anova(for.model)
print(xtable(result), type = "html")
###########################
#away from food

phenstats = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>% #arcsin square root trasnorm proportions for use in t test
  rename("temperature"="temp",
         "experiment"="exp",
         "inversion_genotype"="inv.st") %>% 
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 8)#remove zeros or 1s

for.model = lmer(f.ratio ~ sex  + inversion_genotype + temperature + (1| experiment), data = phenstats, REML = T) 

result = anova(for.model)
print(xtable(result), type = "html")
##############
##statistics##
##############
#mergetogether our data
head(mindata)
minfixed = mindata %>% 
  pivot_longer(cols = c(mean.duration, mean.peak, mean.base), names_to = "Phenotype", values_to = "values") %>% 
  select(c(flyidfull, temp, inv.st, sex, Phenotype, values))
head(sleepdata)
sleepfixed = sleepdata %>% 
  mutate(`Sleep Duration` = 60 - `Sleep Duration`) %>% 
  mutate(flyidfull = paste(flyid.total, exp, sep = "_"),
         Phenotype = "Activity") %>% 
  rename("values"="Sleep Duration") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype))
head(phendata)
foodfixed = phendata %>% 
  mutate(f.ratio = f.ratio + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>%
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 1) %>% #remove zeros or 1s) %>% #remove zeros or 1s

  mutate(flyidfull = paste(flyid, exp, sep = "_"),
         Phenotype = "Foraging") %>% 
  rename("values"="f.ratio") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype))

foodaway = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>%
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 8) %>% #remove zeros or 1s
  
  mutate(flyidfull = paste(flyid, exp, sep = "_"),
         Phenotype = "awayfood") %>% 
  rename("values"="f.ratio") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype,exp))
all.phen = rbind(minfixed, sleepfixed, foodfixed)
all.phen = all.phen %>% 
  mutate(inv.st = case_when(inv.st == "heterozygous" ~ "K2.Inv_Std",
                            inv.st == "K2.Inv_Std" ~ "K2.Inv_Std",
                            inv.st == "K2.Heterozygous" ~ "K2.Inv_Std",
                            inv.st == "homozygous.inverted" ~ "K3.Inverted",
                            inv.st == "K3.Inverted" ~ "K3.Inverted",
                            inv.st == "homozygous.standard" ~ "K1.Standard",
                            inv.st == "K1.Standard" ~ "K1.Standard")) %>% 
  as.data.table(.)
#add sleep info 

saveRDS(all.phen, "sr.stats.data3")
all.phen = readRDS("sr.stats.data2")
#we want to do t.tests bewteen each karyotype, for each phenotype, for each temperuture, for each sex. 
#make a ref.table
ref.table = expand.grid(c("male","female"), unique(all.phen$temp), unique(all.phen$Phenotype))
#find difference between genotypes in every condition for each trait
out = foreach(f = c(1:dim(ref.table)[1]), .errorhandling = "remove") %do% {
  #f =30
  ref.info = ref.table[f,]
  dt = all.phen[temp == unlist(ref.info[1,2])][Phenotype == unlist(ref.info[1,3])][sex == unlist(ref.info[1,1])]
  # model.add = lm(value ~ def.id + inv.st , data = dt)
  # model.mult = lm(value ~ def.id * inv.st , data = dt)
  # afit = anova(model.add, model.mult)
  I.S = t.test(dt[inv.st == "K1.Standard"]$values, dt[inv.st == "K3.Inverted"]$values)
  H.S = t.test(dt[inv.st == "K1.Standard"]$values, dt[inv.st == "K2.Inv_Std"]$values)
  H.I = t.test(dt[inv.st == "K2.Inv_Std"]$values, dt[inv.st == "K3.Inverted"]$values)
  df = data.frame(
    temp =  unlist(ref.info[1,2]),
    phenotype = unlist(ref.info[1,3]),
    sex = unlist(ref.info[1,1]),
    comparison = c("I/S", "H/S","H/I"),
    t = c(I.S$statistic,H.S$statistic,H.I$statistic),
    df = c(I.S$parameter,H.S$parameter,H.I$parameter),
    p = c(I.S$p.value,H.S$p.value,H.I$p.value)
    
  )
  df
}
stats.out = rbindlist(out)

sig = stats.out %>% 
  filter(p < 0.05) %>% 
  as.data.table(.)
sig
data = na.omit( all.phen[sex == "female"][temp == 25][Phenotype == "mean.base"])

x =aov(values ~ inv.st, data = data)
#now check for differces between sex, grouping across genotypes
ref.table = expand.grid( unique(all.phen$temp), unique(all.phen$Phenotype))

#find difference between genotypes in every condition for each trait
out = foreach(f = c(1:dim(ref.table)[1]), .errorhandling = "remove") %do% {
  #f = 10
  ref.info = ref.table[f,]
  dt = all.phen[temp == unlist(ref.info[1,1])][Phenotype == unlist(ref.info[1,2])][temp == 25]
  # model.add = lm(value ~ def.id + inv.st , data = dt)
  # model.mult = lm(value ~ def.id * inv.st , data = dt)
  # afit = anova(model.add, model.mult)
  I = t.test(dt[sex == "female"]$values, dt[sex == "male"]$values)
  df = data.frame(
    temp =  unlist(ref.info[1,1]),
    phenotype = unlist(ref.info[1,2]),
    
    
    t = I$statistic,
    df = I$parameter,
    p = I$p.value
    
  )
  df
}
stats.out = rbindlist(out)

sig.sex = stats.out %>% 
  filter(p < 0.05) %>% 
  as.data.table(.)
sig.sex
#now draw comparisons across each temperature, again not by inv.st
#now check for differces between sex, grouping across genotypes
ref.table = expand.grid( unique(all.phen$temp), unique(all.phen$Phenotype))
#find difference between genotypes in every condition for each trait
out = foreach(f = c(1:dim(ref.table)[1]), .errorhandling = "remove") %do% {
  #f = 10
  ref.info = ref.table[f,]
  #compare the two temps 
  dt = all.phen[temp != unlist(ref.info[1,1])][Phenotype == unlist(ref.info[1,2])][sex == "female"]
  temps = unique(dt$temp)
  
  I = t.test(dt[temp == temps[1]]$values, dt[temp == temps[2]]$values)
  df = data.frame(
    temp.excluded =  unlist(ref.info[1,1]),
    phenotype = unlist(ref.info[1,2]),
    
    
    t = I$statistic,
    df = I$parameter,
    p = I$p.value
    
  )
  df
}
stats.out = rbindlist(out)

sig.temp = stats.out %>% 
  filter(p < 0.05) %>% 
  as.data.table(.)
sig.temp

#gene by environment interaction
# we want to compare across inv, temp, for each trait. 
gx = foreach(f = unique(all.phen$Phenotype), .errorhandling = "remove") %do% {
  f = unique(all.phen$Phenotype)[2]
  
  dt = all.phen[Phenotype == f][sex == "female"]
  dt$exp = as.factor(dt$exp)
  model.add = lm(values ~ temp + inv.st , data = dt)
  model.mult = lm(values ~ temp * inv.st, data = dt)
  afit = anova(model.add, model.mult)
  
  dt = data.frame(
    phenotype = f,
    df = afit[2,3],
    Fstat = afit[2,5],
    Pval = afit[2,6]
  )
  dt
}

ge.out = rbindlist(gx)
