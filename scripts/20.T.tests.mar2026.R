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
exp7data = readRDS("exp1.data")
exp6data = readRDS("exp2.data")

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")


#saveRDS(mergedata4, "simplephenodata")
#merge in modeling data for lrc
exp10data = readRDS("exp3.data")
setwd("C:/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/dartdata")
sleepdata = readRDS("sleepdatacompiledalltemp")
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects//")

#foraging data
phendata = as.data.table(readRDS("fullprop"))
#activity based SR

# activitySR = readRDS("C:/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/April_2025_objects/activitySRdata")
# exp10data %>% 
#   group_by(hour) %>% 
#   summarize(mean.act = mean(activity)) %>% 
#   ggplot(
#     aes( x = hour, y = mean.act)
#   ) + geom_point()
# 
# ggplot(exp10data, aes(hour, activity)) + geom_point()
#check genotypes
allgeno = c(exp6data$geno, exp7data$genotype, exp10data$genotype)
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


##############
##statistics##
##############
#mergetogether our data
head(mindata)
minfixed = mindata %>% 
  pivot_longer(cols = c(mean.duration, mean.peak, mean.base), names_to = "Phenotype", values_to = "values") %>% 
  select(c(flyidfull, temp, inv.st, sex, Phenotype, values,exp))
head(sleepdata)
sleepfixed = sleepdata %>% 
  mutate(`Sleep Duration` = 60 - `Sleep Duration`) %>% 
  mutate(flyidfull = paste(flyid.total, exp, sep = "_"),
         Phenotype = "Activity") %>% 
  rename("values"="Sleep Duration") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype,exp))
head(phendata)
foodfixed = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>%
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 1) %>% #remove zeros or 1s
  
  mutate(flyidfull = paste(flyid, exp, sep = "_"),
         Phenotype = "Food.away") %>% 
  rename("values"="f.ratio") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype,exp))
foodaway = phendata %>% 
  mutate(f.ratio = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
         f.ratio = asin(sqrt(f.ratio)) 
  )%>%
  filter(! f.ratio %in% c("-Inf","Inf"),
         region == 8) %>% #remove zeros or 1s
  
  mutate(flyidfull = paste(flyid, exp, sep = "_"),
         Phenotype = "Food.far") %>% 
  rename("values"="f.ratio") %>% 
  select(c(flyidfull, sex, inv.st, temp, values, Phenotype,exp))
head(activitySR)
actfixed = activitySR %>% 
  mutate(flyidfull = paste(flyid, exp, sep = "_")) %>% 
  pivot_longer(cols = c(duration, mag, basal_act), names_to = "Phenotype", values_to = "values") %>% 
  select(c(flyidfull, temp, inv.st, sex, Phenotype, values,exp))
all.phen = rbind(minfixed, sleepfixed, foodfixed,foodaway, actfixed)
all.phen = all.phen %>% 
  mutate(inv.st = case_when(inv.st == "heterozygous" ~ "K2.Inv_Std",
                            inv.st == "K2.Inv_Std" ~ "K2.Inv_Std",
                            inv.st == "K2.Heterozygous" ~ "K2.Inv_Std",
                            inv.st == "homozygous.inverted" ~ "K3.Inverted",
                            inv.st == "K1.Homozygous-Inverted" ~ "K3.Inverted",
                            inv.st == "K3.Inverted" ~ "K3.Inverted",
                            inv.st == "homozygous.standard" ~ "K1.Standard",
                            inv.st == "K1.Standard" ~ "K1.Standard",
                            inv.st == "K3.Homozygous.Standard" ~ "K1.Standard")) %>% 
  as.data.table(.)

#add sleep info 

saveRDS(all.phen, "sr.stats.data8")
all.phen = readRDS("sr.stats.data8")
#we want to do t.tests bewteen each karyotype, for each phenotype, for each temperuture, for each sex. 
#make a ref.table
ref.table = expand.grid(c("male","female"), unique(all.phen$temp), unique(all.phen$Phenotype))
#find difference between genotypes in every condition for each trait
out = foreach(f = c(1:dim(ref.table)[1]), .errorhandling = "remove") %do% {
  # f =37
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

#perform multiple testing correction BH with trait at the family level
stats.out = stats.out %>% 
  group_by(phenotype) %>% 
  mutate(p.adjusted = p.adjust(p, method = "BH"))
saveRDS(stats.out, "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/June_2025_objects/t.stats")
sig = stats.out %>% 
  filter(p < 0.05) %>% 
  as.data.table(.)
sig
data = na.omit( all.phen[sex == "female"][temp == 25][Phenotype == "mean.base"])

x =aov(values ~ inv.st, data = data)

#now check the location proportion data, across temperatures and regions
ref.table = expand.grid(unique(phendata$region), unique(phendata$temp))
#find difference between genotypes in every condition for each trait
out = foreach(f = c(1:dim(ref.table)[1]), .errorhandling = "remove") %do% {
  #f =1
  ref.info = ref.table[f,]
  dt = phendata[temp == unlist(ref.info[1,2])][region == unlist(ref.info[1,1])][sex == "female"]
  dt = dt %>% 
    mutate(region.prop = region.prop + (1/(4*dim(phendata)[1])), #add a small constant to prevent square root of zero
           region.prop = asin(sqrt(region.prop)) 
    )%>%
    filter(! region.prop %in% c("-Inf","Inf")) %>% #remove zeros or 1s
    
    mutate(flyidfull = paste(flyid, exp, sep = "_"))
  # model.add = lm(value ~ def.id + inv.st , data = dt)
  # model.mult = lm(value ~ def.id * inv.st , data = dt)
  # afit = anova(model.add, model.mult)
  I.S = t.test(dt[inv.st == "K1.Standard"]$region.prop, dt[inv.st == "K3.Inverted"]$region.prop)
  H.S = t.test(dt[inv.st == "K1.Standard"]$region.prop, dt[inv.st == "K2.Heterozygous"]$region.prop)
  H.I = t.test(dt[inv.st == "K2.Heterozygous"]$region.prop, dt[inv.st == "K3.Inverted"]$region.prop)
  df = data.frame(
    temp =  unlist(ref.info[1,2]),
    
    region = unlist(ref.info[1,1]),
    comparison = c("I/S", "H/S","H/I"),
    t = c(I.S$statistic,H.S$statistic,H.I$statistic),
    df = c(I.S$parameter,H.S$parameter,H.I$parameter),
    p = c(I.S$p.value,H.S$p.value,H.I$p.value)
    
  )
  df
}
stats.out = rbindlist(out)

sigprop = stats.out %>% 
  filter(p < 0.05) %>% 
  as.data.table(.)
sigprop

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
  #f = unique(all.phen$Phenotype)[3]
  
  dt = all.phen[Phenotype == f][sex == "female"]
  dt$exp = as.factor(dt$exp)
  model.add = lmer(values ~ temp + inv.st + (1|exp) , data = dt)
  model.mult = lmer(values ~ temp * inv.st + (1|exp), data = dt)
  
  # model.add = lm(values ~ temp + inv.st  , data = dt)
  # model.mult = lm(values ~ temp * inv.st, data = dt)
  afit = anova(model.add, model.mult)
  
  dt = data.frame(
    phenotype = f,
    df = afit[2,7],
    Chisq = afit[2,6],
    Pval = afit[2,8]
  )
  dt
}

ge.out = rbindlist(gx)
ge.out
a = all.phen[Phenotype == "Activity"]
