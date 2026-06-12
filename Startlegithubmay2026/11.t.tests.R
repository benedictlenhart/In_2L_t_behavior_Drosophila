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

setwd("/Users/adamlenhart/Documents/Bergland Research/StartleManuscript/startle.github.objectsmay2026/")
#activity and startle response data
mindata = readRDS("activity.startle.phenotypesPROCESSED")
#sleep data
sleepdata = readRDS("sleepdataPROCESSED")
#position near food data
phendata = readRDS("fullprop")

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
         Phenotype = "Food.near") %>% 
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

all.phen = rbind(minfixed, sleepfixed, foodfixed,foodaway)
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

