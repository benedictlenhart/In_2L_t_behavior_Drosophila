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
  mutate(flyidfull = paste(flyid,exp, sep = "_")) %>%
  group_by(flyidfull,temp, stimili,inv.st,sex) %>%
  summarise(mean.duration = mean(duration),
            mean.peak = mean(scaled.act),
            mean.base = mean(base.act)) %>%
  as.data.table(.)
# fulldata = rbind(mindata, maxdata)
# #make a function that groups up all the data by fly
# flydata = talldata%>%
#   mutate(intensity = as.numeric(as.character(intensity))) %>%
#   mutate(flyidfull = paste(flyid,exp, sep = "_")) %>%
#   group_by(flyidfull, inv.st,exp,temp) %>%
#   summarise(mean.duration = mean(duration),
#             mean.peak = mean(scaled.act),
#             mean.base = mean(base.act)) %>%
#   as.data.table(.)

#make combined pheno data object for graphing
setwd("C:/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/dartdata")
sleepdata = readRDS("sleepdatacompiledalltemp")
a = aov(`Sleep Duration`~ inv.st, data = sleepdata[temp == 25])
summary(a)
sleeppheno = sleepdata %>% 
  ####important change- inverse of sleep
  ## instead of duration sleep per hour, duration active
  mutate(`Sleep Duration` = 60 - `Sleep Duration`) %>% 
  mutate(inv.st = case_when(inv.st == "heterozygous" ~ "K2.Inv_Std",
                            inv.st == "homozygous.inverted" ~ "K3.Inverted",
                            T ~ "K1.Standard")) %>% 
  #mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  #filter(exp == 10) %>%  
  #filter(temp == 25) %>% 
  #filter(sex == "female") %>% 
  mutate(temp = as.factor(temp)) %>% 
  na.omit(.) %>% 
  group_by(inv.st,temp,sex# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(`Sleep Duration`)[1],
            uci = ci(`Sleep Duration`)[2],
            lci = ci(`Sleep Duration`)[3] 
            
  ) %>% 
  mutate(phenotype = "SleepDuration",
         stimili = "High Intensity")
male = mindata[sex == "male"]
table(male$inv.st)
durpheno = mindata %>% 
  #mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  #filter(exp == 10) %>%  
  #filter(stimili == "High Intensity") %>% 
  #filter(temp == 25) %>% 
  #filter(sex == "female") %>% 
  mutate(mean.duration = mean.duration * 3600) %>% 
  mutate(temp = as.factor(temp)) %>% 
  na.omit(.) %>% 
  group_by(inv.st,temp, stimili,sex# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(mean.duration)[1],
            uci = ci(mean.duration)[2],
            lci = ci(mean.duration)[3] 
            
  )%>% 
  mutate(phenotype = "duration")

magpheno = mindata %>% 
  #mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  #filter(exp == 10) %>% 
  #filter(stimili == "High Intensity") %>% 
  #filter(temp == 25) %>% 
  #filter(sex == "female") %>% 
  mutate(temp = as.factor(temp)) %>% 
  na.omit(.) %>% 
  group_by(inv.st,temp,stimili,sex# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(mean.peak)[1],
            uci = ci(mean.peak)[2],
            lci = ci(mean.peak)[3] 
            
  )%>% 
  mutate(phenotype = "Magnitude")

basedata = mindata %>% 
  #mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  #filter(exp == 10) %>% 
  #filter(stimili == "High Intensity") %>% 
  #filter(temp == 25) %>% 
  #filter(sex == "female") %>% 
  mutate(temp = as.factor(temp)) %>% 
  na.omit(.) %>% 
  group_by(inv.st,temp,stimili,sex# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(mean.base)[1],
            uci = ci(mean.base)[2],
            lci = ci(mean.base)[3] 
            
  )%>% 
  mutate(phenotype = "Activity")
phendata = rbind(sleeppheno, durpheno,magpheno,basedata)

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
saveRDS(phendata, "fig4.groupeddata")

