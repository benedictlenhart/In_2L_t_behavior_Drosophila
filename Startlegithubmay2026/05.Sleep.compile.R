#sleep combine
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(gmodels)
#july data

july.sleep = readRDS("sleepexp1RAW")
head(july.sleep)
#fix july.sleep 


aug.sleep = readRDS( "sleepexp2RAW")

#November data


nov.sleep = readRDS("sleepexp3RAW")
#combine the individual fly data
head(july.sleep)
july.clean = july.sleep %>% 
  #filter(sex == "female") %>% 
  select(flyid.total, traits, value, inv.st,sex, geno) %>% 
  mutate(temp = 25) %>% 
  mutate(exp = "6") %>% 
  rename("genotype"="geno")

july.clean= dcast(july.clean, flyid.total+sex + exp + inv.st + temp+genotype ~ traits, value.var = "value")


head(aug.sleep)
aug.clean = aug.sleep %>% 
  rename("flyid.total" = flyid) %>% 
  select(flyid.total, `Sleep Bout`, `Sleep Duration`, `Bout Duration`, temp, inv.st, genotype) %>% 
  mutate(exp = "7",
         sex = "female")
head(nov.sleep)
nov.clean = nov.sleep %>% 
  rename("flyid.total" = flyid) %>% 
  select(-c( Day, fly, day)) %>% 
  mutate(inv.st = case_when(grepl("K3.het", inv.st) == T ~ "heterozygous",
                            grepl("K1.S", inv.st) == T ~ "homozygous.standard",
                            grepl("K2.I", inv.st) == T ~ "homozygous.inverted",)) %>% 
  mutate(exp = "10",
         sex = "female")
#bind
colnames(july.clean)
colnames(aug.clean)
colnames(nov.clean)
talldata2 = rbind(july.clean, aug.clean, nov.clean)
talldata2 = talldata2 %>% 
  #filter(temp == 30) %>% 
  # mutate(inv.st = case_when(grepl("heterozygous", inv.st) == T ~ "K2.Inv_Std",
  #                           grepl("homozygous.standard", inv.st) == T ~ "K1.Standard",
  #                           grepl("homozygous.inverted", inv.st) == T ~ "K3.Inverted",)) %>% 
  group_by(exp) %>% 
  mutate(sleep.scaled = (`Sleep Duration` - mean(`Sleep Duration`))/ sd(`Sleep Duration`)) %>% 
  as.data.table(.)
saveRDS(talldata2, "sleepdataFINAL")

