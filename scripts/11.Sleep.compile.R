#sleep combine
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(gmodels)
#july data
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/")
july.sleep = readRDS("sleepdata")
head(july.sleep)
#fix july.sleep 

#august data
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/augexp/")
aug.sleep = readRDS( "sleepdataaug")

#November data

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/dartdata//")
nov.sleep = readRDS("novsleep")
#combine the individual fly data
head(july.sleep)
july.clean = july.sleep %>% 
  #filter(sex == "female") %>% 
  select(flyid.total, traits, value, inv.st,sex) %>% 
  mutate(temp = 25) %>% 
  mutate(exp = "6") 

july.clean= dcast(july.clean, flyid.total+sex + exp + inv.st + temp ~ traits, value.var = "value")


head(aug.sleep)
aug.clean = aug.sleep %>% 
  rename("flyid.total" = flyid) %>% 
  select(flyid.total, `Sleep Bout`, `Sleep Duration`, `Bout Duration`, temp, inv.st) %>% 
  mutate(exp = "7",
         sex = "female")
head(nov.sleep)
nov.clean = nov.sleep %>% 
  rename("flyid.total" = flyid) %>% 
  select(-c(genotype, Day, fly, day)) %>% 
  mutate(inv.st = case_when(grepl("K3.het", inv.st) == T ~ "heterozygous",
                            grepl("K1.S", inv.st) == T ~ "homozygous.standard",
                            grepl("K2.I", inv.st) == T ~ "homozygous.inverted",)) %>% 
  mutate(exp = "10",
         sex = "female")
#bind
talldata2 = rbind(july.clean, aug.clean, nov.clean)
talldata2 = talldata2 %>% 
  #filter(temp == 30) %>% 
  # mutate(inv.st = case_when(grepl("heterozygous", inv.st) == T ~ "K2.Inv_Std",
  #                           grepl("homozygous.standard", inv.st) == T ~ "K1.Standard",
  #                           grepl("homozygous.inverted", inv.st) == T ~ "K3.Inverted",)) %>% 
  group_by(exp) %>% 
  mutate(sleep.scaled = (`Sleep Duration` - mean(`Sleep Duration`))/ sd(`Sleep Duration`)) %>% 
  as.data.table(.)
saveRDS(talldata2, "sleepdatacompiledalltemp")

