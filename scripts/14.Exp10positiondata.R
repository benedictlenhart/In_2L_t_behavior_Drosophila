#New analysis of aug dart data

library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(readxl)
library(foreach)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/exp10pos/")
#set up the outline of which genotype is where
setup = c(rep("def", 6), rep("bal",5))
day2 = data.frame(
  day = "day2",
  flyid = paste0("Fly_", c(1:107), "_Day2"),
  genotype = c(rep("I2_S4",3),rep("I5_S4",8),rep("I2_S3",8),rep("I5_S3",3),rep("I6_S3",8),rep("I5_S2",8) ,rep("I6_S2",8),rep("I6_S1",3),rep("I6_I1",3),rep("I6_I1",3),rep("I5_I2",3) ,rep("I6_I2",3),rep("I5_I6",3),rep("S3_S4",8),rep("S2_S4",8),rep("S1_S4",3) ,rep("S2_S3",8),rep("S1_S3",8),rep("S1_S1",8))
)

day3 = data.frame(
  day = "day3",
  flyid = paste0("Fly_", c(1:108), "_Day3"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5),rep("I5_I2",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",5),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
day4 = data.frame(
  day = "day4",
  flyid = paste0("Fly_", c(1:108), "_Day4"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5),rep("I5_I2",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",5),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
day5 = data.frame(
  day = "day5",
  flyid = paste0("Fly_", c(1:103), "_Day5"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",5),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
day6 = data.frame(
  day = "day6",
  flyid = paste0("Fly_", c(1:103), "_Day6"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",5),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
day7 = data.frame(
  day = "day7",
  flyid = paste0("Fly_", c(1:103), "_Day7"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",5),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
day8 = data.frame(
  day = "day8",
  flyid = paste0("Fly_", c(1:101), "_Day8"),
  genotype = c(rep("I1_S4",5),rep("I2_S4",5),rep("I5_S4",3),rep("I6_S4",5),rep("I2_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("I5_S2",5) ,rep("I6_S2",5),rep("I6_S1",5),rep("I2_S1",5),rep("I5_I1",5),rep("I6_I1",5) ,rep("I6_I2",5),rep("I5_I6",5),rep("S3_S4",3),rep("S2_S4",5),rep("S1_S4",5) ,rep("S2_S3",5),rep("S1_S3",5),rep("S1_S1",5))
)
ref.table = list( day2, day3, day4,day6, day5,day7,day8)
re.table = rbindlist(ref.table)
#load in data files

pheno.dir = ("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/exp10pos/")
pheno.files <- list.files(path= c(pheno.dir) , all.files=F, full.names=T, recursive=T )
#
#pheno.files = pheno.files[-2]
outbig = foreach(f = c(1:8)) %do% {
  # f = 1
  #dt = fread(paste0("day",f,".x"))
  g = pheno.files[f]
  dt = read_excel(g)
  #remove top three rows
  dt = dt[-(1:2),]
  names = dt[1,]
  colnames(dt) = names
  dt = dt[-1,]
  #drop day
  #dt = dt[,-1]
  #melt
  melt = melt(as.data.table(dt), id.vars = c("Time"), value.name = "activity", variable.name = "flyid")
  #add day 
  melt$order = f
  melt
  
}
out = rbindlist(outbig)
out$activity = as.numeric(out$activity)
group.guide = data.frame(
  Group = (rep(c(1:6), each = 18)),
  group = c(1:108)
  
)
#make a group column
out$group = as.numeric(gsub("Fly #", "", out$flyid))
out = merge(out, group.guide, by = "group")
#clean up flyid
out$flyid = gsub("Fly #", "Fly_", out$flyid)
#make 
pheno.files
#add day info based on the order
day.ref = data.frame(
  order = c(1:8),
  day = c(1:8)
)
# we cut day 1
day.ref = day.ref[-c(1),]
out = merge(out, day.ref, by = "order")
out$flyid = paste0(out$flyid, "_Day", out$day)
#I want to check to make sure the number of flies matches across days
re.table[,day:= tstrsplit(ref.table$flyid, "_")[3] ]
table(re.table$day)
out.small = out %>%
  select(!c(Time, activity)) %>%
  distinct(.)
table(out.small$day)
#it looks like there are some flies on day 1 I forgot to annoate (around 6). I removed them to avoid confusion. 
#merge in genotypes
re.table = re.table %>% 
  select(!day)
merged = merge(out, re.table, by = c("flyid"))

key = data.table(
  genotype = unique(merged$genotype),
  inv.st = c(rep("K1.S",2), rep("K3.het", 11), rep("K2.I",5), rep("K1.S", 4))
)
merge2 = merge(merged, key, by = "genotype")

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
saveRDS(merge2, "exp10pos.dt")
