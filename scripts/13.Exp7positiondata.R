#New analysis of aug dart data

library(lme4)

library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(readxl)
library(foreach)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/exp7pos/")
#set up the outline of which genotype is where

day1 = data.frame(
  day = "day1",
  flyid = paste0("Fly_", c(1:108), "_Day1"),
  genotype = c(rep("S3_S1",18),rep("I1_I4",18),rep("I4_I2",18),rep("I1_S1",18),rep("I6_S1",18),rep("S3_S1",9),rep("I6_I4",9) )
)
day2 = data.frame(
  day = "day2",
  flyid = paste0("Fly_", c(1:108), "_Day2"),
  genotype = c(rep("I6_I4",6),rep("I4_I1",6),rep("I5_I4",6),rep("I6_S2",6),rep("I1_S1",6),rep("I6_S1",6),rep("I5_S2",6),rep("S2_S1",6),rep("I4_S1",6),rep("S3_S1",6),rep("S4_S1",6),rep("I6_I1",6),rep("I5_S1",6),rep("I6_S4",6),rep("S4_S3",6),rep("I5_S3",6),rep("I6_S3",6),rep("S3_S2",6))
)
day3 = data.frame(
  day = "day3",
  flyid = paste0("Fly_", c(1:105), "_Day3"),
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I5",5),rep("I6_I1",5),rep("I5_S3",5),rep("S4_S2",5),rep("I6_S3",5),rep("S3_S2",5))
)
day4 = data.frame(
  day = "day4",
  flyid = paste0("Fly_", c(1:108), "_Day4"),
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I5",5),rep("I6_I1",5),rep("S4_S3",5),rep("I5_S3",5),rep("S4_S2",5),rep("I6_S3",5),rep("S3_S2",3))
)
day5 = data.frame(
  day = "day5",
  flyid = paste0("Fly_", c(1:100), "_Day5"),
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I1",5),rep("I5_S3",5),rep("S4_S2",5),rep("I6_S3",5),rep("S3_S2",5))
)
day6 = data.frame(
  day = "day6",
  flyid = paste0("Fly_", c(1:100), "_Day6"),
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I1",5),rep("I5_S3",5),rep("S4_S2",5),rep("I6_S3",5),rep("S3_S2",5))
)
day7 = data.frame(
  day = "day7",
  flyid = paste0("Fly_", c(1:108), "_Day7"),#S4xs1 is replaced with s3_s1
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),rep("S3_S1",5),rep("S3_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I5",5),rep("I6_I1",5),rep("S4_S3",5),rep("I5_S3",5),rep("S4_S2",5),rep("I6_S3",5),rep("S3_S2",3))
)
day8 = data.frame(
  day = "day8",
  flyid = paste0("Fly_", c(1:108), "_Day8"),
  genotype = c(rep("I6_I4",5),rep("I4_I1",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("I4_S1",5),
               rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I5",5),rep("I6_I1",5),rep("S4_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("S3_S2",3),rep("S4_S2",5))
)
day9 = data.frame(
  day = "day9",
  flyid = paste0("Fly_", c(1:100), "_Day9"),
  genotype = c(rep("I4_I1",5),rep("I6_I4",5),rep("I5_I4",5),rep("I6_S2",5),rep("I1_S2",5),rep("I1_S1",5),rep("I6_S1",5),rep("I5_S2",5),rep("S2_S1",5),rep("S3_S1",5),rep("S4_S1",5),rep("I5_S1",5),rep("I5_I1",5),rep("I6_S4",5),rep("I6_I5",5),rep("I6_I1",5),rep("S4_S3",5),rep("I5_S3",5),rep("I6_S3",5),rep("S3_S2",5))
  
)
ref.table = list(day1, day2, day3, day4,day5,day7,day8,day9)
re.table = rbindlist(ref.table)
#load in data files

pheno.dir = ("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/exp7pos/")
pheno.files <- list.files(path= c(pheno.dir) , all.files=F, full.names=T, recursive=T )

#remove aug 1, and only go up to seven items from the list

outbig = foreach(f = c(1:9)) %do% {
  # f = 1

  g = pheno.files[f]
  dt = read_excel(g)
  #remove top three rows
  dt = dt[-(1:2),]
  names = dt[1,]
  colnames(dt) = names
  dt = dt[-1,]

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
  order = c(1:9),
  day = c(2,3,4,5,6,7,8,9,1)
)
# we cut day 1, and day 9
day.ref = day.ref[-c(1,9),]
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

inv.ref = data.table(
  genotype = unique(merged$genotype),
  inv.st = c("H","S","S","I",
             
             "I","I","H", "H",
             "H","H","H","S",
             "H","S","S","H",
             "I", "H","I", "I",
             "H","S"
             
             
  )
)
merge2 = merge(merged,inv.ref, by = "genotype")
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/exp7pos/")
saveRDS(merge2, "exp7pos.dt")
