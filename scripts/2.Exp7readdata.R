#New analysis of aug dart data

library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Sep_2023_objects/")

#set up the outline of which genotype is where

day1 = data.frame(
  day = "day1",
  flyid = paste0("Fly_", c(1:108), "_Day1"),
  genotype = c(rep("S3_S1",18),rep("I_I4",18),rep("I4_I2",18),rep("I1_S1",18),rep("I6_S1",18),rep("S3_S1",9),rep("I6_I4",9) )
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
day8


#load in data files

dt = fread("july29.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(26,46:48,75,87,89,90,96)]

#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day2")
#merge in genotype
mergedata2 = merge(meltdt, day2, by = "flyid")



##############3
###load day 4###
##############3
dt = fread('aug2data.csv', header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")
#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)

#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(56:60, 86:90)]

#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day4")
#merge in genotype
mergedata4 = merge(meltdt, day4, by = "flyid")
##############3
###load day 5###
##############3
dt = fread('aug3data.csv', header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)

#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(25,85,101:108)]
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day5")
#merge in genotype
mergedata5 = merge(meltdt, day5, by = "flyid")
##############3
###load day 6###
##############3
dt = fread('aug4data.csv', header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")
#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)
#we need to take in account censoring (different for each file)

#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(105:108)]

meltdt = meltdt[! flyid %in% c(51:54)]
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day6")
#merge in genotype
mergedata6 = merge(meltdt, day6, by = "flyid")

##############3
###load day 7###
##############3
dt = fread('aug5data.csv', header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)

#a
table(meltdt$flyid)
#censor other dead flies

#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day7")
#merge in genotype
mergedata7 = merge(meltdt, day7, by = "flyid")

##############3
###load day 8###
##############3
dt = fread('aug7data.csv', header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(49:54,98)]
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day8")
#merge in genotype
mergedata8 = merge(meltdt, day8, by = "flyid")

#############3
###load day 9###
##############3
dt = fread("aug8data.csv", header = T)
dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)
#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(82,101:108)]
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day9")
#merge in genotype
mergedata9 = merge(meltdt, day9, by = "flyid")


#bind and clean data
talldata = rbind(mergedata2,mergedata4,mergedata5,mergedata6,mergedata7,mergedata8,mergedata9)

#make sure activity looks good
inv.ref = data.table(
  genotype = unique(talldata$genotype),
  inv.st = c("heterozygous",
             "homozygous.standard",
             "homozygous.inverted",
             "homozygous.inverted",
             "heterozygous",
             "homozygous.inverted",
             "heterozygous", 
             "heterozygous",
             "heterozygous",
             "homozygous.standard",
             "heterozygous",
             
             "homozygous.standard",
             "homozygous.standard",
             "homozygous.inverted",
             "heterozygous",
             "heterozygous",
             "homozygous.standard",
             "heterozygous",
             "homozygous.standard",
             "heterozygous",
             "homozygous.inverted",
             "homozygous.inverted"
             
  )
)

merge2 = merge(talldata, inv.ref, by = "genotype")
merge2$Time = as.numeric(merge2$Time)
merge2$hour = merge2$Time / 3600 + 6

saveRDS(merge2, "augmerge2")
