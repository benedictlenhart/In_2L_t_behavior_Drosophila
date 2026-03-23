#sleep analysis
#load in and prepare diverse DART data
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(gmodels)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/augexp/")
#repeat lines 10-246 with sleep20, sleep 25, sleep30
sleepdt = fread("sleep30.csv")
sleepdt = na.omit(sleepdt)
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

##############3
###load day 2###
##############3
dt = sleepdt[Day == 2]
dt= dt[,-1]#remove first column
meltdt = dt

#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)

meltdt$flyid = ifelse(meltdt$flyid >= 9, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 26, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 46, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 90, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 96, meltdt$flyid + 1, meltdt$flyid)

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
dt = sleepdt[Day == 4]
dt= dt[,-1]#remove first column
meltdt = dt
#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)
#we need to take in account censoring (different for each file)
meltdt$flyid = ifelse(meltdt$flyid >= 87, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 97, meltdt$flyid + 1, meltdt$flyid)
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
dt = sleepdt[Day == 5]
dt= dt[,-1]#remove first column
meltdt = dt

#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 25, meltdt$flyid + 1, meltdt$flyid)
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
dt = sleepdt[Day == 6]
dt= dt[,-1]#remove first column
meltdt = dt
#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)
#we need to take in account censoring (different for each file)
meltdt$flyid = ifelse(meltdt$flyid >= 107, meltdt$flyid + 1, meltdt$flyid)
meltdt = meltdt[! flyid %in% c(51:54)]
meltdt$flyid = ifelse(meltdt$flyid >= 55, meltdt$flyid - 4, meltdt$flyid)
#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(105:108)]

#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day6")
#merge in genotype
mergedata6 = merge(meltdt, day6, by = "flyid")

##############3
###load day 7###
##############3
dt = sleepdt[Day == 7]
dt= dt[,-1]#remove first column
meltdt = dt

#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
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
dt = sleepdt[Day == 8]
dt= dt[,-1]#remove first column
meltdt = dt

#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 37, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 50, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 57, meltdt$flyid + 1, meltdt$flyid)

#a
table(meltdt$flyid)
#censor other dead flies
meltdt = meltdt[! flyid %in% c(49:54,98)]
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day8")
#merge in genotype
mergedata8 = merge(meltdt, day8, by = "flyid")
##############3
###load day 9###
##############3
dt = sleepdt[Day == 9]
dt= dt[,-1]#remove first column
meltdt = dt

#remove extra
meltdt$flyid = gsub("Fly #", "", dt$`Fly Index`)
meltdt$flyid = as.numeric(meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 13, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 14, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 35, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 40, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 41, meltdt$flyid + 1, meltdt$flyid)
meltdt$flyid = ifelse(meltdt$flyid >= 82, meltdt$flyid + 1, meltdt$flyid)
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
inv.ref = readRDS("diverse.meta.RDS")
merge2 = merge(talldata, inv.ref, by = "genotype")
dt30 = merge2
#dt20 = merge2
#dt25 = merge2
dt20$temp = 20
dt25$temp = 25
dt30$temp = 30
fulldata = rbind(dt20,dt25,dt30)
saveRDS(fulldata, "sleepdataaug")
