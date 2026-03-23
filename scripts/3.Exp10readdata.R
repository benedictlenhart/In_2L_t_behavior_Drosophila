###annotations for first sep experiment
library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects//")
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
#load in data files
#############3
##day2########3
###############

dt = fread("day2.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
# #censor other dead flies
# meltdt = meltdt[! flyid %in% c(26,46:48,75,87,89,90,96)]

#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day2")
#merge in genotype
mergedata2 = merge(meltdt, day2, by = "flyid")
#########
##day3##
########

dt = fread("day3.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day3")
#merge in genotype
mergedata3 = merge(meltdt, day3, by = "flyid")

#########
##day4##
########

dt = fread("day4.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day4")
#merge in genotype
mergedata4 = merge(meltdt, day4, by = "flyid")

#########
##day5##
########

dt = fread("day5.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day5")
#merge in genotype
mergedata5 = merge(meltdt, day5, by = "flyid")
#########
##day6##
########

dt = fread("day6.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day6")
#merge in genotype
mergedata6 = merge(meltdt, day6, by = "flyid")
#########
##day7##
########

dt = fread("day7.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day7")
#merge in genotype
mergedata7 = merge(meltdt, day7, by = "flyid")
#########
##day8##
########

dt = fread("day8.2.csv", header = T)

dt= dt[,-1]#remove first column
#melt data
meltdt = melt(dt, id.vars = "Time", variable.name = "flyid", value.name = "activity")

#remove extra
meltdt$flyid = gsub("Fly #", "", meltdt$flyid)
meltdt$flyid = as.numeric(meltdt$flyid)


#a
table(meltdt$flyid)
#fix back flyid
meltdt$flyid = paste0("Fly_", meltdt$flyid, "_Day8")
#merge in genotype
mergedata8 = merge(meltdt, day8, by = "flyid")





#bind and clean data
talldata = rbind(mergedata2,mergedata3,mergedata4,mergedata5,mergedata6,mergedata7,mergedata8)
#make a key to add in karyotype
key = data.table(
  genotype = unique(talldata$genotype),
  inv.st = c(rep("K3.het", 8), rep("K2.I",4), rep("K1.S", 6), rep("K3.het",3), "K2.I")
)
mergeall = merge(talldata, key, by = "genotype")
#remove the day part from day
mergeall$day = gsub("day","", mergeall$day)
mergeall$day = as.numeric(mergeall$day)

#adjust the data based on day
part2 = mergeall[day > 2]

part2$Time = as.numeric(part2$Time)
part2$hour = part2$Time / 3600 + 4
merge2 = part2 %>% 
  mutate(temp = case_when(hour < 8 ~ 20,
                          hour > 12 ~ 30,
                          T ~ 25)) %>% 
  mutate(activity = as.numeric(activity))
saveRDS(merge2, "lowstartle")
merge2 = readRDS("lowstartle")
#try day 1
part2 = mergeall[day ==2]

part2$Time = as.numeric(part2$Time)
part2$hour = part2$Time / 3600 + 5
merge2 = part2 %>% 
  mutate(temp = case_when(hour < 8 ~ 20,
                          hour > 11 ~ 30,
                          T ~ 25)) %>% 
  mutate(activity = as.numeric(activity))
saveRDS(merge2, "highstartle")
merge2 = readRDS("highstartle")
