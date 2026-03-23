#analyze July DART
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/")
objects = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/"
#reference table to f the genotypes of each flyid

reftable = data.table( 
  flyid.total = c(paste0("1_Fly#" , c(1:18) , "-DAY1"),#the flyid as recorded on dart
                  paste0("2_Fly#" , c(1:14) , "-DAY1"),
                  paste0("3_Fly#" , c(1:8) , "-DAY1"),
                  paste0("4_Fly#" , c(1:18) , "-DAY1"),
                  paste0("5_Fly#" , c(1:18) , "-DAY1"),
                  paste0("6_Fly#" , c(1:14) , "-DAY1"),
                  
                  paste0("1_Fly#" , c(1:18) , "-DAY2"),
                  paste0("2_Fly#" , c(1:18) , "-DAY2"),
                  paste0("3_Fly#" , c(1:18) , "-DAY2"),
                  paste0("4_Fly#" , c(1:18) , "-DAY2"),
                  paste0("5_Fly#" , c(1:18) , "-DAY2"),
                  paste0("6_Fly#" , c(1:18) , "-DAY2"),
                  
                  paste0("1_Fly#" , c(1:18) , "-DAY3"),
                  paste0("2_Fly#" , c(1:18) , "-DAY3"),
                  paste0("3_Fly#" , c(1:18) , "-DAY3"),
                  paste0("4_Fly#" , c(1:18) , "-DAY3"),
                  paste0("5_Fly#" , c(1:18) , "-DAY3"),
                  paste0("6_Fly#" , c(1:18) , "-DAY3"),
                  
                  paste0("1_Fly#" , c(1:18) , "-DAY4"),
                  paste0("2_Fly#" , c(1:18) , "-DAY4"),
                  paste0("3_Fly#" , c(1:18) , "-DAY4"),
                  paste0("4_Fly#" , c(1:18) , "-DAY4"),
                  paste0("5_Fly#" , c(1:18) , "-DAY4"),
                  paste0("6_Fly#" , c(1:18) , "-DAY4")
                  ),
  geno = c(rep("I4_S2", 18),#the genetic background
           rep("I4_S1", 14),
           rep("I1_S2", 8),
           rep("I4_I1", 18),
           rep("S2_S1", 18),
           c( rep("I4_I1",4), rep("I4_S2",4), rep("S2_S1",4), rep("I4_S1",2)),


           rep("I4_I1", 18),
           rep("I4_S1", 18),
           rep("S2_S1", 18),
           rep("I4_S2", 18),
           rep("I1_S2", 18),
           c( rep("I4_I1",4), rep("I4_S2",2), rep("S2_S1",4), rep("I1_S2",4), rep("I4_S1",4)),
           
           rep("I4_I1", 18),
           rep("I4_S1", 18),
           rep("S2_S1", 18),
           rep("I4_S2", 18),
           rep("I1_S2", 18),
           c( rep("I4_I1",4), rep("I4_S2",4), rep("S2_S1",4), rep("I1_S2",2), rep("I4_S1",4)),
           
           rep("I4_I1", 18),
           rep("I4_S1", 18),
           rep("S2_S1", 18),
           rep("I4_S2", 18),
           rep("I1_S2", 18),
           c( rep("I4_I1",2), rep("I4_S2",4), rep("S2_S1",4), rep("I1_S2",4), rep("I4_S1",4))
    
  ),
  sex = c( #the fly's sex
    c(rep("male", 9), rep("female", 9)),# day 1 is a little different
    c(rep("male", 5), rep("female", 9)),
    c(rep("male", 3), rep("female", 5)),
    c(rep("male", 9), rep("female", 9)),
    c(rep("male", 9), rep("female", 9)),
    c("male","male", "female", "female","male","male", "female", "female","male","male", "female", "female", "female","female"),#for day 1 the extras are female, for the others they are male
  
    rep(
      c(rep("male", 9), rep("female", 9)) ,5
    ),
    c("male","male", "female", "female","male","male","male","male", "female", "female","male","male", "female", "female","male","male", "female", "female"),
    
    rep(
      c(rep("male", 9), rep("female", 9)) ,5
    ),
    c("male","male", "female", "female","male","male", "female", "female","male","male", "female", "female","male", "male", "male","male", "female", "female"),
    
  
    rep(
      c(rep("male", 9), rep("female", 9)) ,5
    ),
    c("male","male", "male","male","female", "female","male","male", "female", "female","male","male", "female", "female","male", "male", "female", "female")
    
  )
)
#load in movement data

filelist = list.files(path = objects, full.names = T)

fileout = foreach(f = c(1: length(filelist))) %do% {
  f = 3
  dt = fread(filelist[f])
  #narrow the data
  dt = dt[c(4:dim(dt)[1]), c(1:21)]
 # make row 1 into colnames
  rownames = unlist(as.vector(dt[1,]))
  colnames(dt) = rownames
  dt = dt[-1,]
  dt$Day = 1
  #melt the data 
  melt = melt(dt, id.vars = c("Group", "Day", "Time"), variable.name = "flyid", value.name = "activity")
  #clean up and add a unique fly variable
  melt = melt %>% 
    select(c(Group, Time, flyid, activity)) %>% 
    mutate(flyid = paste0(Group, "_", flyid))
  melt$flyid = gsub("\\ ", "", melt$flyid)
  melt$day = f
  melt$flyid.total = paste0(melt$flyid, "-DAY", melt$day)
  melt
  
}
out = rbindlist(fileout)
#merge in metadata like genotype and sex
mergedata = merge(out, reftable, by = "flyid.total")

mergedata$activity = as.numeric(mergedata$activity)

#create a inv reference
inv.ref = data.table(
  geno = c("I4_I1", "I4_S2", "I4_S1", "I1_S2", "S2_S1"),
  inv.st = c("K1.Homozygous-Inverted","K2.Heterozygous", "K2.Heterozygous", "K2.Heterozygous","K3.Homozygous.Standard")
)
merge2 = merge(mergedata, inv.ref, by = "geno")
merge2$Time = as.numeric(merge2$Time)
merge2$hour = merge2$Time / 3600 + 6
saveRDS(merge2, "julymerge2")
