library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(gmodels)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/")
objects = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/dart_july/sleebout/"

#make a reference table 
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
  #f = 2
  dt = fread(filelist[f])
  #create flyidtotal variable
  dt$day = f
  dt$flyid.total = paste0(dt$Group, "_", dt$`Fly Index`, "-DAY",dt$day)
  dt$flyid.total = gsub("\\ ", "", dt$flyid.total)
  dt


}
out = rbindlist(fileout, use.names = T)
#remove empty cells
out = na.omit(out)
#merge in metadata like genotype and sex
mergedata = merge(out, reftable, by = "flyid.total")
mergedata = mergedata %>% 
  select( ! c(Group, 'Fly Index'))

#melt data
meltdata = melt(mergedata, id.vars = c("flyid.total","geno", "sex", "day" ), variable.name = "traits")
#plot the difference in sleep between groups

inv.ref = data.table(
  geno = c("I4_I1", "I4_S2", "I4_S1", "I1_S2", "S2_S1"),
  inv.st = c("homozygous.inverted","heterozygous", "heterozygous", "heterozygous","homozygous.standard")
)
merge2 = merge(meltdata, inv.ref, by = "geno")
saveRDS(merge2, "sleepdata")
