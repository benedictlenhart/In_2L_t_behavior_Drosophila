library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)
library(foreach)

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/dartdata//")
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
#bind ref files
ref = rbind(day2, day3, day4, day5, day6, day7, day8)
#load in data files using a loop per temperature
outbig = foreach(f = c(20,25,30)) %do% {
  #f = 25
  dt = fread(paste0(f,"sleep.csv"))
  dt$Experiment = dt$Experiment + 2 # add 2 to the day
  dt.merge = dt %>% 
    rename("Day" = "Experiment",
           "fly" = "Fly Index") %>% 
    mutate(flyid = paste0("Fly_", fly, "_Day", Day)) %>% 
    merge(., ref, by = "flyid") %>% 
    mutate(temp = f)
  
}
bigbind = rbindlist(outbig)  
#loop for smaller data
smallout = foreach(f = c(20,25,30)) %do% {
  #f = 25
  dt = fread(paste0(f,"_14.csv"))
  dt$Experiment = 2 # experiment/day is 2
  dt.merge = dt %>% 
    rename("Day" = "Experiment",
           "fly" = "Fly Index") %>% 
    mutate(flyid = paste0("Fly_", fly, "_Day", Day)) %>% 
    merge(., ref, by = "flyid") %>% 
    mutate(temp = f)
  
}
smallbind = rbindlist(smallout)
fullout = rbind(bigbind, smallbind)
#now add in inversion information

key = data.table(
  genotype = unique(fullout$genotype),
  inv.st = c(rep("K1.S", 2),rep("K3.het", 11), rep("K2.I",5), rep("K1.S", 4))
)
mergeall = merge(fullout, key, by = "genotype")
#remove the day part from day
mergeall$day = gsub("day","", mergeall$day)
mergeall$day = as.numeric(mergeall$day)
saveRDS(mergeall, "novsleep")
#############
### graphing
#############33
mergeall %>% 
  #distinct(.) %>% 
  #filter(temp == 20) %>% 
  group_by(temp,inv.st,# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(`Bout Duration`)[1],
            uci = ci(`Bout Duration`)[2],
            lci = ci(`Bout Duration`)[3] 
            
  )%>% 
  ggplot( aes(
    x=as.factor(temp),
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  xlab("Temperature") +
  ylab("Bout number") +
  #facet_grid(.~temp) +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()
