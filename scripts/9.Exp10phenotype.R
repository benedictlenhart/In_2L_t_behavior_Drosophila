library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)
library(readxl)
library(foreach)


setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")
data = readRDS("exp3position.data.part1")
data$fullid = data$flyid
data2 = readRDS("exp3position.data.part2")
data2$fullid = data2$flyid
#this experiment is structured differently then the others, and so I need to address the data slightly differently. We have two runs of the dart with slightly different timing of temperature and stimili. In addition, the base activity follows an odd pattern of decaying over time, so in this case we recalculate base activity prior to each pulse. 
tidytry1 = data %>% 
  mutate(startle = case_when(hour >= 4.25 & hour < 4.75 ~ 1,
                             hour >= 4.75 & hour < 5.25 ~ 2,
                             hour >= 5.25 & hour < 5.75 ~ 3,
                             hour >= 5.75 & hour < 6.25 ~ 4,
                             hour >= 6.25 & hour < 6.75 ~ 5,
                             hour >= 6.75 & hour < 7.25 ~ 6,
                             hour >= 7.25 & hour < 7.75 ~ 7,
                             
                             hour >= 8.25 & hour < 8.75 ~ 1,
                             hour >= 8.75 & hour < 9.25 ~ 2,
                             hour >= 9.25 & hour < 9.75 ~ 3,
                             hour >= 9.75 & hour < 10.25 ~ 4,
                             hour >= 10.25 & hour < 10.75 ~ 5,
                             hour >= 10.75 & hour < 11.25 ~ 6,
                             hour >= 11.25 & hour < 11.75 ~ 7,
                             
                             hour >= 12.25 & hour < 12.75 ~ 1,
                             hour >= 12.75 & hour < 13.25 ~ 2,
                             hour >= 13.25 & hour < 13.75 ~ 3,
                             hour >= 13.75 & hour < 14.25 ~ 4,
                             hour >= 14.25 & hour < 14.75 ~ 5,
                             hour >= 14.75 & hour < 15.25 ~ 6,
                             hour >= 15.25 & hour < 15.75 ~ 7,
                             T ~ NA
                             )) %>% 
  mutate(startle2 = case_when(hour >= 4.5 & hour < 5 ~ 1,
                             hour >= 5 & hour < 5.5 ~ 2,
                             hour >= 5.5 & hour < 6 ~ 3,
                             hour >= 6 & hour < 6.5 ~ 4,
                             hour >= 6.5 & hour < 7 ~ 5,
                             hour >= 7 & hour < 7.5 ~ 6,
                             hour >= 7.5 & hour < 8 ~ 7,
                             
                             hour >= 8.5 & hour < 9 ~ 1,
                             hour >= 9 & hour < 9.5 ~ 2,
                             hour >= 9.5 & hour < 10 ~ 3,
                             hour >= 10 & hour < 10.5 ~ 4,
                             hour >= 10.5 & hour < 11 ~ 5,
                             hour >= 11 & hour < 11.5 ~ 6,
                             hour >= 11.5 & hour < 12 ~ 7,
                             
                             hour >= 12.5 & hour < 13 ~ 1,
                             hour >= 13 & hour < 13.5 ~ 2,
                             hour >= 13.5 & hour < 14 ~ 3,
                             hour >= 14 & hour < 14.5 ~ 4,
                             hour >= 14.5 & hour < 15 ~ 5,
                             hour >= 15 & hour < 15.5 ~ 6,
                             hour >= 15.5 & hour < 16 ~ 7,
                             T ~ NA
  )) %>% 
  mutate(intensity = case_when(startle == 1 ~ 20,
                               startle == 2 ~ 23,
                               startle == 3 ~ 26,
                               startle == 4 ~ 30,
                               startle == 5 ~ 33,
                               startle == 6 ~ 36,
                               startle == 7 ~ 40
                               )) %>% 
  
  group_by(fullid, startle, temp) %>% 
  mutate(exp = 10.2) %>% 
    mutate(median = median(hour)) %>%# set up the point of stimilus
  as.data.table(.)

#make one for day 1
tidytry2 = data2 %>% 
  mutate(startle = case_when(hour >= 5.25 & hour < 5.75 ~ 1,
                             hour >= 5.75 & hour < 6.25 ~ 2,
                             hour >= 6.25 & hour < 6.75 ~ 3,
                             hour >= 6.75 & hour < 7.25 ~ 4,
                             hour >= 7.25 & hour < 7.75 ~ 5,
                             
                             hour >= 8.25 & hour < 8.75 ~ 1,
                             hour >= 8.75 & hour < 9.25 ~ 2,
                             hour >= 9.25 & hour < 9.75 ~ 3,
                             hour >= 9.75 & hour < 10.25 ~ 4,
                             hour >= 10.25 & hour < 10.75 ~ 5,
                             
                             hour >= 11.25 & hour < 11.75 ~ 1,
                             hour >= 11.75 & hour < 12.25 ~ 2,
                             hour >= 12.25 & hour < 12.75 ~ 3,
                             hour >= 12.75 & hour < 13.25 ~ 4,
                             hour >= 13.25 & hour < 13.75 ~ 5,
                  
                             T ~ NA
  )) %>% 
  mutate(startle2 = case_when(hour >= 5.5 & hour < 6 ~ 1,
                             hour >= 6 & hour < 6.5 ~ 2,
                             hour >= 6.5 & hour < 7 ~ 3,
                             hour >= 7 & hour < 7.5 ~ 4,
                             hour >= 7.5 & hour < 8 ~ 5,
                             
                             hour >= 8.5 & hour < 9 ~ 1,
                             hour >= 9 & hour < 9.5 ~ 2,
                             hour >= 9.5 & hour < 10 ~ 3,
                             hour >= 10 & hour < 10.5 ~ 4,
                             hour >= 10.5 & hour < 11 ~ 5,
                             
                             hour >= 11.5 & hour < 12 ~ 1,
                             hour >= 12 & hour < 12.5 ~ 2,
                             hour >= 12.5 & hour < 13 ~ 3,
                             hour >= 13 & hour < 13.5 ~ 4,
                             hour >= 13.5 & hour < 14 ~ 5,
                             
                             T ~ NA
  )) %>% 
  mutate(intensity = case_when(startle == 1 ~ 20,
                               startle == 2 ~ 40,
                               startle == 3 ~ 60,
                               startle == 4 ~ 80,
                               startle == 5 ~ 100)) %>% 
  
  group_by(fullid, startle, temp) %>% 
  mutate(median = median(hour)) %>%# set up the point of stimilus
  mutate(exp = 10.1) %>% 
  as.data.table(.)
#merge data
mergedata = rbind(tidytry1, tidytry2)
baseact = mergedata %>% 
  group_by(fullid, intensity, temp,exp) %>% 
  filter(hour < median) %>% 
  filter(hour > (median - 0.0833)) %>% 
  summarise(base.act = mean(activity)) %>% 
  as.data.table(.)
mergedata2 = merge(mergedata,baseact, by = c("fullid",  "temp", "intensity","inv.st","genotype"))
#find peak activity post startle
peakact = na.omit(mergedata2) %>% 
  group_by(fullid, intensity, temp) %>% 
  filter(hour > median) %>% 
  filter(activity == max(activity)) %>% 
  summarise(peak.act = activity,
            peak.time = min(hour)) %>% 
  as.data.table(.)
#peakact[fullid == "Fly_86_Day6"]
#fix startle to include the entire half hour post startle
mergedata3 = merge(mergedata2,peakact, by = c("fullid",  "temp", "intensity"))


#find time activity returns to baseline
durationact = mergedata3 %>%
  group_by(fullid, intensity, temp) %>%
  filter(hour >= peak.time ) %>%
  filter(activity <= base.act ) %>%
  filter(hour == min(hour)) %>%
  distinct(.) %>%
  summarise(end.time = hour,
          duration = end.time - peak.time) %>%
  as.data.table(.)

mergedata4 = merge(mergedata3, durationact, by = c("fullid",  "temp", "intensity"))
#nice that was mostly painless
#scale by base act 15 minutes prior to startle

mergedata4$scaled.act = mergedata4$peak.act - mergedata4$base.act
mergedata4$intensity = as.factor(mergedata4$intensity)
mergedata4 = na.omit(mergedata4)
saveRDS(mergedata4, "simplephenodata")
