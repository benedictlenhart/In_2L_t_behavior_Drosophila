
#analyze July DART
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(readxl)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
mergedata = readRDS("exp7pos.dt")

#what is the range of positions
mergedata$pos = as.numeric(mergedata$activity)
missing = mergedata %>%
  filter(if_any(everything(), is.na)) %>% 
  select(flyid)
missingids = unlist(unique(missing))

day.range = mergedata %>% 
  filter(!flyid %in% missingids) %>% #remove flies with missing data
  group_by(flyid) %>% 
  mutate(min = min(pos),
         max = max(pos)) %>% 
  mutate(midpoint = (min + max)/2,
         eighth = (max-min)/8) %>% 
  ungroup()
#ok- to find temp we'll need to convert to hours.
day.range = day.range %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(hour = Time / 3600 + 6) %>% 
  mutate(temp = case_when(hour < 9 ~ 20,
                          hour >= 9 & hour < 12 ~ 25,
                          T ~ 30)) 
#ok- different groups have different position ranges

#min(closest to bottom, left, aka food.  max, farthest from food)
#midpoint varies a bit depending on day, but ultimatelly very close. I think we can safely just use overall mid per group. 

#analysis- we want to see how long each fly spent close to food versus not- should that just be food size, or closer? 
#we want to see how many times the fly crossed midpoint. 
###################################33
##feeding assay##########
#######################
#split tube into four quarters. quarter closest to food is considered near food.

#min to max.feed is the quarter closes to food. 
#for each fly- find times they are in food quarter verss not, and find the ratio. we'll start with whole experiment.
f.ratio = day.range %>% 
  # mutate(max.feed = (min + midpoint)/2) %>% 
  mutate(max.feed = min + ((max-min)/8)) %>% 
  mutate(f.id = ifelse(pos <= max.feed,1,0)) %>% 
  dplyr::group_by(temp, flyid,inv.st) %>% 
  mutate(rows = n()) %>% 
  mutate(inv.st = case_when(inv.st == "I" ~ "K3.Inverted",
                            inv.st == "H" ~ "K1.Standard",
                            T ~ "K2.Heterozygous")) %>% 
  dplyr::summarise(f.ratio = sum(f.id)/rows) %>% 
  distinct(.)

saveRDS(f.ratio, "exp7feeding.n")

#check overall distribution of feeding time.

f.ratio%>% 
  na.omit() %>% 
  group_by( temp, inv.st,# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(f.ratio)[1],
            uci = ci(f.ratio)[2],
            lci = ci(f.ratio)[3] 
            
  )%>% 
  ggplot( aes(
    x=temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  #xlab("def.status") +
  #ylab("Basal Activity") +
  #facet_grid(temp~., scales = "fixed") +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()

##############
##activity##
#############
#mimic trikinetics, count number of times fly crosses midpoint.
#step 1- simply position to either 1 or 0, as either above or below midpoint
#step 2 use the lag function to find the difference
#step 3 look for nonzero differences
f.ratio = day.range %>% 
  # mutate(max.feed = (min + midpoint)/2) %>% 
  mutate(f.id = ifelse(pos <= midpoint,0,1)) %>% 
  dplyr::group_by(flyid,inv.st,temp) %>% 
  mutate(diff = f.id - lag(f.id, 1)) %>% 
  mutate(diff = ifelse(diff == 0, 0,1)) %>% #change to 1 anytime there is change
  as.data.table(.)
#now we're going to aggregate and find the activity within each minute.
f.ratio = f.ratio %>% 
  na.omit() %>% 
  mutate(minute = floor(Time / 60)) %>%  # Create minute bins
  dplyr::group_by(minute,flyid,inv.st,temp,hour) %>% 
  summarise(total_value = sum(diff))
saveRDS(f.ratio, "exp7Trikact")
#check for activity in the first hour

#check for activity in the first hour, find mean activity
act20 = f.ratio %>% 
  filter(hour < 7 & hour >= 6) %>% 
  dplyr::group_by(flyid,inv.st,temp) %>% 
  summarise(base.act = mean(total_value)) 
act25 = f.ratio %>% 
  filter(hour < 10 & hour >= 9) %>% 
  dplyr::group_by(flyid,inv.st,temp) %>% 
  summarise(base.act = mean(total_value))
act30 = f.ratio %>% 
  filter(hour < 13 & hour >= 12) %>% 
  dplyr::group_by(flyid,inv.st,temp) %>% 
  summarise(base.act = mean(total_value))
mean.act = rbind(act20, act25, act30)
mean.act%>% 
  na.omit() %>% 
  group_by(temp, inv.st,# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(base.act)[1],
            uci = ci(base.act)[2],
            lci = ci(base.act)[3] 
            
  )%>% 
  ggplot( aes(
    x=temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  #xlab("def.status") +
  #ylab("Basal Activity") +
  #facet_grid(temp~., scales = "fixed") +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()
