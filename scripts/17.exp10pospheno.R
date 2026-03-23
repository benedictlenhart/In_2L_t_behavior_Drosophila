
#analyze July DART
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(readxl)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
mergedata = readRDS("exp10pos.dt")

#what is the range of positions
mergedata$pos = as.numeric(mergedata$activity)
missing = mergedata %>%
  filter(if_any(everything(), is.na)) %>% 
  select(flyid)
missingids = unlist(unique(missing))

#mergedata$pos = gsub("\\.","",mergedata$pos)
summary(mergedata$pos)

day.range = mergedata %>% 
  filter(!flyid %in% missingids) %>% #remove flies with missing data
  
  group_by(flyid) %>% 
  mutate(min = min(pos),
         max = max(pos)) %>% 
  mutate(midpoint = (min + max)/2,
         eighth = (max-min)/8) %>% 
  ungroup() %>% 
  as.data.table(.)

#ok- we'll need to finangle each day seperatly here
part2 = day.range[day.x > 2]

part2$Time = as.numeric(part2$Time)
part2$hour = part2$Time / 3600 + 4
merge2 = part2 %>% 
  mutate(temp = case_when(hour < 8 ~ 20,
                          hour > 12 ~ 30,
                          T ~ 25)) %>% 
  mutate(activity = as.numeric(activity))

#try day 1
part1 =  day.range[day.x == 2]

part1$Time = as.numeric(part1$Time)
part1$hour = part1$Time / 3600 + 5
merge1 = part1 %>% 
  mutate(temp = case_when(hour < 8 ~ 20,
                          hour > 11 ~ 30,
                          T ~ 25)) %>% 
  mutate(activity = as.numeric(activity))
#rebind
day.range = rbind(merge1, merge2)
#

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
  mutate(f.id = ifelse(pos < max.feed,1,0)) %>% 
  dplyr::group_by(temp, flyid,inv.st,day.x) %>% 
  mutate(rows = n()) %>% 
  mutate(inv.st = case_when(inv.st == "K2.I" ~ "K3.Inverted",
                            inv.st == "K1.S" ~ "K1.Standard",
                            T ~ "K2.Heterozygous")) %>% 
  dplyr::summarise(f.ratio = sum(f.id)/rows) %>% 
  distinct(.)


saveRDS(f.ratio, "exp10feeding.n")
#check overall distribution of feeding time.

f.ratio%>% 
  na.omit() %>% 
  group_by( temp, inv.st,day.x# inversion.st 
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
  facet_grid(.~day.x, scales = "fixed") +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()
