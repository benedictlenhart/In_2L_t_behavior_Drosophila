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
# ranges = mergedata %>% 
#   na.omit(.) %>% 
#   group_by(Group,day.x) %>% 
#  summarize(min = min(pos),
#          max = max(pos),
#          eighth = (max-min)/8) %>% 
#   as.data.table(.)
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
day.range10 = rbind(merge1, merge2)
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

f.ratio = day.range %>% 
  # mutate(max.feed = (min + midpoint)/2) %>% 
  mutate(max.feed = min + ((max-min)/8)) %>% 
  mutate(f.id = ifelse(pos <= max.feed,1,0)) %>% 
  dplyr::group_by(temp, flyid,inv.st) %>% 
  mutate(rows = n()) %>% 
  mutate(inv.st = case_when(inv.st == "K2.I" ~ "K3.Inverted",
                            inv.st == "K1.S" ~ "K1.Standard",
                            T ~ "K2.Heterozygous")) %>% 
  dplyr::summarise(f.ratio = sum(f.id)/rows) %>% 
  distinct(.)


region.prop = day.range %>% 
  mutate(inv.st = case_when(inv.st == "K2.I" ~ "K3.Inverted",
                            inv.st == "K1.S" ~ "K1.Standard",
                            T ~ "K2.Heterozygous")) %>% 
  mutate(region = case_when(pos < (min + eighth) ~ 1,
                            pos >= (min + eighth) & pos < (min + 2*eighth) ~ 2,
                            pos >= (min + 2*eighth) & pos < (min + 3*eighth) ~ 3,
                            pos >= (min + 3*eighth) & pos < (min + 4*eighth) ~ 4,
                            pos >= (min + 4*eighth) & pos < (min + 5*eighth) ~ 5,
                            pos >= (min + 5*eighth) & pos < (min + 6*eighth) ~ 6,
                            pos >= (min + 6*eighth) & pos < (min + 7*eighth) ~ 7,
                            pos >= (min + 7*eighth) ~ 8)) 
df_metadata <- region.prop %>%
  ungroup() %>% 
  select(flyid, inv.st) %>% 
  distinct()  # Keep one row per sample with metadata

region.ratios = region.prop %>%
  count(flyid, region, temp) %>%  # count rows per flyid.total-region
  complete(flyid, region,temp, fill = list(n = 0)) %>%  # fill in missing combinations
  group_by(flyid, temp) %>%
  mutate(
    total_rows = sum(n),
    region.prop = n / total_rows
  ) %>%
  left_join(df_metadata, by = "flyid") %>%  # add metadata back in
  select(flyid, temp, inv.st, region, region.prop) %>% 
  as.data.table(.)
#check the difference between old method and new
f.ratio = as.data.table(f.ratio)
summary(f.ratio[inv.st == "K2.Heterozygous"][temp == 30]$f.ratio)
summary(region.ratios[inv.st == "K2.Heterozygous"][temp == 30][region == 1]$region.prop)
saveRDS(region.ratios, "exp10feedingprop")
#check overall distribution of feeding time.

region.ratios%>% 
  na.omit() %>% 
  group_by( region,temp, inv.st# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(region.prop)[1],
            uci = ci(region.prop)[2],
            lci = ci(region.prop)[3] 
            
  )%>% 
  ggplot( aes(
    x=region,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  #xlab("def.status") +
  #ylab("Basal Activity") +
  facet_grid(temp~., scales = "fixed") +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()
