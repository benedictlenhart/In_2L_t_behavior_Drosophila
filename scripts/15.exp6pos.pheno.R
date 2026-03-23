
#analyze July DART
library(tidyverse)
library(plotrix)
library(data.table)
library(foreach)
library(readxl)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
mergedata = readRDS("exp6positions")

#what is the range of positions
mergedata$pos = as.numeric(mergedata$activity)
#mergedata$pos = gsub("\\.","",mergedata$pos)
summary(mergedata$pos)
day.range = mergedata %>%
  group_by(flyid.total) %>%
  mutate(min = min(pos),
         max = max(pos)) %>%
  mutate(midpoint = (min + max)/2,
         eighth = (max-min)/8) %>% 
  ungroup()

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
  dplyr::group_by(flyid.total,geno,sex) %>% 
  mutate(rows = n()) %>% 
 # dplyr::summarise(f.ratio = sum(f.id)/rows) %>% 
  dplyr::summarise(f.ratio = sum(f.id)/rows) %>%
  distinct(.) %>% 
  as.data.table(.)


inv.ref = data.table(
  geno = c("I4_I1", "I4_S2", "I4_S1", "I1_S2", "S2_S1"),
  inv.st = c("K1.Homozygous-Inverted","K2.Heterozygous", "K2.Heterozygous", "K2.Heterozygous","K3.Homozygous.Standard")
)
merge.feed = merge(f.ratio, inv.ref, by = "geno")
#check overall distribution of feeding time.
saveRDS(merge.feed, "exp6feeding.n")
merge.feed%>% 
  na.omit() %>% 
  mutate(inv.st = factor(inv.st , levels = c("K3.Homozygous.Standard", "K2.Heterozygous", "K1.Homozygous-Inverted"))) %>% 
  group_by(sex, inv.st,# inversion.st 
  ) %>% 
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>% 
  summarise(mean = ci(f.ratio)[1],
            uci = ci(f.ratio)[2],
            lci = ci(f.ratio)[3] 
            
  )%>% 
  ggplot( aes(
    x=sex,
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
