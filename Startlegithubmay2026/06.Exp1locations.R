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
  group_by(flyid.) %>%
 mu(min = min(pos),
         max = max(pos),
         eighth = (max-min)/8)

#ok- different groups have different position ranges

#min(closest to bottom, left, aka food.  max, farthest from food)
#midpoint varies a bit depending on day, but ultimatelly very close. I think we can safely just use overall mid per group.

#analysis- we want to see how long each fly spent close to food versus not- should that just be food size, or closer?
#we want to see how many times the fly crossed midpoint.
###################################33
##feeding assay##########
#######################
#split tube into eights, region closest to food is considered near food.

#min to max.feed is the quarter closes to food.
#for each fly- find times they are in food quarter verss not, and find the ratio. we'll start with whole experiment.
f.ratio = day.range %>%
  # mutate(max.feed = (min + midpoint)/2) %>%
  mutate(max.feed = min + ((max-min)/8)) %>%
  mutate(f.id = ifelse(pos <= max.feed,1,0)) %>%
  dplyr::group_by(flyid.total,geno,sex) %>%
  mutate(rows = n()) %>%
  dplyr::summarise(Nearfood = sum(f.id == 1),
                   awayfood = sum(f.id == 0)) %>%
  distinct(.)
###new analysis###
#split tube into eigths, find the proportion of time each fly spends in each area

#see if it's signicantly different if we try manaully determing regions
region.prop = day.range %>%
  mutate(region = case_when(pos < (min + eighth) ~ 1,
                            pos >= (min + eighth) & pos < (min + 2*eighth) ~ 2,
                            pos >= (min + 2*eighth) & pos < (min + 3*eighth) ~ 3,
                            pos >= (min + 3*eighth) & pos < (min + 4*eighth) ~ 4,
                            pos >= (min + 4*eighth) & pos < (min + 5*eighth) ~ 5,
                            pos >= (min + 5*eighth) & pos < (min + 6*eighth) ~ 6,
                            pos >= (min + 6*eighth) & pos < (min + 7*eighth) ~ 7,
                            pos >= (min + 7*eighth) ~ 8)) %>% # split each position into 1/8 regions
  #filter(region != 8) %>% # a small fraction of flies surpass the last region4
  group_by(flyid.total, geno, sex) %>%
  mutate(total.measure = n()) #find sum of measurements for each fly remaining
region.prop = region.prop %>%
  group_by(flyid.total, geno, sex, region) %>%
 # reframe(region.prop = n()/total.measure) %>%
  reframe(region.prop = n()) %>%
  distinct(.) %>%
  as.data.table(.)

summary(region.prop[region ==1][sex == "male"]$region.prop)
summary(f.ratio[sex == "male"]$f.ratio)
#some flies are reported as never being near food in try1, but do go near food in try 2. let's check why
noeat = f.ratio[f.ratio == 0]
#uhoh- 241 of our 414 flies never go near food? that's weird
#let's check the position over time for some of these, like 1_Fly#1-DAY1
#this means they will never go lower then 45
ggplot(mergedata[flyid.total == "1_Fly#9-DAY2"], aes(Time, pos)) +
  geom_point()
min(mergedata[flyid.total == "1_Fly#1-DAY2"]$pos)
#wut- this fly goes everwhere, but it's min (53) seems to not reflect the min of the group (29. )
#"1_Fly#1-DAY2" has a min of 52. what could be going on?
#let's look at the actual min of flies in grou p1
#oh fuck I found the problem- it's because the flies that are never within a certain group are filtered out- there is no group probability zero being reported. agghhh.
mins = mergedata %>%
  group_by(flyid.total, Group) %>%
  summarise(min = min(pos))
#looks like most flies in group have a min that is atleast above 50, and could not be seen as close to food
max = mergedata %>%
  group_by(flyid.total, Group) %>%
  summarise(max = max(pos))
#so as a general theme it seems for group 1 atleast, many flies actual min/max is aways from the total min/max. could be changes in food and cotton hgeight.
#we can decide to change or keep that, the weirdness if why in our new method so many flies are bieng placed into region 1 that never actually should go there.
inv.ref = data.table(
  geno = c("I4_I1", "I4_S2", "I4_S1", "I1_S2", "S2_S1"),
  inv.st = c("K1.Homozygous-Inverted","K2.Heterozygous", "K2.Heterozygous", "K2.Heterozygous","K3.Homozygous.Standard")
)
merge.feed = merge(region.prop, inv.ref, by = "geno")
#check overall distribution of feeding time.
saveRDS(merge.feed, "exp6feeding.prop")
merge.feed%>%
  na.omit() %>%
  mutate(inv.st = factor(inv.st , levels = c("K3.Homozygous.Standard", "K2.Heterozygous", "K1.Homozygous-Inverted"))) %>%
  group_by(region, sex, inv.st,# inversion.st
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
  facet_grid(sex~., scales = "fixed") +
  #scale_color_manual(values = group.colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw()
