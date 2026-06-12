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
#make a data object of the genotyeps
genos = day.range %>% 
  select(flyid, inv.st) %>% 
  distinct()
table(genos$inv.st)
#how many flies at differnet days/temps
# day.groups = day.range %>% 
#   group_by( day,temp) %>% 
#   summarise(count = length(unique(flyid)))
#ok- different groups have different position ranges

#min(closest to bottom, left, aka food.  max, farthest from food)
#midpoint varies a bit depending on day, but ultimatelly very close. I think we can safely just use overall mid per group. 

#analysis- we want to see how long each fly spent close to food versus not- should that just be food size, or closer? 
#we want to see how many times the fly crossed midpoint. 
###################################33
##feeding assay##########
#######################
#split tube into eigths. Lowest eighth is considered near food.

#min to max.feed is the quarter closes to food. 
#for each fly- find times they are in food quarter verss not, and find the ratio. we'll start with whole experiment.

#see if it's signicantly different if we try manaully determing regions
f.ratio = day.range %>% 
  # mutate(max.feed = (min + midpoint)/2) %>% 
  mutate(max.feed = min + ((max-min)/8)) %>% 
  mutate(f.id = ifelse(pos < max.feed,1,0)) %>% 
  dplyr::group_by(temp, flyid,inv.st) %>% 
  mutate(rows = n()) %>% 
  mutate(inv.st = case_when(inv.st == "I" ~ "K3.Inverted",
                            inv.st == "S" ~ "K1.Standard",
                            T ~ "K2.Heterozygous")) %>% 
  dplyr::summarise(f.ratio = sum(f.id)/rows) %>% 
  distinct(.)
f.genos = f.ratio %>% 
  select(flyid, inv.st) %>% 
  distinct() 
table(f.genos$inv.st)
  df_metadatanew <- day.range %>%
    ungroup() %>% 
    select(flyid, inv.st) %>% 
    distinct()

  f.rationew = day.range %>% 
    # mutate(max.feed = (min + midpoint)/2) %>% 
    mutate(max.feed = min + ((max-min)/8)) %>% 
    mutate(f.id = ifelse(pos < max.feed,1,0)) %>% 
  count(flyid, f.id, temp) %>%  # count rows per flyid.total-region
  complete(flyid, f.id,temp, fill = list(n = 0))
  #lets see which 
  %>%  # fill in missing combinations
  group_by(flyid, temp) %>%
  mutate(
    total_rows = sum(n),
    region.prop = n / total_rows
  ) %>%
    left_join(df_metadatanew, by = "flyid") %>%  # add metadata back in
    select(flyid, temp, inv.st, f.id, region.prop) %>% 
    as.data.table(.)
  
  #compare
  f.rationew  %>% 
    filter(
           temp == 30,
           f.id == 1) %>% 
    summary(n)
  
  
 region.join =  region.ratios  %>% 
    filter(
      region == 1) %>% 
   rename("oldn"="region.prop") %>% 
      select(flyid, "oldn",temp)
  #lets see which flys are being counted as having differnet times near food
  join = f.rationew %>% 
    filter(f.id == 1) %>% 
    left_join(region.join, by = c("temp","flyid"))
  join = f.ratio %>% 
    left_join(region.join, by = c("temp","flyid"))#the data is the same?
  ggplot(join, aes(f.ratio, oldn ))+
    geom_point()
  odd = join %>% 
    mutate(diff = f.ratio - oldn) %>% 
    filter(diff != 0) 
  %>% #53 of the 571 have differnet values
    left_join(df_metadata, by = "flyid")#our new way always undercounts compared to old way
  
region.prop = day.range %>% 
  mutate(inv.st = case_when(inv.st == "I" ~ "K3.Inverted",
                            inv.st == "S" ~ "K1.Standard",
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
#compare old vs new analysis
f.ratio = as.data.table(f.ratio)
summary(f.ratio[inv.st == "K2.Heterozygous"][temp == 30]$f.ratio)
summary(region.ratios[temp == 30][inv.st == "K2.Heterozygous"][region == 1]$region.prop)

#check that there is the same # of flies of each group
unique(f.ratio[inv.st == "K2.Heterozygous"]$flyid)#about half of hets are missing
unique(region.ratios[inv.st == "K2.Heterozygous"]$flyid)
unique(region.ratios$flyid)
#43 flies are missing data, all in 25 or 30
#look for areas of missing data
day.range = as.data.table(day.range)
std.ids = unique(day.range[inv.st == "H"]$flyid)
f = std.ids[104]
ggplot(day.range[flyid == f], aes(Time, pos)) +
  geom_point() +ylim(unique(unlist(day.range[flyid == f]$min)), unique(unlist(day.range[flyid == f]$max)))
ggplot(day.range[flyid ==  "Fly_100_Day8"], aes(Time, pos)) +
  geom_point() 

odd.examine =day.range[flyid ==  "Fly_11_Day4"]
min(mergedata[flyid.total == "1_Fly#1-DAY2"]$pos)
saveRDS(region.ratios, "exp7feedingprop")

#check overall distribution of feeding time.

region.ratios%>% 
  na.omit() %>% 
  group_by( temp, inv.st,region# inversion.st 
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
