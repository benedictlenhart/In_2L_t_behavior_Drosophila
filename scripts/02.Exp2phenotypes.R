library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)
library(readxl)
library(foreach)



data = readRDS("exp2behaviordataRAW")
data$fullid = data$flyid

#try other way at looking at startle

tidytry2 = data %>% 
  mutate(startle.number = case_when(hour >= 7 & hour < 8 ~ 7,
                                    hour >= 8 & hour < 9 ~ 8,
                                    
                                    hour >= 10 & hour < 11 ~ 10,
                                    hour >= 11 & hour < 12 ~ 11,
                                    
                                    
                                    hour >= 13 & hour < 14 ~ 13,
                                    hour >= 14 & hour < 15 ~ 14,
                                    
  )) %>%
  mutate(temp = case_when(hour < 9 ~ 20,
                          hour < 12 ~ 25,
                          T ~ 30
                         )) %>% 
  mutate(intensity = 100) %>% 
  mutate(startle = case_when(hour == 7 ~ 7,
                             hour == 8 ~ 8,
                             hour == 10 ~ 10,
                             hour == 11 ~ 11,
                             hour == 13 ~ 13,
                             hour == 14 ~ 14,
                             T~ NA))%>% 
  as.data.table(.)


#find base acivity of one hour prior to each startle


base.data20 = tidytry2 %>% 
  group_by(flyid,temp) %>% 
  filter(temp == 20) %>% 
  filter(hour < 7 & hour > 6.5) %>% 
  summarise(basal.act = mean(activity))
base.data25 = tidytry2 %>% 
  group_by(flyid,temp) %>% 
  filter(temp == 25) %>% 
  filter(hour < 10 & hour > 9.5) %>% 
  summarise(basal.act= mean(activity))
base.data30 = tidytry2 %>% 
  group_by(flyid, temp) %>% 
  filter(temp == 30) %>% 
  filter(hour < 13 & hour > 12.5) %>% 
  summarise(basal.act = mean(activity))
talldata = rbind(base.data20, base.data25, base.data30)
testdata = merge(tidytry2, talldata , by = c("flyid", "temp"))


peakact = na.omit(testdata) %>%
  group_by(flyid, startle) %>%
  summarise(peakact = activity)



#find time activity returns to baseline
durationact = testdata %>%
  group_by(fullid,startle.number) %>%
  # filter(hour >= peak.time ) %>%
  filter(activity <= basal.act ) %>%
  filter(hour == min(hour)) %>%
  distinct(.) %>%
  summarise(end.time = hour,
            duration = end.time - startle.number) %>%
  as.data.table(.)
#durationact[fullid == "Fly_99_Day8"]
mergedata3 = merge(testdata,peakact, by = c("flyid","startle"))
mergedata4 = merge(mergedata3, durationact, by = c("fullid","startle.number"))

saveRDS(mergedata4, "exp2behaviordataPROCESSED")
