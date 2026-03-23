library(lme4)
#library(aomisc)
library(data.table)
library(tidyverse)
library(plotrix)
library(gmodels)
library(scales)
library(readxl)
library(foreach)


setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/July_2023_objects/")

data = readRDS("exp1position.data")
data$fullid = data$flyid


#try other way at looking at startle

tidytry2 = data %>% 
  #filter(sex == "female") %>% 
  mutate(startle.number = case_when(hour >= 7 & hour < 8 ~ 7,
                                    hour >= 8 & hour < 9 ~ 8,
                                    
                                    hour >= 10 & hour < 11 ~ 10,
                                    hour >= 11 & hour < 12 ~ 11,
                                    
                                    
  )) %>%
  mutate(temp = 25) %>% 
  mutate(intensity = 100) %>% 
  mutate(startle = case_when(hour == 7 ~ 7,
                             hour == 8 ~ 8,
                             hour == 10 ~ 10,
                             hour == 11 ~ 11,
                             T~ NA))%>% 
  as.data.table(.)
#lets try something new, and look at the range of overall activity for flies
#look for extreme outliers (wels with dead/missing fly, or wayward tracking)
overall = tidytry2 %>% 
  group_by(flyid) %>% 
  summarize(mean.act = mean(activity),
            median.act = median(activity),
            sum.act = sum(activity)) %>% 
  as.data.table(.)
overall[mean.act >2 ]
overall[sum.act > 1400]#no flies with no activity, or mostly no activity. 


base.data25 = tidytry2 %>% 
  group_by(flyid.total,temp) %>% 
  filter(temp == 25) %>% 
  filter(hour < 7 & hour > 6.5) %>% 
  summarise(basal.act = mean(activity))

#talldata = rbind(base.data20, base.data25, base.data30)
testdata = merge(tidytry2, base.data25 , by = c("flyid.total", "temp"))


peakact = na.omit(testdata) %>%
  group_by(flyid.total, startle) %>%
  summarise(peakact = activity)



#find time activity returns to baseline
durationact = testdata %>%
  group_by(flyid.total,startle.number) %>%
  # filter(hour >= peak.time ) %>%
  filter(activity <= basal.act ) %>%
  filter(hour == min(hour)) %>%
  distinct(.) %>%
  summarise(end.time = hour,
            duration = end.time - startle.number) %>%
  as.data.table(.)

#new addition, try to first find time spent active in 15 minutes before stimuli, the compare that to 15 minutes after (for magnitude) and how it takes to return to baseline sliding windows that are 15 minutes long, 1 minute step? 




#durationact[fullid == "Fly_99_Day8"]
mergedata3 = merge(testdata,peakact, by = c("flyid.total","startle"))
mergedata4 = merge(mergedata3, durationact, by = c("flyid.total","startle.number"))

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/")
saveRDS(mergedata4, "exp6data.2")
