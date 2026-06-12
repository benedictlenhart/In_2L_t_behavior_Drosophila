#combine feeding data
f6 = readRDS("exp6feeding.prop")
f7 = readRDS("exp7feedingprop")
f10 = readRDS("exp10feedingprop")

head(f6)

head(f7)

head(f10)
f6.fixed = f6 %>% 
  rename("flyid"="flyid.total") %>% 
  select(-geno) %>% 
  mutate(temp = 25,
         exp = 6) %>% 
  mutate(inv.st = case_when(inv.st == "K1.Homozygous-Inverted" ~ "K3.Inverted",
                            inv.st == "K3.Homozygous.Standard" ~ "K1.Standard",
                            T ~ "K2.Heterozygous"))
f7fixed = f7 %>% 
  mutate(sex = "female",
         exp = 7) %>% 
  select(-total_rows)
f10fixed = f10 %>% 
  mutate(sex = "female",
         exp = 10)
fullfeed = rbind(f6.fixed, f7fixed, f10fixed)
f6 %>% 
  filter(region == 1,
         sex == "female") %>% 
  summary(region.prop)
f6.old %>% 
  filter(sex == "female") %>% 
  summary(f.ratio)
saveRDS(fullfeed,"positionsFINAL")

