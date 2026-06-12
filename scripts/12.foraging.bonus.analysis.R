#bonus analysis: analyze the foraging index from Lee 2017 to see effect of inversion
lee.dt = fread("foraging.csv")
#load in inv.ref.table
reftable = readRDS("dgrpreftable")
leefixed = lee.dt %>% 
  rename("DGRP"="DGRP strain") %>% #there's a weird wrinkle where strains < 100 have an 0 to start with
  mutate(DGRP = case_when(DGRP < 100 ~  paste0("DGRP_0", DGRP),
                          T ~ paste0("DGRP_", DGRP)) )
reffixed = reftable %>% 
  select(DGRP, Inversion_2L_t_NA) %>% 
  merge(leefixed, ., by = "DGRP") %>% 
  as.data.table(.)
#check t test effect of in2lt karyotype
t.test(reffixed[Inversion_2L_t_NA == 0]$`Foraging index`, reffixed[Inversion_2L_t_NA == 2]$`Foraging index`)
