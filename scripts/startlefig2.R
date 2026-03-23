library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
#saveRDS(phendata, "fig1.1.groupeddata")
#phendata = as.data.table(readRDS("fig1.1.groupeddata"))
phendata = as.data.table(readRDS("fig1and2data"))

#filter data to only include sleep and activity
phendata2 = phendata %>%
  #filter(phenotype == "SleepDuration")
  filter(
    stimili == "High Intensity") %>% 
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex)) %>% 
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted")))


a2colors = c("red","green4", "blue")

annotation_df <- data.frame(
  
  start = c(0.85 ),
  end = c(1.15), 
  y = c(550),
  label = c("*")
)
b1 = phendata2 %>% 
  filter(temp == 25,
         phenotype == "duration") %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Startle duration (sec)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(aes(
    x= sex,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(aes(
    x= sex,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15) +
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = .5,
    tip_length = 0.03,
    manual = TRUE
  ) +ylim(200,560)

b1

annotation_df <- data.frame(
  
  start = c(1.85 ),
  end = c(2.15), 
  y = c(550),
  label = c("*")
)

b2 = phendata2 %>% 
  filter(sex == "Female",
         phenotype == "duration") %>% 
  ggplot() +  
  xlab("Temperature") +
  ylab("Startle duration (sec)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(aes(
    x= temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(aes(
    x= temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15)  +
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = .5,
    tip_length = 0.03,
    manual = TRUE
  ) +ylim(200,560)
b2


annotation_df <- data.frame(
  
  start = c(1 ),
  end = c(1.15), 
  y = c(1.2),
  label = c("*")
)
b3 = phendata2 %>% 
  filter(temp == 25,
         phenotype == "Magnitude") %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Induced speed (mm/s)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(aes(
    x= sex,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(aes(
    x= sex,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15) +
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = .5,
    tip_length = 0.03,
    manual = TRUE
  ) 
b3

annotation_df <- data.frame(
  
  start = c(0.85,2, 3, 2.85 ),
  end = c(1, 2.15, 3.15, 3.15), 
  y = c(1.1, 1.1, .8, .88),
  label = c("*", "*","***","***")
)

b4 = phendata2 %>% 
  filter(sex == "Female",
         phenotype == "Magnitude") %>% 
  ggplot() +  
  xlab("Temperature") +
  ylab("Induced speed (mm/s)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(aes(
    x= temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(aes(
    x= temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  ),position=position_dodge(width = 0.5), show.legend = T) + theme_bw(base_size = 15) +
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = .5,
    tip_length = 0.03,
    manual = TRUE
  ) 
b4



#fig 1
ggsave( ((b1 + b2) /(b3 + b4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) + 
          plot_annotation(
            tag_levels = c("A")) ,
        file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/srFig2.2.pdf", width = 8, height = 7)

