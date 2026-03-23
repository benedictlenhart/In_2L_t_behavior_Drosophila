library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")
#saveRDS(phendata, "fig1.1.groupeddata")
#phendata = as.data.table(readRDS("fig1.1.groupeddata"))
phendata = as.data.table(readRDS("fig4.groupeddata"))


#filter data to only include sleep and activity
phendata2 = phendata %>%
  #filter(phenotype == "SleepDuration")
  filter(
         stimili == "High Intensity",
         phenotype %in% c("Activity","SleepDuration")) %>% 
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex)) %>% 
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted")))

a2colors = c("red","green4", "blue")
annotation_df <- data.frame(
 
  start = c(0.85, 1, 0.85, 1.85),
  end = c(1, 1.15, 1.15,2), 
  y = c(40, 44,48, 30),
  label = c("*** ", "***"," ***","*")
)
a1 = phendata2 %>% 
  filter(temp == 25,
         phenotype == "SleepDuration") %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Activity (min/hour)") +
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
  ) +ylim(16, 51)
a1


annotation_df <- data.frame(
  
  start = c(0.85, 0.85, 1.85, 2, 1.85, 2.85, 3, 2.85),
  end= c(1, 1.15, 2, 2.15, 2.15, 3, 3.15, 3.15), 
  y = c(38, 42,40, 44,48, 48, 52, 56),
  label = "***"
)

a2 = phendata2 %>% 
  filter(sex == "Female",
         phenotype == "SleepDuration") %>% 
  ggplot( ) +  
  xlab("Temperature") +
  ylab("Activity (min/hour)") +
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
  ) +ylim(27,57)
a2



a3 = phendata2 %>% 
  filter(temp == 25,
         phenotype == "Activity") %>% 
  ggplot( ., aes(
    x= sex,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  xlab("Sex") +
  ylab("Speed (mm/s)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15) 
a3

a4 = phendata2 %>% 
  filter(sex == "Female",
         phenotype == "Activity") %>% 
  ggplot( ., aes(
    x= temp,
    y=mean,
    ymin=lci,
    ymax=uci,
    color = inv.st
  )) +  
  xlab("Temperature") +
  ylab("Speed (mm/s)") +
  #facet_grid(.~sex, scales = "free") +
  scale_color_manual(values = a2colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw(base_size = 15) 
a4



#fig 1
ggsave( ((a1 + a2) /(a3 + a4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) + 
          plot_annotation(
            tag_levels = c("A")) ,
        file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/srFig1.2.pdf", width = 8, height = 7)

