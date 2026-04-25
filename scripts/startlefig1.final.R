library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/")

library(ggbeeswarm)
#saveRDS(phendata, "fig1.1.groupeddata")
#phendata = as.data.table(readRDS("fig1.1.groupeddata"))
phendata = as.data.table(readRDS("sample.level.data"))
phendata
table(phendata[phenotype == "SleepDuration"]$Value)
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
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted"))) %>% 
  group_by(inv.st,temp,sex,phenotype# inversion.st
  ) %>%
  
  # mutate(genotype = fct_reorder(genotype, inv.st)) %>%
  summarise(mean = ci(Value)[1],
            uci = ci(Value)[2],
            lci = ci(Value)[3]
            
  ) 
#perform similiar fixing on raw data
phendata_raw = phendata %>%
  filter(stimili == "High Intensity",
         phenotype %in% c("Activity", "SleepDuration")) %>%
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         # Apply the same 60 - Value flip for SleepDuration only
         #Value = ifelse(phenotype == "SleepDuration", 60 - Value, Value),
         sex = str_to_title(sex)) %>%
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted")))

a2colors = c("red","magenta", "blue")
annotation_df <- data.frame(
  
  start = c(0.85, 1, 0.85, 1.85),
  end = c(1, 1.15, 1.15,2), 
  y = c(41, 45,48, 28),
  label = c("*** ", "***"," ***","**")
)
a1 = phendata2 %>% 
  filter(temp == 25, phenotype == "SleepDuration") %>% 
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +  
  xlab("Sex") +
  ylab("Sleep (min/hour)") +
  scale_color_manual(values = a2colors) +
  scale_fill_manual(values = a2colors)+
  # Beeswarm layer first (behind)
  geom_beeswarm(data = filter(phendata_raw, temp == 25, phenotype == "SleepDuration"),
                aes(x = sex, y = Value, color = inv.st),
                dodge.width = 0.5,
                alpha = 0.1, size = 0.8, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.1, position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5), show.legend = FALSE, size = 3, color = "black", pch = 21) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  ylim(16, 51) +
  theme_bw(base_size = 15)
a1

# --- Plot a2: Sleep by Temp in Females ---
annotation_df <- data.frame(
  start = c(0.85, 0.85, 1.85, 2, 1.85, 2.85, 3, 2.85),
  end = c(1, 1.15, 2, 2.15, 2.15, 3, 3.15, 3.15), 
  y = c(40, 44, 41, 45, 49, 49, 53, 57),
  label = rep("***",8)
)

a2 = phendata2 %>% 
  filter(sex == "Female", phenotype == "SleepDuration") %>% 
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +  
  xlab("Temperature") +
  ylab("Activity (min/hour)") +
  scale_color_manual(values = a2colors) +
  scale_fill_manual(values = a2colors)+
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", phenotype == "SleepDuration"),
                aes(x = temp, y = Value, color = inv.st),
                dodge.width = 0.5,
                cex = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.3, position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5), show.legend = FALSE, size = 3, color = "black", pch = 21) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  ylim(0, 59) +
  theme_bw(base_size = 15)
a2
# --- Plot a3: Activity by Sex at 25C ---
a3 = phendata2 %>% 
  filter(temp == 25, phenotype == "Activity") %>% 
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +  
  xlab("Sex") +
  ylab("Speed (mm/s)") +
  scale_color_manual(values = a2colors) +
  scale_fill_manual(values = a2colors)+
  geom_beeswarm(data = filter(phendata_raw, temp == 25, phenotype == "Activity"),
                aes(x = sex, y = Value, color = inv.st),
                cex = 0.5,
                dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.4, position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5), show.legend = FALSE, size = 3, color = "black", pch = 21 ) +
  guides(color = guide_legend(title = "In(2L)t Genotype")) +
  theme_bw(base_size = 15)
a3
a4 =  phendata2 %>% 
  filter(sex == "Female", phenotype == "Activity") %>% 
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +  
  xlab("Temperature") +
  ylab("Speed (mm/s)") +
  scale_color_manual(values = a2colors, guide = "none") +  # suppress color legend
  scale_fill_manual(
    values = a2colors,
    name = "In(2L)t Genotype",
    labels = c("Standard", "Inverted/Standard", "Inverted")
  ) +
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", phenotype == "Activity"),
                aes(x = temp, y = Value, color = inv.st),  # beeswarm stays on color
                cex = 0.5,
                dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.4, position = position_dodge(width = 0.5), 
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5), show.legend = TRUE, 
             size = 3, color = "black", pch = 21) +
  theme_bw(base_size = 15)
a4
#fig 1
ggsave( ((a1 + a2) /(a3 + a4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) + 
          plot_annotation(
            tag_levels = c("A")) ,
        file = "/Users/adamlenhart/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/srFig1.3.2.pdf", width = 8, height = 7)

