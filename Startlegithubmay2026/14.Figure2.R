library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)
library(ggbeeswarm)
library(ggbeeswarm)


phendata = as.data.table(readRDS("figuredataFINAL"))

# --- Summarized data ---
phendata2 = phendata %>%
  filter(stimili == "High Intensity") %>% 
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

# --- Raw data with matching transformations ---
phendata_raw = phendata %>%
  filter(stimili == "High Intensity",
         phenotype %in% c("duration", "Magnitude")) %>%
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex)) %>%
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted")))

a2colors = c("red", "magenta", "blue")

# --- b1: Startle duration by Sex at 25C ---
annotation_df <- data.frame(
  start = c(0.85),
  end   = c(1.15),
  y     = c(550),
  label = c("*")
)

b1 = phendata2 %>%
  filter(temp == 25, phenotype == "duration") %>%
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +
  xlab("Sex") +
  ylab("Startle duration (sec)") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, temp == 25, phenotype == "duration"),
                aes(x = sex, y = Value, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.1, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  # geom_signif(data = annotation_df,
  #             aes(xmin = start, xmax = end, annotations = label, y_position = y),
  #             textsize = 5, vjust = .5, tip_length = 0.03,
  #             manual = TRUE, inherit.aes = FALSE) +
  ylim(200, 560) +
  theme_bw(base_size = 15)
b1
# --- b2: Startle duration by Temp in Females ---
annotation_df <- data.frame(
  start = c(1.85),
  end   = c(2.15),
  y     = c(550),
  label = c("*")
)

b2 = phendata2 %>%
  filter(sex == "Female", phenotype == "duration") %>%
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +
  xlab("Temperature") +
  ylab("Startle duration (sec)") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", phenotype == "duration"),
                aes(x = temp, y = Value, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.1, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  # geom_signif(data = annotation_df,
  #             aes(xmin = start, xmax = end, annotations = label, y_position = y),
  #             textsize = 5, vjust = .5, tip_length = 0.03,
  #             manual = TRUE, inherit.aes = FALSE) +
  ylim(200, 560) +
  theme_bw(base_size = 15)
b2
# --- b3: Magnitude by Sex at 25C ---
annotation_df <- data.frame(
  start = c(1),
  end   = c(1.15),
  y     = c(1.2),
  label = c("*")
)

b3 = phendata2 %>%
  filter(temp == 25, phenotype == "Magnitude") %>%
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +
  xlab("Sex") +
  ylab("Induced speed (mm/s)") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, temp == 25, phenotype == "Magnitude"),
                aes(x = sex, y = Value, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.5, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  # geom_signif(data = annotation_df,
  #             aes(xmin = start, xmax = end, annotations = label, y_position = y),
  #             textsize = 5, vjust = .5, tip_length = 0.03,
  #             manual = TRUE, inherit.aes = FALSE) +
  theme_bw(base_size = 15)
b3
# --- b4: Magnitude by Temp in Females ---
annotation_df <- data.frame(
  start = c(  3,    2.85),
  end   = c( 3.15, 3.15),
  y     = c(  1.1,  1.46),
  label = c(  "***","***")
)

b4 = phendata2 %>%
  filter(sex == "Female", phenotype == "Magnitude") %>%
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +
  xlab("Temperature") +
  ylab("Induced speed (mm/s)") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", phenotype == "Magnitude"),
                aes(x = temp, y = Value, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.5, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = TRUE) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  theme_bw(base_size = 15)
b4
# --- Save ---
ggsave(
  ((b1 + b2) / (b3 + b4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) +
    plot_annotation(tag_levels = c("A")),
  file = "/Users/adamlenhart/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/srFig2.1.pdf",
  width = 8, height = 7
)
