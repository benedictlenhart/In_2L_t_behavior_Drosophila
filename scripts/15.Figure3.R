library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)
library(ggbeeswarm)


phendata = as.data.table(readRDS("positionsFINAL"))

# --- Summarized data ---
phendata2 = phendata %>%
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex),
         region = as.factor(region)) %>%
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted"))) %>%
  group_by(inv.st, sex, temp, region) %>%
  summarise(mean = ci(region.prop)[1],
            uci  = ci(region.prop)[2],
            lci  = ci(region.prop)[3])

# --- Raw data ---
phendata_raw = phendata %>%
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex),
         region = as.factor(region)) %>%
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted")))

a2colors = c("red", "magenta", "blue")

# --- c1: Region 1 by Sex at 25C (no annotation) ---
c1 = phendata2 %>%
  filter(temp == 25, region == 1) %>%
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +
  xlab("Sex") +
  ylab("Proportion of time near food") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, temp == 25, region == 1),
                aes(x = sex, y = region.prop, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.5, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  theme_bw(base_size = 15)
c1
# --- c2: Region 1 by Temp in Females ---
annotation_df <- data.frame(
  start = c(2.85, 3),
  end   = c( 3.15, 3.15),
  y     = c( 0.32, 0.27),
  label = c("*", "*")
)

c2 = phendata2 %>%
  filter(sex == "Female", region == 1) %>%
  mutate(temp = as.factor(temp)) %>%
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +
  xlab("Temperature") +
  ylab("Proportion of time near food") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", region == 1) %>%
                  mutate(temp = as.factor(temp)),
                aes(x = temp, y = region.prop, color = inv.st),
                cex = 0.3, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.5, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  theme_bw(base_size = 15)
c2
# --- c3: Region 8 by Sex at 25C ---
annotation_df <- data.frame(
  start = c(0.85, 0.85, 1.85),
  end   = c(1.15, 1,    2),
  y     = c(0.30, 0.275, 0.22),
  label = c("***", "***", "**")
)

c3 = phendata2 %>%
  filter(temp == 25, region == 8) %>%
  ggplot(aes(x = sex, y = mean, fill = inv.st)) +
  xlab("Sex") +
  ylab("Proportion of time fartherest from food") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, temp == 25, region == 8),
                aes(x = sex, y = region.prop, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.1, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = FALSE) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  ylim(-0.005, 0.30) +
  theme_bw(base_size = 15)
c3

# --- c4: Region 8 by Temp in Females ---
annotation_df <- data.frame(
  start = c(0.85, 0.85, 1.85, 1.85, 2.85, 3,    2.85),
  end   = c(1,    1.15, 2,    2.15, 3,    3.15,  3.15),
  y     = c(0.3, 0.35, 0.3, 0.35, 0.3, 0.35, 0.4),
  label = c("***", "***", "***", "***", "***", "**", "***")
)

c4 = phendata2 %>%
  filter(sex == "Female", region == 8) %>%
  mutate(temp = as.factor(temp)) %>%
  ggplot(aes(x = temp, y = mean, fill = inv.st)) +
  xlab("Temperature") +
  ylab("Proportion of time farthest from food") +
  scale_color_manual(values = a2colors, guide = "none") +
  scale_fill_manual(values = a2colors,
                    name = "In(2L)t Genotype",
                    labels = c("Standard", "Inverted/Standard", "Inverted")) +
  geom_beeswarm(data = filter(phendata_raw, sex == "Female", region == 8) %>%
                  mutate(temp = as.factor(temp)),
                aes(x = temp, y = region.prop, color = inv.st),
                cex = 0.5, dodge.width = 0.5,
                alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0.1, position = position_dodge(width = 0.5),
                show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3, color = "black", pch = 21, show.legend = TRUE) +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = .5, tip_length = 0.03,
              manual = TRUE, inherit.aes = FALSE) +
  theme_bw(base_size = 15)
c4
# --- Save ---
ggsave(
  ((c1 + c2) / (c3 + c4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) +
    plot_annotation(tag_levels = c("A")),
  file = "/Users/adamlenhart/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/Fig3.1.pdf",
  width = 8, height = 8
)
