library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Feb_2025_objects/")
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects//")

phendata = as.data.table(readRDS("fullfeed.n"))
#filter and clean data
phendata2 = phendata %>%

  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex)) %>% 
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted"))) %>% 
  group_by(inv.st , sex,temp) %>% 
  summarise(mean = ci(f.ratio)[1],
            uci = ci(f.ratio)[2],
            lci = ci(f.ratio)[3] 
            
  )


a2colors = c("red","magenta", "blue")


annotation_df <- data.frame(
  
  start = c(1,0.85, 2, 1.85 ),
  end = c(1.15, 1.15, 2.15, 2.15), 
  y = c(.16, .179, .06, .12),
  label = c("***", "*","*","*")
)

c1 = phendata2 %>% 
  filter(temp == 25) %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Ratio of time near food") +
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
+ ylim(-0.005, 0.185)
c1

annotation_df <- data.frame(
  
  start = c(0.85, 1, 2, 1.85, 3),
  end= c(1,1.15,  2.15, 2.15,   3.15), 
  y = c(.185, 0.195, .16,0.18, .22),
  label = c("**","***","***","*","***")
)

c2 = phendata2 %>% 
  filter(sex == "Female") %>% 
  mutate(temp = as.factor(temp)) %>% 
  ggplot() +  
  xlab("Temperature") +
  ylab("Ratio of time near food") +
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
+ ylim(-0.005, 0.24)
c2




#fig 1
ggsave( ((c1 + c2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) + 
          plot_annotation(
            tag_levels = c("A")) ,
        file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2025_objects/images/srFig3.3.pdf", width = 8, height = 4)



