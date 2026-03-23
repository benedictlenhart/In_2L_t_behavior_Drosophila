library(data.table)
library(tidyverse)
library(patchwork)
library(gmodels)
library(ggsignif)
library(stringr)


setwd("Documents/Bergland Research/R_data_objects/Jan_2025_objects//")

phendata = as.data.table(readRDS("fullprop"))
#filter and clean data
phendata2 = phendata %>%
  # filter(sex == "male") %>% 
  mutate(inv.st = case_when(inv.st == "K1.Standard" ~ "Standard",
                            inv.st == "K3.Inverted" ~ "Inverted",
                            T ~ "Inverted/Standard"),
         sex = str_to_title(sex),
         region = as.factor(region)) %>% 
  mutate(inv.st = factor(inv.st, levels = c("Standard","Inverted/Standard","Inverted"))) %>% 
  group_by(inv.st , sex,temp,region) %>% 
  summarise(mean = ci(region.prop)[1],
            uci = ci(region.prop)[2],
            lci = ci(region.prop)[3] 
            
  )




a2colors = c("red","magenta", "blue")


# annotation_df <- data.frame(
#   
#   start = c(1,0.85, 2, 1.85 ),
#   end = c(1.15, 1.15, 2.15, 2.15), 
#   y = c(.16, .179, .06, .12),
#   label = c("***", "*","*","*")
# )

c1 = phendata2 %>% 
  filter(temp == 25,
         region == 1) %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Proportion of time near food") +
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
  ),position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15)
#   geom_signif(
#     data = annotation_df,
#     aes(xmin = start, xmax = end, annotations = label, y_position = y),
#     textsize = 5, vjust = .5,
#     tip_length = 0.03,
#     manual = TRUE
#   ) 
# + ylim(-0.005, 0.185)
c1

annotation_df <- data.frame(
  
  start = c(0.85, 2.85, 3),
  end= c(1.15, 3.15,3.15), 
  y = c(.185,0.25, .235),
  label = c("*","**","**")
)

c2 = phendata2 %>% 
  filter(sex == "Female",
         region == 1) %>% 
  mutate(temp = as.factor(temp)) %>% 
  ggplot() +  
  xlab("Temperature") +
  ylab("Proportion of time near food") +
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

c2

#now show the away from food trait
annotation_df <- data.frame(
  temp = c(20,20,20,20,20,
           25,25,
           30,30,30,30,30,30,30),
  start = c(0.85, 3.85, 4,7.85, 7.85,
            7.85, 7.85,
            0.85, 1, 1.85,6, 7.85, 8, 7.85),
  end= c(1.15, 4.15,4.15, 8.15, 8,
         8.15, 8,
         1.15, 1.15, 2.15, 6.15, 8, 8.15, 8.15 ), 
  y = c(.185,0.15, .2, 0.27, 0.32,
        0.27, 0.32,
        0.27, 0.23, 0.2, 0.135,0.25, 0.3, 0.35),
  label = c("*","**","*","***","***",
            "***","***",
            "**","**","**","*","***","***","**")
)

annotation_df <- data.frame(
  
  start = c(0.85, 0.85, 1.85),
  end= c(1.15, 1,2), 
  y = c(.28,0.26, .2),
  label = c("***","***","**")
)
c3 = phendata2 %>% 
  filter(temp == 25,
         region == 8) %>% 
  ggplot() +  
  xlab("Sex") +
  ylab("Proportion of time near food") +
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
  ),position=position_dodge(width = 0.5), show.legend = F) + theme_bw(base_size = 15)+
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = .5,
    tip_length = 0.03,
    manual = TRUE
  )
+ ylim(-0.005, 0.185)
c3

annotation_df <- data.frame(
  
  start = c(0.85, 0.85, 1.85, 1.85, 2.85, 3, 2.85),
  end= c(1, 1.15, 2, 2.15, 3, 3.15, 3.15), 
  y = c(.26, .275, .26, .27, .25, .275, .285),
  label = c("***","***","***","***","***","**","***")
)

c4 = phendata2 %>% 
  filter(sex == "Female",
         region == 8) %>% 
  mutate(temp = as.factor(temp)) %>% 
  ggplot() +  
  xlab("Temperature") +
  ylab("Proportion of time near food") +
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

c4
#create a ribbon graph to show the change in proportion across the tube



#fig 1
ggsave( ((c1 + c2) / (c3+c4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) + 
          plot_annotation(
            tag_levels = c("A")) ,
        file = "/Users/adamlenhart/Documents/Bergland Research/R_data_objects/June_2025_objects/srFig4aug6.pdf", width = 8, height = 8)



