library(ggplot2)
library(tidyverse)
library(ggdist)
library(patchwork)
library(ggbeeswarm)
library(ggtext)
library(colorspace)
library(ragg)
library(plotrix)
install.packages('gghalves')
library(gghalves)
library(RColorBrewer)
library(dplyr)

#### open data
setwd("/ath/to/data")
data <- read.delim2("ODresults_clean.txt",header = T,dec = ",")

#pivot data
data_long <- data %>% pivot_longer(cols = Glc_2_12C:et_2_25C,
                                   names_to = c("Media","Concentracion","Temp"),
                                   names_sep = "_",
                                   values_to = "OD_48hrs")

### order data
data_long$Media <- factor(data_long$Media,levels = c('Glc',
                                                     'Fru',
                                                     'Mal',
                                                     'Suc',
                                                     'et',
                                                     'glc2et'))

### mean data
data_wrangled <- data_long %>% group_by(cepa, Spp, Media, Concentracion,Temp)%>%
  summarise(average = mean(OD_48hrs, na.rm = T), sd= sd(OD_48hrs, na.rm = T))
data_wrangled

### variation coeficient calculation
glucose <- data_long %>%
  filter(Media == 'Glc')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

maltose <- data_long %>%
  filter(Media == 'Mal')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

fructose <- data_long %>%
  filter(Media == 'Fru')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

sucrose <- data_long %>%
  filter(Media == 'Suc')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

etanol <- data_long %>%
  filter(Media == 'et')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

etanol_estres <- data_long %>%
  filter(Media == 'glc2et')%>%
  summarize(mean = mean(OD_48hrs, na.rm = T), sd = sd(OD_48hrs, na.rm = T),
            CV = sd(OD_48hrs,na.rm = T)/mean(OD_48hrs, na.rm =T)*100)

##### colorfriendly pallete
display.brewer.all(colorblindFriendly=TRUE)
pal <- brewer.pal(6, "Dark2")

##### half violin plot
ggplot(data_wrangled, aes(x = Media, y = average  ,col = Media)) + 
  ggdist::stat_halfeye(aes(colour = Media, fill = after_scale(color)),
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .4, 
    alpha = .2)+
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none")+
  theme_bw(base_size = 20)+
  ylab('OD 620nm 48 h')+
  xlab('')
