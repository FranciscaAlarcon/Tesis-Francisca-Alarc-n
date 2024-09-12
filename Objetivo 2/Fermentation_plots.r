library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(conover.test)
library(nortest)
install.packages('multcomview')
library(multcompView)


###Open data
setwd("/path/to/dile")
data <- read.table("script_fermentation25C.txt",
                   header = T,
                   sep = "\t",
                   dec = ",")
###Long pivot Data
data_long <- data %>% 
  pivot_longer(
    cols = !Days,
    names_to = c("Strain","replicate","Pop"),
    names_sep = "_",
    values_to = "CO2_lost") %>%
  group_by(Strain,Days,Pop)%>%
  filter(Strain != 'CBS7001')
data_long

### mean and sd by group
data_wrangled <- data_long %>% #filter(Strain!="C.") %>%
  summarize(average = mean(CO2_lost,na.rm = T), sd = sd(CO2_lost, na.rm =T))
data_wrangled

### order plots
data_wrangled$Pop <- factor(data_wrangled$Pop,levels = c("S.cerevisiae",
                                                         "S.pastorianus",
                                                         "S.uvarum",
                                                         "CC",
                                                         "TT",
                                                         "RCV"))

### fermentation curves

p1 <- data_wrangled %>% #filter(Strain!="C.") %>%
  ggplot(aes(x=Days,y=average,group=Strain, colour= Pop))+
  geom_point(position=position_dodge(0.05), aes(size = 2))+
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=0.8,
                position=position_dodge(0.05),
                linetype=1)+
  geom_line(data=data_wrangled[!is.na(data_wrangled$average),],position=position_dodge(0.05))+
  ggtitle("Mosto cervecero 7° Brix a 25°C") +
  xlab("Días de fermentación") +
  ylab("Pérdida de dióxido de carbono (g/l)")+
  scale_color_manual(values = c("#117733",
                                "#999933",
                                "#CC6677",
                                "#AA4499",
                                "#44AA99",
                                "#DDCC77"))+
  ylim(-1,30)+
  xlim(-0.2,16)+
  theme_bw(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5))
p1

### save plot
xsetwd("/Users/frana/Dropbox/TesisFransicaAlarcon/Plots/")
ggsave("beerwort_22042024.png", dpi = 1200, type = "transparent",  width = 10, height = 7)


##### endpoint plot #####

###filter last day
dia_15 <- data_long %>% filter(Days == '15')

### filter data for errorbar
dia_15_datawrangled <- data_wrangled %>% filter(Days == '15')

### order plot by spp
dia_15$Pop <- factor(dia_15$Pop,levels = c("S.cerevisiae",
                                           "S.pastorianus",
                                           "S.uvarum",
                                           "CC",
                                           "TT",
                                           "RCV"))
### enpoint plot
p2 <- dia_15 %>% 
  ggplot(aes(x=reorder(Strain, -CO2_lost),y = CO2_lost,col=Pop))+
  geom_point(position=position_dodge(0.05),size = 3)+
  stat_summary(fun = mean, geom = "point", size = 2, color = "#4A4E50", aes(group = Strain))+
  geom_errorbar(data = dia_15_datawrangled, aes(x=Strain, ymin=average-sd, ymax=average+sd), width=0.2,
                inherit.aes = F)+
  ylim(0,30)+
  ggtitle("Mosto cervecero 7° Brix a 25°C") +
  xlab("") +
  ylab("Pérdida total de dióxido de carbono (g/l)")+
  scale_color_manual(values = c("#117733",
                                "#999933",
                                "#CC6677",
                                "#AA4499",
                                "#44AA99",
                                "#DDCC77"))+
  theme_bw()+
  theme_bw(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
p2

### save plot
setwd("/path/to/file")
ggsave("plot.png", dpi = 1200, type = "transparent",  width = 10, height = 7)

#### stats for endpoint

dia_15
value_num <- as.numeric(dia_15$CO2_lost)

### normality test
shapiro.test(value_num)
lillie.test(value_num)

### comparative non parametric test
kruskal.test(CO2_lost ~ Strain, data = dia_15)

###save results test
c <- conover.test(dia_15$CO2_lost, dia_15$Strain, method = 'Bonferroni',list = T,table = T)
c <- as.data.frame(c)

#### save stats
setwd("/path/to/stats/directory")
writexl::write_xlsx(c, 'p-values fermentaciones 25C.xlsx')
