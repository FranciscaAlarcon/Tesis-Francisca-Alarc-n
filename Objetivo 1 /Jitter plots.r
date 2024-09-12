### cargar librerias
library(ggplot2)
library(tidyverse)
library(ggdist)
library(patchwork)
library(ggbeeswarm)
library(ggtext)
library(colorspace)
library(ragg)
library(plotrix)

### open directory
setwd("/path/to/data/file")
### open file, orden de columnas del archivo recomendada: cepa|especie|replica|condicion1|condicion2...|condcion n
data <- read.delim2("data.txt",header = T,dec = ",")

### pivot data
data_long <- data %>% pivot_longer(cols = Glc_2_12C:et_2_25C,
                      names_to = c("Media","Concentracion","Temp"),
                      names_sep = "_",
                      values_to = "OD_48hrs")

####order for plots
data_long$Media <- factor(data_long$Media,levels = c("Glc",
                                                     "Fru",
                                                     "Suc",
                                                     "Mal",
                                                     "glc2et",
                                                     "et"))
data_long$Concentracion <- factor(data_long$Concentracion,levels = c("0.5",
                                                                     "2",
                                                                     "10",
                                                                     "20"))

data_long$Spp <- factor(data_long$Spp,levels = c("S. uvarum",
                                                 "S. cerevisiae(maltosa-negativa)",
                                                 "S. chiloensis(SA-C/CC)",
                                                 "S. chiloensis(SA-C/TT)",
                                                 "S. chiloensis(SA-C/RCV)",
                                                 "S. eubayanus"))

########## JITTER PLOT ##########

#calular promedio y desviación estándar para cada cepa por temperatura. 

mean_12C <- data_long %>% filter(Temp == "12C") %>% group_by(Spp,Media,Concentracion,Temp) %>%
  summarize(mean=mean(OD_48hrs,na.rm = T),sd=sd(OD_48hrs,na.rm=T))

mean_25C <- data_long %>% filter(Temp == "25C") %>% group_by(Spp,Media,Concentracion,Temp) %>%
  summarize(mean=mean(OD_48hrs,na.rm = T),sd=sd(OD_48hrs,na.rm=T))


#agrupamos promedios segun 'Media' y 'Temp' para construir barras de error

glc05_12C <- mean_12C %>% filter(Media == "Glc", Concentracion == "0.5")
glc2_12C <- mean_12C %>% filter(Media == "Glc", Concentracion == "2")
glc10_12C <- mean_12C %>% filter(Media == "Glc", Concentracion == "10")
glc20_12C <- mean_12C %>% filter(Media == "Glc", Concentracion == "20")

glc05_25C <- mean_25C %>% filter(Media == "Glc", Concentracion == "0.5")
glc2_25C <- mean_25C %>% filter(Media == "Glc", Concentracion == "2")
glc10_25C <- mean_25C %>% filter(Media == "Glc", Concentracion == "10")
glc20_25C <- mean_25C %>% filter(Media == "Glc", Concentracion == "20")

mal05_12C <- mean_12C %>% filter(Media == "Mal", Concentracion == "0.5")
mal2_12C <- mean_12C %>% filter(Media == "Mal", Concentracion == "2")
mal10_12C <- mean_12C %>% filter(Media == "Mal", Concentracion == "10")
mal20_12C <- mean_12C %>% filter(Media == "Mal", Concentracion == "20")

mal05_25C <- mean_25C %>% filter(Media == "Mal", Concentracion == "0.5")
mal2_25C <- mean_25C %>% filter(Media == "Mal", Concentracion == "2")
mal10_25C <- mean_25C %>% filter(Media == "Mal", Concentracion == "10")
mal20_25C <- mean_25C %>% filter(Media == "Mal", Concentracion == "20")

fru05_12C <- mean_12C %>% filter(Media == "Fru", Concentracion == "0.5")
fru2_12C <- mean_12C %>% filter(Media == "Fru", Concentracion == "2")
fru10_12C <- mean_12C %>% filter(Media == "Fru", Concentracion == "10")
fru20_12C <- mean_12C %>% filter(Media == "Fru", Concentracion == "20")

fru05_25C <- mean_25C %>% filter(Media == "Fru", Concentracion == "0.5")
fru2_25C <- mean_25C %>% filter(Media == "Fru", Concentracion == "2")
fru10_25C <- mean_25C %>% filter(Media == "Fru", Concentracion == "10")
fru20_25C <- mean_25C %>% filter(Media == "Fru", Concentracion == "20")

sac05_12C <- mean_12C %>% filter(Media == "Suc", Concentracion == "0.5")
sac2_12C <- mean_12C %>% filter(Media == "Suc", Concentracion == "2")
sac10_12C <- mean_12C %>% filter(Media == "Suc", Concentracion == "10")
sac20_12C <- mean_12C %>% filter(Media == "Suc", Concentracion == "20")

sac05_25C <- mean_25C %>% filter(Media == "Suc", Concentracion == "0.5")
sac2_25C <- mean_25C %>% filter(Media == "Suc", Concentracion == "2")
sac10_25C <- mean_25C %>% filter(Media == "Suc", Concentracion == "10")
sac20_25C <- mean_25C %>% filter(Media == "Suc", Concentracion == "20")

glc2et05_12C <- mean_12C %>% filter(Media == "glc2et", Concentracion == "0.5")
glc2et2_12C <- mean_12C %>% filter(Media == "glc2et", Concentracion == "2")
et2_12C <- mean_12C %>% filter(Media == "et", Concentracion == "2")

glc2et05_25C <- mean_25C %>% filter(Media == "glc2et", Concentracion == "0.5")
glc2et2_25C <- mean_25C %>% filter(Media == "glc2et", Concentracion == "2")
et2_25C <- mean_25C %>% filter(Media == "et", Concentracion == "2")


####jitterplot
jp_et2_25C <- data_long %>%
#filtrar segun temperatura, medio y concentración a graficar
  filter(Temp=="25C", Media == "et", Concentracion == "2")%>%
  ggplot(aes(data_long,x=Spp,y=OD_48hrs,color=Spp))+
  geom_jitter()+
  scale_color_manual(values = c("#CC6677",
                                "#332288",
                                "#AA4499",
                                "#44AA99",
                                "#DDCC77",
                                "#661100"))+
  labs(title = 'Etanol 2% v/v ')+
  stat_summary(fun = mean, geom = "point", size = 2, color = "#4A4E50", aes(group = Spp))+
  geom_errorbar(data = et2_25C, aes(x = Spp, ymin = mean - sd, ymax = mean + sd),width = 0.15,
                inherit.aes = F)+
  theme_bw(base_size = 15)+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.direction = "horizontal")+
  xlab("")+
  ylab("")+
  ylim(0,1.25)
jp_et2_25C


#plot print
plot <- ((jp_glc05_12C|jp_glc2_12C|jp_glc10_12C|jp_glc20_12C)/(jp_mal05_12C|jp_mal2_12C|jp_mal10_12C|jp_mal20_12C)) + plot_layout(guides = 'collect')&
theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))


plot <- ((jp_glc05_25C|jp_glc2_25C|jp_glc10_25C|jp_glc20_25C)/(jp_mal05_25C|jp_mal2_25C|jp_mal10_25C|jp_mal20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_fru05_12C|jp_fru2_12C|jp_fru10_12C|jp_fru20_12C)/(jp_sac05_12C|jp_sac2_12C|jp_sac10_12C|jp_sac20_12C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_fru05_25C|jp_fru2_25C|jp_fru10_25C|jp_fru20_25C)/(jp_sac05_25C|jp_sac2_25C|jp_sac10_25C|jp_sac20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_glc2_12C|jp_glc2et05_12C|jp_glc2et2_12C)/(jp_glc2_25C|jp_glc2et05_25C|jp_glc2et2_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- (jp_et2_12C|jp_et2_25C) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))


plot <- ((jp_glc05_12C|jp_glc2_12C|jp_glc10_12C|jp_glc20_12C)/(jp_glc05_25C|jp_glc2_25C|jp_glc10_25C|jp_glc20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_mal05_12C|jp_mal2_12C|jp_mal10_12C|jp_mal20_12C)/(jp_mal05_25C|jp_mal2_25C|jp_mal10_25C|jp_mal20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_fru05_12C|jp_fru2_12C|jp_fru10_12C|jp_fru20_12C)/(jp_fru05_25C|jp_fru2_25C|jp_fru10_25C|jp_fru20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

plot <- ((jp_sac05_12C|jp_sac2_12C|jp_sac10_12C|jp_sac20_12C)/(jp_sac05_25C|jp_sac2_25C|jp_sac10_25C|jp_sac20_25C)) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal',legend.text = element_text(face = 'italic'))

#### save figure
setwd("/path/to/file")
ggsave("glc&mal_12C.png", dpi = 1200, type = "transparent",  width = 12, height = 8)


### export figure (other way)
("/Path/to/file")
pdf("glc_mal_12C.pdf",
    width = 12,
    height = 8)
plot
dev.off()

##### decrease order plot####

### filter data for S. chiloensis
filtered_data <- data_long %>%
  filter(Media =="Mal",
         Concentracion == '2',
         ### select temperature (12 or 25)
         Temp == "25C",
         Spp != 'S. cerevisiae(maltosa-negativa)',
         Spp != 'S. uvarum',
         Spp != 'S. eubayanus')

### calculate of mean and standar deviation
data_wrangled <- filtered_data %>%
  group_by(cepa,Spp)%>%
  summarize(ODmax=mean(OD_48hrs,na.rm = T),sd=sd(OD_48hrs,na.rm=T))

### decrease plot
data_mal2_25C <-ggplot(data_wrangled,aes(x=reorder(cepa,-ODmax),y=ODmax,
                                         col=Spp,shape=Spp))+
  geom_jitter(size=3.5,position = position_dodge())+
  geom_errorbar(data = data_wrangled, aes(ymin = ODmax - sd,
                                          ymax = ODmax + sd),width = 0.4,
                position = position_dodge(width = 0.75))+
  labs(title = "OD 620nm en maltosa 2% a 25°C")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.text = element_text(face = "italic"))+
  scale_color_manual(values = c("S. chiloensis(SA-C/CC)"="#AA4499",
                                "S. chiloensis(SA-C/TT)"="#44AA99",
                                "S. chiloensis(SA-C/RCV)"="#DDCC77"))+
  xlab("")+
  ylab("OD 48hr")+
  ylim(0,1)

### plot 12° and 25° together 
(data_mal2_12C/data_mal2_25C) + plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom', legend.direction = 'horizontal')

### export figure
setwd("/Users/frana/Dropbox/TesisFransicaAlarcon/Plots/")
ggsave("decrease_mal2.png", dpi = 1200, type = "transparent",  width = 16, height = 8)

