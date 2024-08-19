BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(tidyverse)
install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)
install.packages("colorRamp2")
library(colorRamp2)
library(circlize)
install.packages("hrbrthemes")
library(hrbrthemes)
library(writexl)

### open data
setwd("Path/to/data.txt")
data <- read.delim2("ODresults_clean.txt",header = T,dec = ",") # , is decimal

### pivot data
data_long <- data %>% pivot_longer(cols = Glc_2_12C:et_2_25C,
                                   names_to = c("Media",'Concentracion',"Temp"),
                                   names_sep = "_",
                                   values_to = "OD_48hrs")

### order data
data_long$Media <- factor(data_long$Media,levels = c("Glc",
                                                     "Fru",
                                                     "Mal",
                                                     "Sac",
                                                     "glc2et",
                                                     "et"))

#mean data
heatmap_mean <- data %>%
  group_by(cepa,Spp)%>%
  summarise(Glc_0.5_12C=mean(Glc_0.5_12C,na.rm = T),
            Glc_2_12C=mean(Glc_2_12C,na.rm = T),
            Glc_10_12C=mean(Glc_10_12C,na.rm = T),
            Glc_20_12C=mean(Glc_20_12C,na.rm = T),
            Mal_0.5_12C=mean(Mal_0.5_12C,na.rm = T),
            Mal_2_12C=mean(Mal_2_12C,na.rm = T),
            Mal_10_12C=mean(Mal_10_12C,na.rm = T),
            Mal_20_12C=mean(Mal_20_12C,na.rm = T),
            Fru_0.5_12C=mean(Fru_0.5_12C,na.rm = T),
            Fru_2_12C=mean(Fru_2_12C,na.rm = T),
            Fru_10_12C=mean(Fru_10_12C,na.rm = T),
            Fru_20_12C=mean(Fru_20_12C,na.rm = T),
            Suc_0.5_12C=mean(Suc_0.5_12C,na.rm = T),
            Suc_2_12C=mean(Suc_2_12C,na.rm = T),
            Suc_10_12C=mean(Suc_10_12C,na.rm = T),
            Suc_20_12C=mean(Suc_20_12C,na.rm = T),
            glc2et_0.5_12C=mean(glc2et_0.5_12C,na.rm = T),
            glc2et_2_12C=mean(glc2et_2_12C,na.rm = T),
            et_2_12C=mean(et_2_12C,na.rm = T),
            Glc_0.5_25C=mean(Glc_0.5_25C,na.rm = T),
            Glc_2_25C=mean(Glc_2_25C,na.rm = T),
            Glc_10_25C=mean(Glc_10_25C,na.rm = T),
            Glc_20_25C=mean(Glc_2_25C,na.rm = T),
            Mal_0.5_25C=mean(Mal_0.5_25C,na.rm = T),
            Mal_2_25C=mean(Mal_2_25C,na.rm = T),
            Mal_10_25C=mean(Mal_10_25C,na.rm = T),
            Mal_20_25C=mean(Mal_20_25C,na.rm = T),
            Fru_0.5_25C=mean(Fru_0.5_25C,na.rm = T),
            Fru_2_25C=mean(Fru_2_25C,na.rm = T),
            Fru_10_25C=mean(Fru_10_25C,na.rm = T),
            Fru_20_25C=mean(Fru_20_25C,na.rm = T),
            Suc_0.5_25C=mean(Suc_0.5_25C,na.rm = T),
            Suc_2_25C=mean(Suc_2_25C,na.rm = T),
            Suc_10_25C=mean(Suc_10_25C,na.rm = T),
            Suc_20_25C=mean(Suc_20_25C,na.rm = T),
            glc2et_0.5_25C=mean(glc2et_0.5_25C,na.rm = T),
            glc2et_2_25C=mean(glc2et_2_25C,na.rm = T),
            et_2_25C=mean(et_2_25C,na.rm = T))
 
            
heatmap_data <- heatmap_mean[, !(names(heatmap_mean) %in% c("cepa", "Spp"))] # to keep only the values

# Convert the remaining data to a matrix
heatmap_matrix <- as.matrix(heatmap_data)
heatmap_matrix
#Set strains names
rownames(heatmap_matrix) = sapply(heatmap_mean$cepa,
                          function(x) strsplit(as.character(x),split = "\\\\")[[1]][1])
#Set strains Pop
pop_df = data.frame(heatmap_mean[,2])

pop_df$Spp <- factor(pop_df$Spp,levels = c("S. cerevisiae(vinica)",
                                           "S. cerevisiae(ale)",
                                           "S. uvarum",
                                           "S. pastorianus",
                                           "S. cerevisiae(maltosa-negativa)",
                                           "S. chiloensis(SA-C/CC)",
                                           "S. chiloensis(SA-C/TT)",
                                           "S. chiloensis(SA-C/RCV)",
                                           "S. eubayanus"))

# Create a heatmap using ComplexHeatmap Vx.x.x.
scaled_mat = (scale((heatmap_matrix)))
scaled_mat

### create split vector by media
split_vector <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,12)

### create pallete
my_palette <- colorRampPalette(c('blue', "white", "red"))(n = 100)

### heatmap plot
Heatmap(width = unit(20, "cm"),
        cluster_columns = F,
        scaled_mat,
        col=my_palette,
        name = "Z-score",
        rect_gp = gpar(col = "grey", lwd = 2),
        show_row_names = T,
        right_annotation = rowAnnotation(df = pop_df, col = list(Spp = c("S. cerevisiae(vinica)"="#88CCEE",
                                                                         "S. cerevisiae(ale)"="#999933",
                                                                         "S. uvarum"="#CC6677",
                                                                         "S. pastorianus"="#117733",
                                                                         "S. cerevisiae(maltosa-negativa)" = '#332288',
                                                                         "S. chiloensis(SA-C/CC)"="#AA4499",
                                                                         "S. chiloensis(SA-C/TT)"="#44AA99",
                                                                         "S. chiloensis(SA-C/RCV)"="#DDCC77",
                                                                         "S. eubayanus"="#661100"))),
        column_order=c("Glc_0.5_12C",
                       "Glc_2_12C",
                       "Glc_10_12C",
                       "Glc_20_12C",
                       "Mal_0.5_12C",
                       "Mal_2_12C",
                       "Mal_10_12C",
                       "Mal_20_12C",
                       "Fru_0.5_12C",
                       "Fru_2_12C",
                       "Fru_10_12C",
                       "Fru_20_12C",
                       "Suc_0.5_12C",
                       "Suc_2_12C",
                       "Suc_10_12C",
                       "Suc_20_12C",
                       "et_2_12C",
                       "glc2et_0.5_12C",
                       "glc2et_2_12C",
                       "Glc_0.5_25C",
                       "Glc_2_25C",
                       "Glc_10_25C",
                       "Glc_20_25C",
                       "Mal_0.5_25C",
                       "Mal_2_25C",
                       "Mal_10_25C",
                       "Mal_20_25C",
                       "Fru_0.5_25C",
                       "Fru_2_25C",
                       "Fru_10_25C",
                       "Fru_20_25C",
                       "Suc_0.5_25C",
                       "Suc_2_25C",
                       "Suc_10_25C",
                       "Suc_20_25C",
                       "et_2_25C",
                       "glc2et_0.5_25C",
                       "glc2et_2_25C"),
        column_split = split_vector, column_gap = unit(2,'mm'),
        row_split = 4)

