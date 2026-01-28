##### PhR Assay - Graphing #####


### Information ### 
# Created Date: 2025/12/20
# Last edited on: 2026/1/28


# This is a good reference for making graphs
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

########### Imports -----
#library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggthemes)
library(ggh4x)
library(svglite)

### importing fonts 
# I am using times new roman for the graphing, so I need to import fonts. 

#install.packages("extrafont")
library(extrafont)
font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output, just do this on your first immport
#fonts()                       #vector of font family names, use if necessary

########## Reading Data ---------
getwd()
setwd("20251203 PhR Graphing") # Set to your working directory

data <- read.csv("1220_PhR Summary.csv")|>
  mutate(Wavelength = as.character(Wavelength)) # numerical to char

# Change from "222" to "UV222" because units are important
# These values ("UV222" or "UV254") will show in the legends later on
data["Wavelength"] <- lapply(data["Wavelength"], gsub, pattern="222", replacement = "UV222")
data["Wavelength"] <- lapply(data["Wavelength"], gsub, pattern="254", replacement = "UV254")


# Factoring is helpful when you want to graph things in order, 
# otherwise you might get "6 hrs PhR" in front of "After UV"
data$Label <- factor(data$Label, levels = c("Before UV", "After UV", "3 hrs PhR", "6 hrs PhR"))
data$Irradiation <- factor(data$Irradiation, levels = c("No UV", "3 Log"))
data$Wavelength <-  factor(data$Wavelength, levels= c("UV222", "UV254", "No UV"))


########## statistics -----
# This T test is just for fun, the actual analysis will use ANOVA. 
vx <-  filter(data, Target == "T4", Wavelength == "254", Irradiation == "3 Log")
vy <- filter(data, Target == "T4", Wavelength == "222", Irradiation == "3 Log")

var.test(x=vx$LogReactivation,y=vy$LogReactivation)
t.test(x=vx$LogReactivation,y=vy$LogReactivation, var.equal=F,paired=F)

########### Graphing -----
## MS2 ---------
dMS2 <- data |>
  filter(Target  == "MS2") 

dT4 <- data |>
  filter(Target == "T4")

# This graph is just for MS2 but you can make the same for T4. 
g <- ggplot(dMS2, aes(x = Label, y = LRV, color = Wavelength))
g + geom_point(size = 2) +
  theme_base() +
  labs(x = "Light Incubation Duration", y = "LRV") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
  


## GRAPH (All data) ------

# p_ENに保存, save the graph to p_EN
p_EN <- ggplot(data, aes(x =Label, y = LRV, color = Wavelength, shape = Wavelength)) +
  geom_point(size = 4, 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = LRV-LRV.SD, ymax =LRV+LRV.SD), 
                width = 1,
                position=position_dodge(width=0.5)) +
  theme_base() +
  theme(text=element_text(family="Times New Roman", face="bold"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 13, face = "bold"),
        strip.text = element_text(size = 15)
        ) + 
  labs(y = "LRV") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~ Target, nrow = 1)

######グラフを出力/output
p_EN

# SAVE p_EN as a png file
ggsave(filename="phr_results_EN.png", plot = p_EN, dpi = 300, width = 8.0, height = 4.8)



###### Output the graph in Japanese-------------
# You can use the same graph as the English one
# the theme will override, so you only have to change the places you want to change to Japanese
p_EN +
  labs(y = "不活化率", color="波長", shape = "波長") +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")


# saving files in SVG format----------------
### The svg format keeps all elements of your graph in one file.
### You'll be able to edit single elements when it's difficult to adjust on R.
### Save in SVG format, paste it on a poerpoint file, and ungroup(グループ解除)


#install.packages("svglite")
library(svglite)
ggsave(filename="phr_results_JP.svg")



########
## I'm not using this graph but I'll leave it just in case
old_graph <- ggplot(data, aes(x = interaction(Wavelength,Label), y = LRV, color = Wavelength)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = LRV-LRV.SD, ymax =LRV+LRV.SD, width = 0.5)) +
  scale_x_discrete(guide = "axis_nested")+
  
  theme_base() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
        strip.text = element_text(size = 15),
  ) + 
  labs(y = "LRV") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~ Target, nrow = 1)