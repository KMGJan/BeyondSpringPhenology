#!/usr/bin/env Rscript
rm(list=ls())
################################################################################
#                                                                 18.08.2022   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                         Plankton succession                                  #
#                         Updated - 29.01.2024                                 #
################################################################################
# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
################################################################################
# Load the datasets
## Zooplankton:
zp <- read.delim(
  "Output/Data/Weekly_Full_ZP.txt",
  sep = ";")

## Phytoplankton:
pp <- read.delim(
  "Output/Data/Weekly_Full_PP.txt",
  sep = ";")
# Temperature:
temp <- read.delim(
  "Output/Data/Abiotic.txt",
  sep=";")

################################################################################
# Combine zooplankton and phytoplankton datasets
df_new <- rbind(pp, zp)

library(knitr)

se <- function(x) {
  n <- length(x[!is.na(x)])
  if (n > 2) {
    out <- sd(x, na.rm = TRUE) / sqrt(n)
  } else {
    out <- NA
  }
  return(out)
}

df_new %>%
  group_by(Taxa, Station, Group) %>%
  summarise(timing = round(mean(Timing),0),
            seTiming = round(sd(Timing), 0),
            magnitude = round(mean(Magnitude),2),
            seMagnitude = round(sd(Magnitude),2)) %>%
  ungroup()%>%
  filter(Taxa %in% c("Acartia", "Centropages", "Copepoda","Cladocera", "Bosmina", "Evadne", "Cyanobacteria", "Dinoflagellates", "Mesodinium", "Pseudocalanus", "Spring_bloom", "Spring_Diatoms","Synechococcales", "Summer_bloom", "Fall_Diatoms","Synchaeta", "Temora"))%>%
  mutate(Peak = paste(timing, seTiming, sep ="±"),
         Magnitude = paste(magnitude, seMagnitude, sep = "±"))%>%
  dplyr::select(Group,Taxa, Station, Peak, Magnitude)%>% 
  gather(Parameter, Value, 4:5)%>% 
  spread(Station, Value) %>%
  dplyr::select(Parameter, Group, Taxa, BY31, BY15, BY5) %>%
  knitr::kable(caption = "mean ± sd within station", digits = 2)%>%
  
  print()

df_new %>%
  group_by(Taxa, Group) %>%
  summarise(timing = round(mean(Timing)),
            seTiming = round(sd(Timing)),
            magnitude = round(mean(Magnitude),1),
            seMagnitude = round(sd(Magnitude),1),
            start = round(mean(Start)),
            seStart = round(sd(Start)),
            end = round(mean(End)),
            seEnd = round(sd(End))) %>%
  ungroup()%>%
  filter(Taxa %in% c("Acartia", "Centropages", "Copepoda","Cladocera", "Cyanobacteria", "Dinoflagellates", "Mesodinium", "Pseudocalanus", "Spring_bloom", "Spring_Diatoms", "Summer_bloom","Synechococcales", "Fall_Diatoms","Synchaeta", "Temora"))%>%
  mutate(Peak = paste(timing, seTiming, sep ="±"),
         Magnitude = paste(magnitude, seMagnitude, sep = "±"),
         Start = paste(start, seStart, sep = "±"),
         End = paste(end, seEnd, sep = "±"))%>%
  dplyr::select(Group, Taxa, Magnitude, Start,Peak, End)%>% 
  knitr::kable(caption = "mean ± sd across stations", digits = 2) %>%
  
  print()
################################################################################
# Preapare some aestetics for the final plot
months = month.abb
scale_x_month <- scale_y_continuous(breaks = seq(0, 51, by = 4.333),
                                    labels = months,
                                    limits = c(-10, 365))
scale_x_blank <-  scale_y_continuous(breaks = seq(0, 51, by= 4.333),
                                     labels = rep("", 12),
                                     limits = c(-10, 365))
# --- Panel c and d ---
p_1_df <- df_new %>%
  group_by(Taxa, Station, Group) %>%
  summarise(Med = mean(Timing),
            Max = mean(End),
            Min = mean(Start),
            Magnitude = mean(Magnitude, na.rm=T)) %>% 
  mutate(Taxa = ifelse(Taxa == "Fall_Diatoms", "Fall Diatoms", ifelse(
    Taxa == "Spring_Diatoms", "Spring Diatoms", ifelse(
      Taxa == "Summer_bloom", "Summer bloom", ifelse(
        Taxa == "Spring_bloom", "Spring bloom", as.character(Taxa))))),
    Taxa = factor(Taxa,
                  levels = c("Fall Diatoms",
                             "Bosmina",
                             "Pseudocalanus",
                             "Eurytemora",
                             "Temora",
                             "Centropages",
                             "Acartia",

                             "Cladocera",
                             "Copepoda",
                             "Synechococcales",
                             "Cyanobacteria",
                             "Summer bloom",
                             "Other",
                             "Evadne",
                             "Synchaeta",
                             "Mesodinium",
                             "Dinoflagellates", 
                             "Spring bloom" ,
                             "Spring Diatoms")),
    Station = factor(Station,
                     levels= c("BY5", "BY15", "BY31")),
    Group = ifelse(Group == "Phytoplankton",
                   "Phytoplankton",
                   "Zooplankton")) %>% 
  filter(Taxa %in% c("Fall Diatoms",
                     "Pseudocalanus",
                     "Temora",
                     "Centropages",
                     "Acartia",
                     "Mesodinium",
                     "Evadne",
                     "Bosmina",
                     "Cladocera",
                     "Copepoda",
                     "Synechococcales",
                     "Summer bloom",
                     "Cyanobacteria",
                     "Synchaeta",
                     "Dinoflagellates",
                     "Spring bloom" ,
                     "Spring Diatoms")) %>%
  mutate(facet = ifelse(Taxa %in% c("Copepoda","Cladocera", "Spring bloom", "Summer bloom"), "a", "b"))

Fig_3 <- p_1_df %>%
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5"))) |> 
  
  ggplot()+
  ##Point for the Median timing and the size relative to the magnitude:
  geom_linerange(size=1,
                 mapping = aes(x = Taxa,
                               y = Med,
                               ymin = Min,
                               ymax = Max,
                               col = Station),
                 alpha = 1,
                 position = position_dodge(width = 0.7))+
  geom_point(mapping = aes(
    size = Magnitude,
    x = Taxa,
    y = Med,
    fill = Station),
    alpha = 1,
    shape=21,
    stroke = 1,
    position = position_dodge(width = .7)) +
  
  #Some aestetics, scales the fill and the sizes
  scale_size(breaks = c(50, 150, 250),
             range = c(3, 20)) +
  scale_fill_manual(values = c("#8da0cb","#fc8d62","#66c2a5"),
                    guide = "none") +
  scale_color_manual(values=c(1,1,1), guide="none")+
  scale_y_continuous(breaks = seq(0, 52, by = 4.333333),
                     limits = c(0, 52),
                     labels = c(month.abb, ""),
                     position = "left",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0))) +

  labs(x = NULL,
       y = NULL) +

  coord_flip() +
  theme_classic() +
  #Change the theme
  theme(axis.ticks = element_line(color = "black"),
        axis.line.x.top = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.y = element_text(face = "italic",
                                   color = "black",
                                   size=20,
                                   hjust = 1,
                                   vjust=.5),
        panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        panel.grid.major.x  = element_line(color = "grey70"),
        legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold",
                                  size = 15),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent",
                                         color = "transparent"),
        axis.text = element_text(face = "plain",
                                 color = "black",
                                 size=20,
                                 hjust = 0,
                                 vjust=1),
        strip.background = element_rect(fill = "transparent",
                                        color= "transparent"),
        strip.text = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10)))+
  facet_grid(facet ~., scales = "free", space = "free")
Fig_3

ggsave("Output/Figures/Fig_3.pdf", width = 12, height =7)

################################################################################
# --- panel a ----

panel_a <- temp %>%
  filter(Year <= 2022, Parameter %in% c("Temp_0.20")) %>%
  
  mutate(Station = factor(Station,
                          levels = c("BY31", "BY15", "BY5")),
         Period = case_when(
           Year %in% seq(1995, 2007, 1) ~ "1995-2007",
           Year %in% seq(2008, 2022, 1) ~ "2008-2022"),
         # To match the pannel a, the month should be converted to Week
         month = month * 30.5 - 30.5) %>% 
  group_by(month, Station, Period, Parameter)%>%
  summarise(Avg = mean(Value),
            SD = sd(Value),
            min= min(Value),
            max = max(Value),
            n=n()) %>% 
  
  filter(Period == "2008-2022") %>%
  
  ggplot(mapping = aes(x = month + 30.5/2,
                       y = Avg,
                       ymin = min,
                       ymax = max,
                       fill = Station)) +
  geom_linerange(position = position_dodge(width = 20),
                 size = 1) +
  geom_point(position = position_dodge(width = 20),
             size = 5, stroke=1, shape = 21) +
  scale_color_manual(values = c("grey50", 1)) +
  scale_y_continuous(breaks = seq(0, 20, 5)) +
  scale_x_continuous(breaks = seq(0, 365, 30.5),
                     labels = months,
                     limits = c(0, 365),
                     position = "top",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0))) +
  scale_fill_manual(values = c("#8da0cb","#fc8d62","#66c2a5")) +
  labs(x = NULL,
       y = "Temperature (°C)") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 20),
        axis.title.y = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        panel.grid.major.x  = element_line(color = "grey70"),
        legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold",
                                    size = 15),
        legend.position = "right",
        strip.text = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = "transparent"),
        axis.text.x = element_text(color = "black",
                                   size = 20, hjust =0))
panel_a

# ---- panel b ----
pannel_b <- 
  temp %>%
  filter(Year <= 2022, Parameter %in% c("Salinity")) %>%
  
  mutate(Station = factor(Station,
                          levels = c("BY31", "BY15", "BY5")),
         Period = case_when(
           Year %in% seq(1995, 2007, 1) ~ "1995-2007",
           Year %in% seq(2008, 2022, 1) ~ "2008-2022"),
         # To match the pannel a, the month should be converted to Week
         month = month * 30.5 - 30.5) %>% 
  group_by(month, Station, Period, Parameter)%>%
  summarise(Avg = mean(Value),
            SD = sd(Value),
            min= min(Value),
            max = max(Value),
            n=n()) %>% 
  
  filter(Period == "2008-2022") %>%
  
  ggplot(mapping = aes(x = month + 30.5/2,
                       y = Avg,
                       ymin = min,
                       ymax = max,
                       fill = Station)) +
  geom_linerange(position = position_dodge(width = 20),
                 size = 1) +
  geom_point(position = position_dodge(width = 20),
             size = 5, stroke=1, shape = 21) +
  scale_color_manual(values = c("grey50", 1)) +
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  scale_x_continuous(breaks = seq(0, 365, 30.5),
                     labels = months,
                     limits = c(0, 365),
                     position = "top",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0))) +
  scale_fill_manual(values = c("#8da0cb","#fc8d62","#66c2a5")) +
  labs(x = NULL,
       y = "Salinity") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 20),
        axis.title.y = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        panel.grid.major.x  = element_line(color = "grey70"),
        legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold",
                                    size = 15),
        legend.position = "right",
        strip.text = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = "transparent"),
        axis.text.x = element_blank())
Fig_1_de <- panel_a/pannel_b
Fig_1_de
ggsave("Output/Figures/Fig_1_de.pdf", width = 10, height = 8)
# Export the legend
legend <- cowplot::get_legend(legend_final)
# Combine pannel a, b and c
left <- panel_a / pannel_b /
  pannel_c_d +   
  plot_layout(height = c(1, 0.7, 4),
              guides = 'collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.box.background = element_rect(fill = "transparent",
                                             color = 1))

# Remove legend from pannel a and b
pannel_left <- left &
  plot_annotation(tag_levels = 'a', tag_suffix = '.') &
  theme(legend.position = 'none',
        plot.tag = element_text(size = 25))

pannel_left

# --- Map ----

# Import dataframe with the stations coordinates

###############################################################################
#         If it is the first time to run the code run this part               #
###############################################################################
#zooplankton <- readRDS("./../Data/zooplankton.rds")                          #
#station <- zooplankton %>%                                                   #
#  dplyr::select(Station, sample_latitude_dd, sample_longitude_dd) %>%        #
#  filter(Station %in% c("BY5 BORNHOLMSDJ",                                   #
#                        "BY15 GOTLANDSDJ",                                   #
#                        "BY31 LANDSORTSDJ")) %>%                             #
#  mutate(Station = ifelse(Station == "BY31 LANDSORTSDJ",                     #
#                          "BY31",                                            #
#                          ifelse(Station == "BY5 BORNHOLMSDJ",               #
#                                 "BY5",                                      #  
#                                 "BY15"))) %>%                               #
#  group_by(Station) %>%                                                      #
#  summarise(longitude = mean(as.numeric(sample_longitude_dd)),               #
#            latitude = mean(as.numeric(sample_latitude_dd))) %>%             #
#  as.data.frame() %>%                                                        #
#  unique()                                                                   #
#station %>% write_rds("./../Data/Station.rds")                               #
###############################################################################
#                           Otherwise start here                              #
###############################################################################
Station <- readRDS("Data/Station.rds") |> 
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5")))

# Import dataframe with the polygones of the worlds
world <- map_data("world")

# Plot a map of the sampling area
ggplot() +
  
  geom_polygon(data = world,
               mapping = aes(x = long,
                             y = lat,
                             group = group), 
               fill = NA,
               colour = "black") +
  # Set the limits of the map
  coord_map("ortho",
            xlim = c(14.5,
                     27.5),
            ylim = c(54.5,
                     65.5)) +
  scale_x_continuous(breaks = seq(15,
                                  25,
                                  5),
                     labels = c("15°E",
                                "20°E",
                                "25°E")) +
  scale_y_continuous(breaks = seq(55,
                                  65,
                                  2.5),
                     labels = c("55°N",
                                "57.5°N",
                                "60°N",
                                "62.5°N",
                                "65°N")) +
  labs(title = NULL, 
       x=NULL, y=NULL) +
  theme_bw() +
  theme(plot.background = element_blank(), 
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill="transparent"), 
        strip.background = element_blank(), 
        strip.text = element_blank(), 
        panel.spacing = unit(1, "mm"), 
        axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), 
        axis.ticks = element_line(colour = "black"), 
        axis.text = element_text(colour = "black",
                                 size = 15)) +
  
  geom_point(data = Station,
             mapping = aes(y = latitude,
                           x = longitude,
                           fill = Station),
             size = 8,
             stroke = 1,
             shape = 21) +
  geom_text(data = Station,
            mapping = aes(y = latitude + .3,
                          x = longitude + .5,
                          label = Station),
            size = 8) + 
  scale_fill_manual(values = c("#8da0cb","#fc8d62","#66c2a5"))
ggsave("Output/Figures/Fig_1_a.pdf")
rm(list=ls())
sessionInfo()


