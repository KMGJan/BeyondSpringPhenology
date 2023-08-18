rm(list=ls())
################################################################################
#                                                                 18.08.2022   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                         Plankton succession                                  #
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
  summarise(timing = round(mean(Timing),2),
            seTiming = round(se(Timing), 2),
            magnitude = round(mean(Magnitude),2),
            seMagnitude = round(se(Magnitude),2)) %>%
  ungroup()%>%
  filter(Taxa %in% c("Acartia", "Centropages", "Copepoda","Cladocera", "Bosmina", "Evadne", "Cyanobacteria", "Dinoflagellates", "Mesodinium", "Pseudocalanus", "Spring_bloom", "Spring_Diatoms","Synechococcales", "Summer_bloom", "Fall_Diatoms","Synchaeta", "Temora"))%>%
  mutate(Peak = paste(timing, seTiming, sep ="±"),
         Magnitude = paste(magnitude, seMagnitude, sep = "±"))%>%
  dplyr::select(Group,Taxa, Station, Peak, Magnitude)%>% 
  gather(Parameter, Value, 4:5)%>% 
  spread(Station, Value) %>%
  dplyr::select(Parameter, Group, Taxa, BY31, BY15, BY5) %>%
  knitr::kable(caption = "mean ± se within station", digits = 2)%>%
  
  print()

df_new %>%
  group_by(Taxa, Group) %>%
  summarise(timing = round(mean(Timing)),
            seTiming = round(se(Timing)),
            magnitude = round(mean(Magnitude),1),
            seMagnitude = round(se(Magnitude),1),
            start = round(mean(Start)),
            seStart = round(se(Start)),
            end = round(mean(End)),
            seEnd = round(se(End))) %>%
  ungroup()%>%
  filter(Taxa %in% c("Acartia", "Centropages", "Copepoda","Cladocera", "Cyanobacteria", "Dinoflagellates", "Mesodinium", "Pseudocalanus", "Spring_bloom", "Spring_Diatoms", "Summer_bloom","Synechococcales", "Fall_Diatoms","Synchaeta", "Temora"))%>%
  mutate(Peak = paste(timing, seTiming, sep ="±"),
         Magnitude = paste(magnitude, seMagnitude, sep = "±"),
         Start = paste(start, seStart, sep = "±"),
         End = paste(end, seEnd, sep = "±"))%>%
  dplyr::select(Group, Taxa, Magnitude, Start,Peak, End)%>% 
  knitr::kable(caption = "mean ± se across stations", digits = 2) %>%
  
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
                             "Evadne",
                             "Cladocera",
                             "Copepoda",
                             "Synechococcales",
                             "Cyanobacteria",
                             "Summer bloom",
                             "Other",
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

pannel_c_d <- p_1_df %>%
  mutate(Group = paste(Group, Station, sep="_"),
         Group = factor(Group, 
                        levels = c("Zooplankton_BY5",
                                   "Zooplankton_BY15",
                                   "Zooplankton_BY31",
                                   "Phytoplankton_BY5",
                                   "Phytoplankton_BY15",
                                   "Phytoplankton_BY31")))%>% 
  ggplot()+
  ##Point for the Median timing and the size relative to the magnitude:
  geom_linerange(size=2,
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
    fill = Group),
    alpha = 1,
    shape=21,
    stroke = 1,
    position = position_dodge(width = .7)) +
  
  #Some aestetics, scales the fill and the sizes
  scale_size(breaks = c(50, 150, 250),
             range = c(3, 20)) +
  scale_fill_manual(values = c( "#f6e8c3","#d8b365","#8c510a", 
                                "#c7eae5","#5ab4ac", "#01665e"),
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
pannel_c_d
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
                       fill = Station,
                       shape = Parameter)) +
  geom_linerange(position = position_dodge(width = 20),
                 size = 2) +
  geom_point(position = position_dodge(width = 20),
             size = 5, stroke=1) +
  scale_color_manual(values = c("grey50", 1)) +
  scale_shape_manual(values = c(24,24))+
  scale_y_continuous(breaks = seq(0, 20, 5)) +
  scale_x_continuous(breaks = seq(0, 365, 30.5),
                     labels = months,
                     limits = c(0, 365),
                     position = "top",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0))) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
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
                       fill = Station,
                       shape = Parameter)) +
  geom_linerange(position = position_dodge(width = 20),
                 size = 2) +
  geom_point(position = position_dodge(width = 20),
             size = 5, stroke=1) +
  scale_color_manual(values = c("grey50", 1)) +
  scale_shape_manual(values = c(25,24))+
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  scale_x_continuous(breaks = seq(0, 365, 30.5),
                     labels = months,
                     limits = c(0, 365),
                     position = "top",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0))) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
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
pannel_b
################################################################################
# For the legend...
legend_c_d <- pannel_c_d +
  theme_classic() +
  #Change the theme
  theme(axis.ticks = element_line(color = "black"),
        axis.line.x.bottom = element_blank(),
        axis.text.y = element_text(face = "italic",
                                   color = "black",
                                   size=20,
                                   hjust = .5,
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
        axis.text.x = element_text(face = "plain",
                                   color = "black",
                                   size=16,
                                   hjust = .5,
                                   vjust=.5)) +
  guides(fill = guide_legend(override.aes = list(size = 15)))
legend_final <- panel_a +   guides(fill = guide_legend(override.aes = list(size=15))) +  
  legend_c_d +
  
  plot_layout(height = c(1, 3),
              guides = 'collect') &
  theme(legend.position = 'right',
        legend.direction = 'horizontal',
        legend.box.background = element_rect(fill = "transparent",
                                             color =1),
        legend.background = element_rect(fill = "transparent",
                                         color = "transparent"),
        plot.background = element_rect(fill = "transparent"))
legend_final

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
################################################################################
# Combine the 3 pannels
final_fig<-cowplot::plot_grid(pannel_left,
                              legend,
                              rel_widths = c(5, 1),
                              label_size = 25,
                              label_fontface = "plain",
                              align = "hv")

final_fig
print(final_fig)
ggsave("Output/Figures/Fig1a-d.pdf",
       width = 10,
       height = 12)

# --- panel_e ----

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
Station <- readRDS("Data/Station.rds")

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
  scale_fill_manual(values = c("#999999",
                               "#4d4d4d",
                               "#e0e0e0"))
ggsave("Output/Figures/Fig1e.pdf")
rm(list=ls())
sessionInfo()