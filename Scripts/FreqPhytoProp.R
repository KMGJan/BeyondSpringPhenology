rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                   Sampling frequency & Density plots                         #
################################################################################
library(patchwork)

# Fig S1 -----------------------------------------------------------------------
sup1a <- read.delim("Output/Data/sup_fig1a.txt",
                    sep = ";")|>
  mutate(facet = "Zooplankton")
sup1b <- read.delim("Output/Data/sup_fig1b.txt",
                    sep = ";") |>
  mutate(facet = "Phytoplankton")
sup1c <- read.delim("Output/Data/sup_fig1c.txt",
                    sep = ";")

sup1 <- sup1c |> 
  mutate(Year = year,
         Month = month,
         facet = "Temperature") |>
  dplyr::select(Year, Month, Station, n, facet) |>
  rbind(sup1a) |>
  rbind(sup1b)

sup1_tot <- sup1 |> 
  mutate(Station = factor(Station, 
                          levels = c("BY31",
                                     "BY15",
                                     "BY5")),
         facet = factor(facet,
                        levels = c("Zooplankton",
                                   "Phytoplankton",
                                   "Temperature")),
         Month = case_when(Month == 1 ~ "Jan",
                           Month == 2 ~ "Feb",
                           Month == 3 ~ "Mar",
                           Month == 4 ~ "Apr",
                           Month == 5 ~ "May",
                           Month == 6 ~ "Jun",
                           Month == 7 ~ "Jul",
                           Month == 8 ~ "Aug",
                           Month == 9 ~ "Sep",
                           Month == 10 ~ "Oct",
                           Month == 11 ~ "Nov",
                           Month == 12 ~ "Dec"),
         Month = factor(Month,
                        levels = c("Dec",
                                   "Nov",
                                   "Oct",
                                   "Sep",
                                   "Aug",
                                   "Jul",
                                   "Jun",
                                   "May",
                                   "Apr",
                                   "Mar",
                                   "Feb",
                                   "Jan"))) |> 

  ggplot(mapping = aes(y = Month,
                       x = Year,
                       fill = as.factor(n))) +
  geom_tile(col = 1) +
  labs(x = NULL,
       y = NULL) +
  coord_fixed()+
  scale_x_continuous(breaks = seq(2008,2021,2))+
  scale_fill_manual(values = c("#edf8fb",
                               "#b3cde3",
                               "#8c96c6",
                               "#88419d")) +
  facet_grid(Station~facet) +
  theme_classic()+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black",
                                 size = 13),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        strip.text = element_text(size = 15,
                                  color = "black"))
sup1_tot
print(sup1_tot)
ggsave("Output/Figures/FigS1.pdf",
       width = 9,
       height = 8)

# Fig S4 -----------------------------------------------------------------------
Density_pp <- read.delim("Output/Data/Density_pp.txt",
                         sep =";") |> 
  mutate(Group = "Phytoplankton")
Density_zp <- read.delim("Output/Data/Density_zp.txt",
                         sep = ";")

Blooms <- data.frame(
  x = c(mean(c(5*7, 22*7)), 
        mean(c(22*7, 37*7))),
  y = 1,
  Station = "BY31",
  label = c("Spring bloom", "Summer bloom")) |>
  transform(Station = factor(Station,
                             levels = c("BY31", "BY5")))
scale_x_month <- scale_x_continuous(breaks = seq(0, 365, 30.5),
                                    limits = c(0, 366),
                                    labels= month.abb,
                                    expand = expand_scale(mult = c(0, 0), 
                                                          add = c(0, 0)))
theme_zp <- theme_classic() +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black"),
        axis.text = element_text(color = "black"))
Density <- rbind(Density_pp,
                 Density_zp)
Total <- Density_pp |>
  filter(Taxa == "Total") |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")))
p2 <- Density_pp |>
  filter(Taxa !="Total") |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")),
         Taxa = factor(Taxa, 
                       levels = c("Other",
                                  
                                  "Mesodinium",
                                  "Diatoms",
                                  "Dinoflagellates",
                                  "Synechococcales",
                                  "Cyanobacteria"))) |>
  
  ggplot(mapping = aes(x = DOY,
                       y = after_stat(count))) +
  geom_density(mapping = aes(fill = Taxa),
               position = "fill",
               col = NA,
               alpha = .8,
               size = 1.1) +
  geom_density(data = Total,
               aes(x = DOY,
                   y = ..count../160),
               fill = NA,
               size = 1.2) +
  #  scale_color_manual(values = c("black", rep('transparent', 4))) +
  scale_fill_manual(values = c( "#ffffcc","#d9f0a3","#addd8e","#78c679","#31a354","#006837")) +
  
  
  scale_y_continuous("Contribution",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0)),
                     breaks = seq(0,0.75,0.25),
                     sec.axis = sec_axis( trans=~.*160, name="Biomass", breaks = seq(0,150,50))) +
  theme_zp +
  facet_grid(Station~.)+
  scale_x_month +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black",
                                   face = "plain",
                                   size = 12),
        legend.position = "right",
        panel.spacing = unit(0, "points"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 15),
        axis.text.x = element_text(hjust = 0),
        axis.title = element_text(colour = "black",
                                  size = 15),
        panel.background = element_rect(colour = "transparent",
                                        fill = "transparent")) +
  geom_vline(xintercept = c(5*7, 22*7),
             lty = "dotted") +
  geom_vline(xintercept = c(37*7),
             lty = "dotted") +
  
  guides(fill = guide_legend(override.aes = list(color = NA)))

p2;print(p2)


zp_df <- Density_zp |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")),
         Taxa = factor(Taxa, 
                       levels = c("Temora",
                                           "Acartia", 
                                           "Centropages",
                                           "Eurytemora", 
                                           "Pseudocalanus",
                                           
                                           "Bosmina",
                                           "Podon",
                                           "Evadne",
                                           "Keratella",
                                           "Synchaeta", "Total")))
p3 <- 
  ggplot(mapping = aes(x = DOY,
                       y = after_stat(count))) +
  geom_density(data = zp_df |> filter(Taxa !="Total"),
               mapping = aes(fill = Taxa),
               position = "fill",
               col = NA,
               alpha = .8,
               size = 1.1) +
  geom_density(data = zp_df |> filter(Taxa == "Total"),
               aes(
                   y = after_stat(count)/300),
               fill = NA,
               size = 1.2) +
  #  scale_color_manual(values = c("black", rep('transparent', 4))) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d", "#f6e8c3","#b2182b","#d6604d","#762a83")) +
  
  scale_y_continuous("Contribution",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0)),
                     breaks = seq(0,0.75,0.25),
                     sec.axis = sec_axis( trans=~.*300, name="Biomass", breaks = seq(0,300,75))) +
  theme_zp +
  facet_grid(Station~.)+
  scale_x_month +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black",
                                   face = "plain",
                                   size = 12),
        legend.position = "right",
        panel.spacing = unit(0, "points"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 15),
        axis.text.x = element_text(hjust = 0),
        axis.title = element_text(colour = "black",
                                  size = 15),
        panel.background = element_rect(colour = "transparent",
                                        fill = "transparent")) +
  geom_vline(xintercept = c(42, 150),
             lty = "dotted") +
  geom_vline(xintercept = c(258),
             lty = "dotted") +
  
  guides(fill = guide_legend(override.aes = list(color = NA)))
p2 + p3 + plot_layout(guides = "collect")& theme(legend.position = 'bottom')

ggsave("Output/Figures/FigS2.pdf",
       height = 7,
       width = 13)

rm(list = ls())
sessionInfo()
