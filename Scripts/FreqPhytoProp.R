#!/usr/bin/env Rscript
rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                   Sampling frequency & Density plots                         #
################################################################################
library(patchwork)
library(tidyverse)

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
ggsave("Output/Figures/Sup_Fig_S1.pdf",
       width = 9,
       height = 8)
rm(list=ls())
# Fig 1 -----------------------------------------------------------------------
pp <- readRDS("Output/Data/Phytoplankton_after_interpolation.rds")
zp <- readRDS("Output/Data/zooplankton_sampling_after_interpolation.rds")
df <-
  pp |> 
  mutate(Gr = "phytoplankton") |> 
  rbind(zp |> 
          mutate(Gr = "zooplankton")) |>
  group_by(Week, Taxa, Gr, Station) |> 
  summarise(Avg = mean(as.numeric(value), na.rm = T)) |> 
  ungroup() |> as_tibble()

  
Density = data.frame()
for(i in unique(df$Station)){
  Interp <- df |>
    filter(Station == i)
  
  Density <- Interp[rep(row.names(Interp), Interp$Avg), 1:2] |>
    mutate(Station = i)|>
    rbind(Density)
}

theme_FigS4 <- theme_classic() +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black"),
        axis.title.x = element_blank(),
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
                                        fill = "transparent"))

# Phytoplankton :
p2 <-
  Density |>
  filter(Taxa %in% c("Other", "Mesodinium", "Diatoms", "Dinoflagellates", "Synechococcales", "Cyanobacteria")) |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")),
         Taxa = factor(Taxa, 
                       levels = c("Other", "Mesodinium", "Diatoms", "Dinoflagellates", "Synechococcales", "Cyanobacteria"))) |>
  
  ggplot(mapping = aes(x = Week,
                       y = after_stat(count))) +
  geom_density(mapping = aes(col = Taxa),
               alpha = .1,
               size = 1.2,
               show.legend = T) +
  
  geom_density(data = Density |>
                 filter(Taxa %in% c("Other", "Mesodinium", "Diatoms", "Dinoflagellates", "Synechococcales", "Cyanobacteria")) |> 
                 mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5"))),
               aes(x = Week,
                   y = after_stat(count)),
               fill = NA,
               size = 1.2) +
  #  scale_color_manual(values = c("black", rep('transparent', 4))) +
  scale_color_manual(values = c("#b2182b", "#ef8a62", "#fddbc7", "#7F93A0",  "#67a9cf",  "#2166ac")) +
  #  scale_fill_manual(values = c(alpha("#b2182b", .1), "#ef8a62", "#fddbc7", "#7F93A0",  "#67a9cf",  "#2166ac")) +
  
  scale_y_continuous("Biomass",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 2)),
                     breaks=seq(0,180, 40)) +
  theme_FigS4 +
  geom_vline(xintercept = c(5, 22, 37),
             lty = "dashed")  +
  facet_grid(Station~.) +
  
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  scale_x_continuous(breaks = seq(0, 52, by = 4.333333),
                     limits = c(0, 52),
                     labels = c(month.abb, ""),
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0)))

# Zooplankton :
p3 <- 
  Density |>
  filter(Taxa %in% c("Synchaeta","Acartia", "Centropages", "Temora","Pseudocalanus","Evadne","Bosmina")) |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")),
         Taxa = factor(Taxa, 
                       levels = c("Synchaeta","Acartia", "Centropages", "Temora","Pseudocalanus","Evadne","Bosmina"))) |>
  
  ggplot(mapping = aes(x = Week,
                       y = after_stat(count))) +
  geom_density(mapping = aes(col = Taxa),
               alpha = .1,
               size = 1.2,
               show_guide = T) +
  
  geom_density(data = Density |>
                 filter(Taxa %in% c("Synchaeta","Acartia", "Centropages", "Temora","Pseudocalanus","Evadne","Bosmina")) |> 
                 mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5"))),
               aes(x = Week,
                   y = after_stat(count)),
               fill = NA,
               size = 1.2) +
  #  scale_color_manual(values = c("black", rep('transparent', 4))) +
  scale_color_manual(values = c("#8c510a", "#dfc27d", "#f6e8c3", "#762a83", "#A5BEA4", "#35978f", "#01665e")) +
  #  scale_fill_manual(values = c(alpha("#b2182b", .1), "#ef8a62", "#fddbc7", "#7F93A0",  "#67a9cf",  "#2166ac")) +
  
  scale_y_continuous("Biomass",
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 2)),
                     breaks=seq(0,280, 60)) +
  theme_FigS4 +
  geom_vline(xintercept = c(5, 22, 37),
             lty = "dashed")  +
  facet_grid(Station~.) +
  
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  scale_x_continuous(breaks = seq(0, 52, by = 4.333333),
                     limits = c(0, 52),
                     labels = c(month.abb, ""),
                     expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 0)))
p3 + p2 + plot_layout(guides = "collect")& theme(legend.position = 'bottom')

ggsave("Output/Figures/Fig_1_bc.pdf",
       height = 10,
       width = 13)

#-------------------------------------------------------------------------------
# Nitrogen and stratification dynamics
library(PlanktonData)
df_month <-
  tibble(month = 1:12,
         Month = month.abb)
df_abiotic <-
  read.delim("Output/Data/Abiotic.txt", sep = ";") |> 
  filter(Parameter %in% c("Nutrient", "strat_index"),
         Year %in% 2008:2022) |> 
  right_join(df_month) |> 
  group_by(Month, Year, Parameter, Station) |> 
  summarise(Value = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  group_by(Month, Parameter, Station) |> 
  summarise(Avg = mean(Value, na.rm = T),
            SE = se(Value, na.rm = T)) |> 
  ungroup() |> 
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Month = factor(Month, levels = month.abb))

# Plot --------------------------------
ggplot() +
  geom_bar(data = df_abiotic |> 
               filter(Parameter == "Nutrient"),
           position = position_dodge2(),
           stat = "identity",
           mapping = aes(x = Month, y = Avg, fill = Station)) +
  geom_errorbar(data = df_abiotic |> 
                  filter(Parameter == "Nutrient"),
                position = position_dodge2(),
                mapping = aes(x = Month, y = Avg, ymin = Avg-SE, ymax = Avg + SE, group = Station)) +
  geom_errorbar(data = df_abiotic |> 
                  filter(Parameter == "strat_index"),
                position = position_dodge2(width = 0.9),
                mapping = aes(x = Month, y = Avg+2, ymin = Avg-SE+2, ymax = Avg + SE+2, group = Station)) +
  geom_point(data = df_abiotic |> 
             filter(Parameter == "strat_index"),
           position = position_dodge2(width = 0.9),
           stat = "identity",
           shape = 21, size = 2,
           mapping = aes(x = Month, y = Avg+2, fill = Station)) +
  scale_fill_manual(values = c("#54278f", "#9e9ac8", "#dadaeb")) +
  scale_y_continuous("Nitrogen /mu molL-1", sec.axis = sec_axis(trans = ~.-2, "stratification index (SST -T°C 60m)/ avg T°C 0-60m")) +
  theme_classic()

ggsave("Output/Figures/Sup_Fig_S4.pdf", width = 8, height = 3)
rm(list = ls())
sessionInfo()
