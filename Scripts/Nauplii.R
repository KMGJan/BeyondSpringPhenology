#!/usr/bin/env Rscript
rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                       Nauplii Phenology metrics                              #
################################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(roxygen2)
library(shaRk)
library(zoo)
library(xts)
library(reshape)

# Import the files -------------------------------------------------------------
# Bodymass.csv has the bodymass of all zooplankton

# Abundance data of zooplankton
zooplankton <- readRDS("Data/zooplankton_02Aug23.rds")

#  Filter and arrange the dataset zooplankton ----------------------------------
zp <- zooplankton  |> 
  # Arrange the data taxonomy as the bodymass data

  # Filter years between 2007 and 2021
  filter(Year > 2006, 
         Year < 2023,
         # at depth of 30 m
         Depth == 30,
         # corresponding to abundance data
         unit == "ind/m3",
         # Only Nauplii
         dev_stage_code == "NP",
         # from these classes
         Class %in% c("Maxillopoda"), # <----- Copepoda
         # And from these 3 stations
         Station %in% c("BY5 BORNHOLMSDJ",
                        "BY15 GOTLANDSDJ",
                        "BY31 LANDSORTSDJ")) |> 
  # Select only columns of interest 
  dplyr::select(Class,
                Order,
                Family,
                Genus,
                Species,
                SDATE,
                Yr_mon,
                Month,
                Year,
                Station,
                Day,
                Parameter,
                Value, 
                Depth, 
                dev_stage_code)

# Merge bodymass and zooplankton data
zoo_table <- zp |>
  mutate(STAGE = dev_stage_code,
         Station = ifelse(Station == "BY31 LANDSORTSDJ",
                          "BY31",
                          ifelse(Station == "BY5 BORNHOLMSDJ",
                                 "BY5",
                                 "BY15")),
         Location = ifelse(Station == "BY5",
                           "S",
                           "N")) |>
  # if the Genus is not know, assign the Taxa as the class,
  # else assign the taxa as the genus
  transform(Genus = ifelse(Genus == "", 
                           as.character(Class), 
                           as.character(Genus)),
            Taxa = as.character(Genus)) |>
  mutate(Taxa = ifelse(Species %in% c("Acartia bifilosa", 
                                      "Acartia longiremis",
                                      "Acartia tonsa",
                                      "Podon intermedius"),
                       as.character(Species),
                       as.character(Taxa)),
         Value = Value / 1000) |>
  
  mutate(Taxa = ifelse(Genus == "Acartia", 
                       "Acartia", 
                       ifelse(Genus == "Podon",
                              "Podon", 
                              as.character(Taxa)))) |>
  filter(Taxa %in% c("Acartia", "Centropages", "Pseudocalanus", "Temora"))
ggplot(data = zoo_table,
       mapping = aes(x = SDATE,
                     y = log(Value),
                     col = as.character(Depth))) +
  geom_point() +
  facet_grid(Station ~
               Taxa)

# Keep only the Taxa with high sampling frequencies ----------------------------
ztable2 <- zoo_table |>
  # We can assign the group copepoda, cladocera, and rotatoria to the genus
  mutate(Group = ifelse(Genus %in% c("Acartia",
                                     "Centropages",
                                     "Eurytemora",
                                     "Pseudocalanus",
                                     "Temora"),
                        "Copepoda",
                        ifelse(Genus %in%c("Bosmina",
                                           "Evadne",
                                           "Podon"),
                               "Cladocera",
                               "Rotatoria")))

# We can keep everything at BY31
ztable2 |>
  ggplot(aes(x=SDATE, y= log(Value)))+
  geom_point()+geom_line()+
  facet_grid(Station~Taxa, scales="free")
# Data check -------------------------------------------------------------------
# But first some function for later:
scale_x_month <- scale_x_continuous(breaks = seq(1, 12, 1),
                                    labels = month.abb)
theme_zp <- theme_classic() +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black"),
        axis.text = element_text(color = "black"))
facet_station <- facet_grid(Station ~ .,
                            scales = "free")
# Group by date and depth
d1 <- ztable2 |>
  group_by(Year,
           Yr_mon,
           Month,
           SDATE,
           Depth,
           Group,
           Genus,
           Station, 
           Taxa) |>
  # Sum of each sampling event
  summarise(Value = sum(Value)) |>
  ungroup() |>
  # Monthly average
  group_by(Year,
           Yr_mon,
           Month,
           Genus,
           Group,
           Station,
           Taxa) |>
  summarise(Value = mean(Value, na.rm = T))
# Check plot
d1 |>
  ggplot(mapping = aes(x = Yr_mon,
                       y = log(Value),
                       col = as.character(Month))) +
  geom_point() +
  facet_grid(Taxa ~ Station,
             scales = "free")

# Monthly average per Taxa
library(PlanktonData)
dGenus <- d1 |>
  filter(Taxa %in% c("Acartia", "Centropages", "Pseudocalanus", "Temora"))|>
  group_by(Month, 
           Group,
           Genus, 
           Station,
           Taxa) |>
  summarise(avg = mean(Value,
                         na.rm = T),
            SE = se(Value))

theme_FigS5 <- theme_classic() +
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

dGenus |>
  mutate(Station = factor(Station, 
                          levels= c("BY31","BY15", "BY5")),
         Taxa = factor(Taxa, 
                       levels = c("Synchaeta","Acartia", "Centropages", "Temora","Pseudocalanus","Evadne","Bosmina"))) |>
  ggplot(mapping = aes(x = Month, 
                       y = avg,
                       #col = Taxa,
                       fill = Taxa,
                       #col = Taxa,
                       ymin = avg-SE,
                       ymax = avg+SE)) +
  geom_line() +
  geom_ribbon(alpha = .4)+
  #geom_errorbar(width = 0)+
  geom_point(shape = 21,
             col = 1,
             size = 3) +
  scale_fill_manual(values = c("#dfc27d", "#f6e8c3", "#762a83", "#A5BEA4")) +
  scale_color_manual(values = c("#dfc27d", "#f6e8c3", "#762a83", "#A5BEA4")) +
  

  facet_grid(.~Station)+
  theme_FigS5 + 
  scale_x_continuous(breaks = seq(1, 12, by = 1),
                                    limits = c(1, 12),
                                    labels = c(month.abb),
                                    expand = expand_scale(mult = c(0, 0), 
                                                          add = c(0.5, .5)))+
  theme(legend.position = "bottom")

ggsave("Output/Figures/Sup_Fig_S5.pdf",
       width = 11, height = 4.5)

rm(list=ls())
sessionInfo()
