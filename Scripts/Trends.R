#!/usr/bin/env Rscript
rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                             Trend analysis                                   #
#                        Updated - 24.01.2024                                  #
################################################################################
library(mblm)
library(Kendall)
library(tidyverse)
library(patchwork)
# --- Plankton -----
pp <- read.delim("Output/Data/Weekly_Full_PP.txt",
                 sep = ";") |> 
  mutate(End = round(End), Start = round(Start))
zp <- read.delim("Output/Data/Weekly_Full_ZP.txt",
                 sep = ";") |>
  dplyr::select(Year, Station, Taxa, 
                Timing, Maximum, Magnitude,
                Start, End, Middle,
                Duration, Group) |> 
  mutate(End = round(End), Start = round(Start))

# ---- Prepare the datasets in long format ------- 

long_pp <- pp |>
  #  mutate(Magnitude = log(Magnitude),
  #         Maximum = log(Maximum))|>
  gather(Parameter, Value,
         seq(5, 11, 1))

long_zp <- zp |> 
  
  mutate(#Magnitude = log(Magnitude),
    #Maximum = log(Maximum),
    Start = as.numeric(Start),
    End = as.numeric(End),
    Timing = as.numeric(Timing)) |>  
  gather(Parameter, Value,
         seq(4, 10, 1))
# ----- z-scores -----
long_df <- 
  rbind(long_pp, long_zp) |> 
  group_by(Station, Taxa, Parameter) |> 
  mutate(z = (Value - mean(Value)) / sd(Value))
rm(pp, zp, long_pp, long_zp)

# ---- Trend detection -----
output =  prediction = data.frame()
for(a in unique(long_df$Parameter)){
  df_try <- long_df |>
    filter(Parameter == a, Year <=2022)
  for(t in unique(df_try$Taxa)){
    df3 <- df_try |>
      filter(Taxa == t)
    for (s in unique(df3$Station)){
      df4 <- df3 |>
        filter(Station == s)
      res <- mblm(z ~ Year,
                  data = df4)

      K <- MannKendall(df4$z)
      P = K$sl[1]
      pre <- predict(res,
                     df4)
      prediction = data.frame(predict = pre,
                              p_trend = K$sl[1],
                              p_slope = summary(res)$coefficients[8],
                              slope = res$coefficients[2],
                              conf_sup = confint(res,level = 0.95)[4],
                              conf_inf = confint(res,level = 0.95)[2],
                              intercept = res$coefficients[1],
                              Year = unique(df4$Year),
                              Station = s,
                              Parameter = a,
                              Taxa = t) |>
        rbind(prediction)
    }
  }
}

# Set the taxa order for the figure from first to reach the seasonal timing to last
Taxa_levels <- long_df |> 
  group_by(Taxa, Station, Parameter) |> 
  summarise(Avg = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  filter(Parameter %in% c("Timing"),
         Station == "BY31") |> 
  arrange(Avg)

# Station BY31
df_plot_updated <-
  prediction |> 
  dplyr::select(p_trend, p_slope, slope, conf_sup, conf_inf, Station, Parameter, Taxa) |> 
  unique() |> 
  filter(Parameter %in% c("Start", "End", "Magnitude", "Timing"),
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocera", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Synechococcales","Cyanobacteria")) |> 
  mutate(facet = ifelse(Parameter == "Magnitude", "b", "a"),
         P_slope = case_when(
           p_slope <= 0.05 ~ "P < 0.05",
           p_slope > 0.05 & p_slope <= 0.1 ~ "P < 0.1",
           p_slope > 0.1 ~ "P > 0.1"),
         P_trend = case_when(
           p_trend <= 0.05 ~ "P < 0.05",
           p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
           p_trend > 0.1 ~ "P > 0.1"),
         Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Taxa = factor(Taxa, levels = Taxa_levels$Taxa),
         Parameter = factor(Parameter, levels = c("Start", "Timing", "End", "Magnitude"))
         )



ggplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(data = df_plot_updated |> 
                  filter(Station == "BY31"),
                mapping = aes(y = slope, ymin = conf_inf, ymax= conf_sup, x = Taxa),
                width = 0) +
  geom_point(data = df_plot_updated |> 
               filter(Station == "BY31"),
             size = 4,
             mapping = aes(y = slope, shape = Parameter, x = Taxa, fill = P_trend)) +

  scale_shape_manual(values = c(21, 21, 21, 21))+
  scale_y_continuous() +
  facet_grid(Parameter~.) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "")
ggsave("Output/Figures/Fig_4_c.pdf", width = 5.5, height = 7.5)

# Suplementary figure: BY15
Taxa_levels_BY15 <- long_df |> 
  group_by(Taxa, Station, Parameter) |> 
  summarise(Avg = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  filter(Parameter %in% c("Timing"),
         Station == "BY15") |> 
  arrange(Avg)

Sup_fig_3_c <-
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(data = df_plot_updated |> 
                  filter(Station == "BY15") |> 
                  mutate(Taxa = factor(Taxa, levels = Taxa_levels_BY15$Taxa)),
                mapping = aes(y = slope, ymin = conf_inf, ymax= conf_sup, x = Taxa),
                width = 0) +
  geom_point(data = df_plot_updated |> 
               filter(Station == "BY15")|> 
               mutate(Taxa = factor(Taxa, levels = Taxa_levels_BY15$Taxa)),
             size = 4,
             mapping = aes(y = slope, shape = Parameter, x = Taxa, fill = P_trend)) +
  
  scale_shape_manual(values = c(21, 21, 21, 21))+
  scale_y_continuous() +
  facet_grid(Parameter~.) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "")

# Suplementary figure: BY5
Taxa_levels_BY5 <- long_df |> 
  group_by(Taxa, Station, Parameter) |> 
  summarise(Avg = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  filter(Parameter %in% c("Timing"),
         Station == "BY5") |> 
  arrange(Avg)

Sup_fig_3_f <-
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(data = df_plot_updated |> 
                  filter(Station == "BY5") |> 
                  mutate(Taxa = factor(Taxa, levels = Taxa_levels_BY5$Taxa)),
                mapping = aes(y = slope, ymin = conf_inf, ymax= conf_sup, x = Taxa),
                width = 0) +
  geom_point(data = df_plot_updated |> 
               filter(Station == "BY5")|> 
               mutate(Taxa = factor(Taxa, levels = Taxa_levels_BY15$Taxa)),
             size = 4,
             mapping = aes(y = slope, shape = Parameter, x = Taxa, fill = P_trend)) +
  
  scale_shape_manual(values = c(21, 21, 21, 21))+
  scale_y_continuous() +
  facet_grid(Parameter~.) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "")

Sup_fig_3_c + Sup_fig_3_f + plot_layout(guides = "collect")
ggsave("Output/Figures/Sup_Fig_S3_cf.pdf", width = 11, height = 7.5)

################################################################################
# Time - Series BY31
Fig_4_a <-
  long_df |> 
  mutate(Station = factor(Station, levels = c("BY31")),
         Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium","Summer_bloom","Synechococcales", "Cyanobacteria","Synchaeta","Copepoda","Acartia","Centropages","Temora","Pseudocalanus","Cladocera","Evadne","Bosmina","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |> 
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Zooplankton",
         Station %in% c("BY31"),
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocera", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Cyanobacteria","Synechococcales")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> 
  ggplot(mapping = aes(x = Year,
                       y = z,
                       fill = Parameter,
                       col = Parameter)) +
  geom_hline(yintercept = 0)+  
  geom_line(size = 1)+

  facet_grid(Taxa  ~  .)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2)) +
  #scale_shape_manual(values = c(21, 22)) +
  scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  #scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
blank <- tibble(Taxa = c("Empty1"),
                Year = 2009,
                Station = "BY31",
                Group = "Phytoplankton",
                Parameter = c("Timing", "Magnitude"),
                Value = NA,
                z = NA,
                facet = "a")

Fig_4_b <-
  long_df |> 
  mutate(Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium","Summer_bloom","Synechococcales", "Cyanobacteria","Synchaeta","Copepoda","Acartia","Centropages","Temora","Pseudocalanus","Cladocera","Evadne","Bosmina","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |> 
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Phytoplankton",
         Station == "BY31",
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocerans", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates","Synechococcales", "Cyanobacteria")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> as_tibble() |> rbind(blank) |> 
  ggplot(mapping = aes(x = Year,
                       y = z,
                       fill = Parameter,
                       col = Parameter)) +
  geom_hline(yintercept = 0)+  
  geom_line(size = 1)+
  
  facet_grid(Taxa  ~  .)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2))+
  #scale_shape_manual(values = c(21, 22)) +
  #scale_fill_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  scale_color_manual(values = c("#01665e","#5ab4ac","#c7eae5")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
Fig_4_a + Fig_4_b +plot_layout(guides = "collect")
ggsave("Output/Figures/Fig_4_ab.pdf", width = 5.5, height = 7.5)
# Sup Figure S3 : BY15 and BY5
blank_BY15_5 <- tibble(Taxa = c("Empty1", "Empty2"),
                       Year = 2009,
                       Station = c("BY15", "BY5"),
                       Group = "Zooplankton",
                       Parameter = c("Timing", "Magnitude"),
                       Value = NA,
                       z = NA,
                       facet = "a")
Sup_Fig_S3_ad <-
  long_df |> 
  mutate(Station = factor(Station, levels = c("BY15", "BY5")),
         Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium","Summer_bloom","Synechococcales", "Cyanobacteria","Synchaeta","Copepoda","Acartia","Centropages","Temora","Pseudocalanus","Cladocera","Evadne","Bosmina","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |>
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Zooplankton",
         Station %in% c("BY15", "BY5"),
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocera", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Cyanobacteria","Synechococcales")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> rbind(blank_BY15_5) |> 
  ggplot(mapping = aes(x = Year,
                       y = z,
                       fill = Parameter,
                       col = Parameter)) +
  geom_hline(yintercept = 0)+  
  geom_line(size = 1)+
  
  facet_grid(Taxa  ~  Station)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2)) +
  #scale_shape_manual(values = c(21, 22)) +
  scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  #scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)


Sup_Fig_S3_be <-
  long_df |> 
  mutate(Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium","Summer_bloom","Synechococcales", "Cyanobacteria","Synchaeta","Copepoda","Acartia","Centropages","Temora","Pseudocalanus","Cladocera","Evadne","Bosmina","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |> 
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Phytoplankton",
         Station %in% c("BY15", "BY5"),
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocerans", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates","Synechococcales", "Cyanobacteria")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> as_tibble() |> 
  ggplot(mapping = aes(x = Year,
                       y = z,
                       fill = Parameter,
                       col = Parameter)) +
  geom_hline(yintercept = 0)+  
  geom_line(size = 1)+
  
  facet_grid(Taxa  ~  Station)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2))+
  #scale_shape_manual(values = c(21, 22)) +
  #scale_fill_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  scale_color_manual(values = c("#01665e","#5ab4ac","#c7eae5")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
Sup_Fig_S3_ad + Sup_Fig_S3_be +plot_layout(guides = "collect")
ggsave("Output/Figures/Sup_Fig_S3_abde.pdf", width = 11, height = 7.5)

################################################################################
# --- Abiotic ----
df <-
  read.table("Output/Data/Abiotic.txt",
             sep = ";",
             dec = ".") |> 
  filter(Year > 2007) |> 
  mutate(season = case_when(
    month %in% 1:3 ~ "winter",
    month %in% 4:6 ~ "spring",
    month %in% 7:9 ~ "summer",
    month %in% 10:12 ~ "fall")) |> 
  group_by(Year, Parameter, Station, season) |> 
  summarise(Value = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  group_by(Station, Parameter, season) |> 
  mutate(z = (Value - mean(Value)) / sd(Value)) |> 
  ungroup() 
# ---- Trend detection -----
output =  prediction = data.frame()
for(a in unique(df$Parameter)){
  df_try <- df |>
    filter(Parameter == a)
  for(t in unique(df$season)){
    df3 <- df_try |>
      filter(season == t)
    for (s in unique(df3$Station)){
      df4 <- df3 |>
        filter(Station == s)
      res <- mblm(z ~ Year,
                  data = df4)
      K <- MannKendall(df4$z)
      P = K$sl[1]
      pre <- predict(res,
                     df4)
      confint(res, level = 0.95)[2]
      confint(res, level = 0.95)[4]
      prediction = data.frame(predict = pre,
                              p_trend = K$sl[1],
                              p_slope = summary(res)$coefficients[8],
                              slope = res$coefficients[2],
                              conf_inf = confint(res, level = 0.95)[2],
                              conf_sup = confint(res, level = 0.95)[4],
                              intercept = res$coefficients[1],
                              Year = unique(df4$Year),
                              Station = s,
                              Parameter = a,
                              season = t) |>
        rbind(prediction)
    }
  }
}

df_prediction <- 
  prediction |> 
  right_join(df) |> 
  filter(Parameter %in% c("Temp_0.20", "Salinity")) |> 
  
  ungroup() |> 
  dplyr::select(Station,
                Year,
                z,
                season,
                Parameter,
                p_trend,p_slope,
                slope,
                conf_inf,
                conf_sup,
                intercept)  |>unique() |> 
  mutate(Station = factor(Station,
                          levels = c("BY31", "BY15", "BY5")),
         season = factor(season,
                         levels = c("winter","spring", "summer", "fall"))) 
df_new <- 
  df_prediction |>
  dplyr::select(Station, season, Parameter, z, conf_inf, conf_sup, p_slope, p_trend, Year) |> 
  unique()
prediction_new <- 
  df_prediction |> 
  dplyr::select(Station, season, Parameter, slope,conf_inf, conf_sup, intercept, p_slope, p_trend) |> 
  unique()
rm(s)
################################################################################
Fig2_a <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = df_new |> 
              filter(Station == "BY31") |> 
              mutate(Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity"))),
            mapping = aes(x = Year, y = z, col = Parameter),
            size = 2) +
  scale_y_continuous(breaks = seq(-2,2,2), limits = c(-2.8,2.8)) +
  scale_x_continuous(breaks = seq(2008,2020,4)) +
  
  
  facet_grid( season~.) +
  scale_color_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
Fig2_b <- prediction_new |> 
  mutate(P_trend = case_when(p_trend <= 0.05 ~ "P < 0.05",
                             p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
                             p_trend > 0.1 ~ "P > 0.1"),
         Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity")))  |> 
  filter(Station == "BY31") |> 
  
  ggplot(mapping = aes(x = Parameter, y = slope))+
  geom_hline(yintercept = 0) +
  geom_errorbar(mapping = aes(ymin = conf_inf, ymax = conf_sup),
                width = 0) +
  geom_point(mapping = aes(fill = P_trend, col = P_trend),
             col = "black", size = 6, shape = 21) +
  facet_grid(season~.)+
  scale_y_continuous(breaks = seq(-.2,.3,.1), limits = c(-0.12,.25), expand = c(0,0))+
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +    
  scale_color_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))
Fig2_a +Fig2_b + plot_layout(guides = "collect", widths  = c(1, 0.4))
ggsave("Output/Figures/Fig_2.pdf", height = 6.5, width =  6)
# Station BY15:
Fig2_a_BY15 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = df_new |> 
              filter(Station == "BY15") |> 
              mutate(Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity"))),
            mapping = aes(x = Year, y = z, col = Parameter),
            size = 2) +
  scale_y_continuous(breaks = seq(-2,2,2), limits = c(-2.8,2.8)) +
  scale_x_continuous(breaks = seq(2008,2020,4)) +
  
  
  facet_grid( season~.) +
  scale_color_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
Fig2_b_BY15 <- prediction_new |> 
  mutate(P_trend = case_when(p_trend <= 0.05 ~ "P < 0.05",
                             p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
                             p_trend > 0.1 ~ "P > 0.1"),
         Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity")))  |> 
  filter(Station == "BY15") |> 
  
  ggplot(mapping = aes(x = Parameter, y = slope))+
  geom_hline(yintercept = 0) +
  geom_errorbar(mapping = aes(ymin = conf_inf, ymax = conf_sup),
                width = 0) +
  geom_point(mapping = aes(fill = P_trend, col = P_trend),
             col = "black", size = 6, shape = 21) +
  facet_grid(season~.)+
  scale_y_continuous(breaks = seq(-.2,.3,.1), limits = c(-0.12,.25), expand = c(0,0))+
  scale_fill_manual(values = c("#ef8a62", "#fddbc7"), "") +    
  scale_color_manual(values = c("#ef8a62", "#fddbc7"), "") +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))
Fig2_a_BY15 +Fig2_b_BY15 + plot_layout(guides = "collect", widths  = c(1, 0.4))
ggsave("Output/Figures/Sup_Fig_S2_ab.pdf", height = 6.5, width =  6)
# Station BY5
Fig2_a_BY5 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = df_new |> 
              filter(Station == "BY5") |> 
              mutate(Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity"))),
            mapping = aes(x = Year, y = z, col = Parameter),
            size = 2) +
  scale_y_continuous(breaks = seq(-2,2,2), limits = c(-2.8,2.8)) +
  scale_x_continuous(breaks = seq(2008,2020,4)) +
  
  
  facet_grid( season~.) +
  scale_color_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  
  labs(y = "z-scores",
       x = NULL)
Fig2_b_BY5 <- prediction_new |> 
  mutate(P_trend = case_when(p_trend <= 0.05 ~ "P < 0.05",
                             p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
                             p_trend > 0.1 ~ "P > 0.1"),
         Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Parameter = factor(Parameter, levels = c("Temp_0.20", "Salinity")))  |> 
  filter(Station == "BY5") |> 
  
  ggplot(mapping = aes(x = Parameter, y = slope))+
  geom_hline(yintercept = 0) +
  geom_errorbar(mapping = aes(ymin = conf_inf, ymax = conf_sup),
                width = 0) +
  geom_point(mapping = aes(fill = P_trend, col = P_trend),
             col = "black", size = 6, shape = 21) +
  facet_grid(season~.)+
  scale_y_continuous(breaks = seq(-.2,.3,.1), limits = c(-0.12,.25), expand = c(0,0))+
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +    
  scale_color_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    face = "italic"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))
Fig2_a_BY5 +Fig2_b_BY5 + plot_layout(guides = "collect", widths  = c(1, 0.4))
ggsave("Output/Figures/Sup_Fig_S2_cd.pdf", height = 6.5, width =  6)
################################################################################
rm(list=ls())
sessionInfo()
