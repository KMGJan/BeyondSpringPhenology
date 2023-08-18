rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                             Trend analysis                                   #
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
                              intercept = res$coefficients[1],
                              Year = unique(df4$Year),
                              Station = s,
                              Parameter = a,
                              Taxa = t) |>
        rbind(prediction)
    }
  }
}
prediction
unique(prediction$Taxa)
df_plot <-
  prediction |> 
  dplyr::select(p_trend, p_slope, slope, Station, Parameter, Taxa) |> 
  unique() |> 
  filter(Parameter %in% c("Start", "End", "Magnitude", "Timing"),
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocerans", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Synechococcales","Cyanobacteria")) |> 
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
         Taxa = factor(Taxa, levels = c(
           "Cladocerans","Bosmina","Evadne","Synchaeta", "Copepoda", "Acartia",  "Temora", "Pseudocalanus", "Centropages",     "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium","Synechococcales", "Cyanobacteria","Summer_bloom","Fall_Diatoms"))
         )
long_df |>ungroup()|> 
  group_by(Taxa, Station, Parameter) |> 
  summarise(mean = mean(Value)) |> 
  ungroup() |> 
  filter(Parameter == "Timing", Station == "BY31")
df_plot|>
  filter(Parameter == "Timing",
         Station == "BY31")
ggplot() +
  geom_hline(yintercept = 0) +
  geom_bar(data = df_plot |> 
             filter(facet == "b"),
           mapping = aes(y = slope, x = Taxa, fill = P_trend),
           stat = "identity",
           width = .7,
           col = "black") +
  

  
  geom_point(data = df_plot |> 
               filter(facet == "a"),
             size = 4,
             mapping = aes(y = slope, shape = Parameter, x = Taxa, fill = P_trend),
             position = position_dodge2(width =.3)) +
  scale_shape_manual(values = c(21, 22, 23))+
  scale_y_continuous() +
  facet_grid(Station~.) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_line(color = "grey70"),
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

ggsave("Output/Figures/Fig4.pdf", width = 6, height = 7.5)
pa <-
  long_df |> 
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Cladocerans", "Bosmina","Evadne","Synchaeta","Copepoda", "Acartia",  "Temora", "Pseudocalanus", "Centropages",     "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium", "Cyanobacteria","Synechococcales","Summer_bloom","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |> 
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Zooplankton",
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocerans", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Cyanobacteria","Synechococcales")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> 
  ggplot(mapping = aes(x = Year,
                     y = z,
                     fill = Station,
                     col = Station)) +
  geom_line(size = 1.5)+
  
  facet_grid(Taxa  ~  Parameter)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2)) +
  #scale_shape_manual(values = c(21, 22)) +
  scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  #scale_color_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_line(color = "grey70"),
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
pa
blank <- tibble(Taxa = c("Empty1"),
                Year = 2009,
                Station = "BY31",
                Group = "Phytoplankton",
                Parameter = c("Timing", "Magnitude"),
                Value = NA,
                z = NA,
                facet = "a")
pb <-
  long_df |> 
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5")),
         Group = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"),
         Taxa = factor(Taxa, levels = c(
           "Bosmina","Evadne","Synchaeta",  "Acartia",  "Temora", "Pseudocalanus", "Centropages",     "Spring_bloom","Spring_Diatoms",  "Dinoflagellates","Mesodinium", "Summer_bloom","Cyanobacteria","Synechococcales","Fall_Diatoms")),
         facet = ifelse(Parameter == "Magnitude", "b", "a")) |> 
  filter(Parameter %in% c("Timing", "Magnitude"),
         Group == "Phytoplankton",
         Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocerans", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates","Synechococcales", "Cyanobacteria")) |> 
  mutate(Parameter = factor(Parameter, levels = c("Timing", "Magnitude"))) |> as_tibble() |> rbind(blank) |> 
  ggplot(mapping = aes(x = Year,
                       y = z,
                       fill = Station,
                       col = Station)) +
  geom_line(size = 1.5)+
  
  facet_grid(Taxa  ~  Parameter)+#, scale = "free") +
  scale_y_continuous(breaks = seq(-2,2,2))+
  #scale_shape_manual(values = c(21, 22)) +
  #scale_fill_manual(values = c("#8c510a","#d8b365",  "#f6e8c3")) +
  scale_color_manual(values = c("#01665e","#5ab4ac","#c7eae5")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_line(color = "grey70"),
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
pb
library(patchwork)
pa+pb
ggsave("Output/Figures/Fig3.pdf", height = 7, width = 9)

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
      prediction = data.frame(predict = pre,
                              p_trend = K$sl[1],
                              p_slope = summary(res)$coefficients[8],
                              slope = res$coefficients[2],
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
                intercept)  |>unique() |> 
  mutate(Station = factor(Station,
                          levels = c("BY31", "BY15", "BY5")),
         season = factor(season,
                         levels = c("winter","spring", "summer", "fall"))) 
df_new <- 
  df_prediction |>
  dplyr::select(Station, season, Parameter, z, p_slope, p_trend, Year) |> 
  unique()
prediction_new <- 
  df_prediction |> 
  dplyr::select(Station, season, Parameter, slope, intercept, p_slope, p_trend) |> 
  unique()


p1 <- 
  ggplot() +
  geom_line(data = df_new,
            mapping = aes(x = Year, y = z, col = Station),
            size = 1.5) +
  scale_y_continuous(breaks = seq(-2,2,2), limits = c(-2.8,2.8)) +
  scale_x_continuous(breaks = seq(2008,2020,4)) +
  
  
  facet_grid(season~ Parameter) +
  scale_color_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  scale_fill_manual(values = c("#4d4d4d", "#999999", "#e0e0e0")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major.y = element_line(color = "grey70"),
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

p2 <- 
  prediction_new |> 
  mutate(
    P_trend = case_when(
      p_trend <= 0.05 ~ "P < 0.05",
      p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
      p_trend > 0.1 ~ "P > 0.1"),
    Station = factor(Station, levels = c("BY31", "BY15", "BY5"))
  )  |> 
  ggplot(mapping = aes(x = Station, y = slope, fill = P_trend, col = P_trend, shape = Parameter))+
  geom_hline(yintercept = 0) +
  geom_point(col = "black", size = 4,
             position = position_dodge2(width = .3)) +
  facet_grid(season~1)+
  scale_shape_manual(values = c(25,24)) +
  scale_y_continuous(breaks = seq(0,.20,.1), limits = c(-0.0002,.22), expand = c(0,0))+
  scale_fill_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +    
  scale_color_manual(values = c("#b2182b", "#ef8a62", "#fddbc7"), "") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey70"),)
#scale_fill_gradient2(low = "#67a9cf", mid = "#f7f7f7", high = "#CD4620")+
cowplot::plot_grid(p1, p2, align = "hv", rel_widths = c(2,1.2))

ggsave("Output/Figures/Fig2.pdf", height = 6.5, width =  9)
rm(list=ls())
sessionInfo()