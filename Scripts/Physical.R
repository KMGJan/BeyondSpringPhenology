rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                           Abiotic Parameters                                 #
################################################################################
# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(roxygen2)
library(zoo)
library(xts)
library(reshape)
library(shaRk)
################################################################################
# Before we start create a folder in the Output folder
# to save the phytoplankton outputs

# Import the data
physical <- readRDS("Data/abiotic_02Aug23.rds")
################################################################################
physical |> 
  filter(station_name %in% c("BY5 BORNHOLMSDJ",
                             "BY31 LANDSORTSDJ",
                             "BY15 GOTLANDSDJ"),
         parameter == "Nitrite+Nitrate NO2+NO3-N") |> 
  mutate(SDATE = as.Date(visit_date,
                         format = "%Y-%m-%d"),
         day = format(SDATE, "%d"),
         month = format(SDATE, "%m"),
         year = format(SDATE, "%Y"),
         Depth = as.numeric(sample_depth_m),
         Station = ifelse(station_name == "BY31 LANDSORTSDJ",
                          "BY31", 
                          ifelse(station_name == "BY15 GOTLANDSDJ",
                                 "BY15",
                                 "BY5")),
         Parameter = "Nitrogen") |> 
  filter(year > 2007, year <= 2022,
         Depth %in% c(0,
                      5,
                      10,
                      15,
                      20)) |> 
  group_by(year,
           month,
           SDATE,
           Depth,
           Parameter,
           Station) |> 
  summarise(Value = mean(as.numeric(value),
                         na.rm=T)) |> 
  ungroup() |> 
  group_by(year,
           month,
           SDATE,
           Parameter,
           Station) |>
  summarise(Value = mean(Value,
                         na.rm=T)) |> 
  ungroup() |>
  mutate(Station = factor(Station, levels = c("BY31", "BY15", "BY5"))) |> 
  
  ggplot(aes( x= month, y = Value, fill = Station)) +
  geom_boxplot() +
  
  scale_fill_manual(values = c("#54278f",    
                                         "#9e9ac8",
                                         "#dadaeb")) +
  theme_classic() +

  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black",
                                   face = "plain",
                                   size = 12),
        legend.position = "right",
        panel.spacing = unit(0, "points"),
        strip.background = element_rect(colour = "transparent"),
        strip.text = element_text(colour = "black",
                                  size = 15),
        axis.text = element_text(colour = "black",
                                 size = 15),
        axis.text.x = element_text(hjust = 0),
        axis.title = element_text(colour = "black",
                                  size = 15),
        panel.background = element_rect(colour = "transparent",
                                        fill = "transparent"),
        panel.border = element_blank())

ggsave("Output/Figures/FigS5.pdf",
       height = 3,
       width = 8)
################################################################################
physical_TS <- physical |>
  
  filter(station_name %in% c("BY5 BORNHOLMSDJ",
                             "BY31 LANDSORTSDJ",
                             "BY15 GOTLANDSDJ"),
         parameter %in% c("Salinity CTD",
                          "Temperature CTD",
                          "Dissolved oxygen O2 bottle",
                          "Dissolved oxygen O2 CTD")) |>
  mutate(SDATE = as.Date(visit_date,
                         format = "%Y-%m-%d"),
         day = format(SDATE, "%d"),
         month = format(SDATE, "%m"),
         year = format(SDATE, "%Y"),
         Depth = as.numeric(sample_depth_m),
         Station = ifelse(station_name == "BY31 LANDSORTSDJ",
                          "BY31", 
                          ifelse(station_name == "BY15 GOTLANDSDJ",
                                 "BY15",
                                 "BY5")),
         Parameter = parameter) |>
  filter(year >= 2007, year <=2022)
################################################################################
# Sea surface temperature:
SST <- physical_TS |>
  filter(Parameter == "Temperature CTD",
         Depth == 0) |> 
  mutate(Parameter = "SST")
# Temperature over the euphotic zone
Temp_020 <- physical_TS |>
  filter(Parameter == "Temperature CTD",
         Depth %in% c(0,
                      5,
                      10,
                      15,
                      20)) |> 
  mutate(Parameter = "Temp_0-20")


Temp <- rbind(SST,
              Temp_020)
rm(SST, Temp_020)
# Salinity
Salinity_020 <-physical_TS |>
  filter(Parameter == "Salinity CTD",
         Depth %in% seq(0,
                        20,
                        5)) |>
  mutate(Parameter = "Salinity")
Salinity_SSS <-physical_TS |>
  filter(Parameter == "Salinity CTD",
         Depth %in% c(0)) |>
  mutate(Parameter = "Salinity_SSS")
Salinity_Bottom_BY31 <- physical_TS |>
  filter(Parameter == "Salinity CTD",
         Station == "BY31",
         Depth == 60)|># %in% seq(430, 440, 1)) |>
  mutate(Parameter = "Salinity_bottom")
Salinity_Bottom_BY15 <- physical_TS |>
  filter(Parameter == "Salinity CTD",
         Station == "BY15",
         Depth == 60)|>#Depth %in% seq(230,240,1)) |>
  mutate(Parameter = "Salinity_bottom")

Salinity_Bottom_BY5 <- physical_TS |>
  filter(Parameter == "Salinity CTD",
         Station == "BY5",
         Depth == 60)|>#Depth %in% seq(70,80,1)) |>
  mutate(Parameter = "Salinity_bottom")

# combine all the salinity data:
Salinity <- rbind(Salinity_020,
                  Salinity_SSS,
                  Salinity_Bottom_BY31,
                  Salinity_Bottom_BY15,
                  Salinity_Bottom_BY5)
rm(Salinity_020, Salinity_SSS)
Oxy_020 <- physical_TS |>
  filter(Parameter %in% c("Dissolved oxygen O2 CTD",
                          "Dissolved oxygen O2 bottle"),
         Depth %in% c(0,
                      5,
                      10,
                      15,
                      20)) |> 
  mutate(Parameter = "Oxy_0-20")
# combine salinity and temperature data
Abiotic <- rbind(Temp, Salinity, Oxy_020)
rm(Temp, Salinity)
################################################################################
d1 <- Abiotic|> 
  group_by(year,
           month,
           SDATE,
           Depth,
           Parameter,
           Station)|>
  summarise(Value = mean(as.numeric(value),
                         na.rm=T)) |>
  group_by(year,
           month,
           SDATE,
           Parameter,
           Station) |>
  summarise(Value = mean(Value,
                         na.rm=T)) |>
  as.data.frame()

d1 |> 
  mutate(year = as.numeric(year)) |>
  filter(year %in% seq(2007, 2022, 1),
         Parameter == "Temp_0-20") |>
  group_by(year,
           month,
           SDATE,
           Station) |>
  summarise(Value = sum(Value))|>
  group_by(year,
           month,
           Station)|>
  summarise(n = n()) |>
  
  mutate(Station = factor(Station,
                          levels = c("BY5",
                                     "BY15",
                                     "BY31"))) |>
  write.table("Output/Data/sup_fig1c.txt",
              sep = ";")
################################################################################
daily <- d1 |> 
  mutate(Year = as.numeric(year)) |>
  mutate(FYEAR = ifelse(Year < 2008,
                        "before",
                        "now"))|> 
  group_by(SDATE,
           Parameter,
           month,
           Year,
           Station,
           FYEAR) |>
  summarise(Value = mean(Value,
                         na.rm = T))
daily |> filter(Parameter == "Salinity_bottom", Year >2007) |> 
  ggplot(aes(x = SDATE, y = Value))+
  geom_point()+
  facet_grid(.~Station)
df = data.frame()
for (i in unique(daily$Station)) {
  df_1 <- daily |> 
    filter(Station == i)
  for(f in unique(daily$FYEAR)){
    df2 <- df_1 |>
      filter(FYEAR == f) |>
      
      group_by(SDATE,
               Parameter) |>
      summarise(Value = mean(Value,
                             na.rm = T)) |> 
      
      dailyInterpretation(taxa = "Parameter") |>
      mutate(Station = i,
             FYEAR = f)
    
    df <- rbind(df2, df)
    
  }
}

################################################################################
library(timetk)

d1 <- daily|>
  
  group_by(SDATE,
           Year,
           Parameter,
           month,
           Station) |>
  summarise_by_time(.by = "week", .week_start = min(as.Date(daily$SDATE)), .date_var = SDATE,
                    Value  = mean(Value, na.rm = T)) |>
  as.data.frame()

allDates <- seq.Date(
  min(as.Date(d1$SDATE)),
  max(as.Date(d1$SDATE)),
  "week")

d6_end = data.frame()
for (i in unique(d1$Station)) {
  d2 <- d1 |>
    filter(Station == i) |>
    dplyr::select(SDATE,
                  Parameter,
                  Value) |>
    spread(Parameter, Value)
  # daily time series
  d3 <- merge(
    x = data.frame(SDATE = allDates),
    y = data.frame(d2),
    all = TRUE)
  # Interpolation
  d3zoo <- zoo(d3, d3[,1]) 
  
  d4 <- na.approx(d3zoo[,-1])
  d4 <- data.frame(cbind(d3zoo[,1], d4))
  colnames(d4)[1] <- "SDATE"
  d5 <- melt(d4, id = c('SDATE'), variable = 'Parameter', na.rm = T)
  d6 <- d5 |>
    mutate(Value = as.numeric(as.character(value)),
           DOY = as.numeric(strftime(SDATE,
                                     format = "%V")),
           Week = as.numeric(strftime(SDATE,
                                      format = "%V")),
           DOYAb = Value * DOY,
           WAb = Value * Week,
           Year = as.numeric(strftime(SDATE,
                                      format = "%Y")),
           Station = i)
  d6_end <- rbind(d6_end, d6)
  rm(d2, d3, d3zoo, d4, d5, d6)
}

min(d6_end$DOY)
d6_end <- d6_end |>
  filter(Year <= 2023,
         DOY != 1, DOY != 52, DOY != 53) |>
  mutate(SDATE = as.Date(SDATE,
                         format = c("%Y-%m-%d"))) |>
  dplyr::select(SDATE,
                Station,
                Value,
                Year,
                Parameter,
                DOY)
################################################################################
se <- function(x) {
  n <- length(x[!is.na(x)])
  if (n > 2) {
    out <- sd(x, na.rm = TRUE) / sqrt(n)
  } else {
    out <- NA
  }
  return(out)
}


#################
dPar <- d6_end |> 
  mutate(month = month(SDATE)) |>
  group_by(month, 
           Parameter,
           SDATE,
           Station,
           Year) |>
  summarise(Value = mean(Value,
                         na.rm = T)) |>
  group_by(Year,
           month,
           Parameter,
           Station) |>
  summarise(Value = mean(Value,
                         na.rm = T),
            n = n())

dPar <- dPar |>
  group_by(Year,
           month,
           Parameter,
           Station) |>
  summarise(Value = mean(Value,
                         na.rm = T),
            n = n()) |> 
  mutate(Value = ifelse(Parameter == "Thermocline",
                        -Value,
                        as.numeric(Value)))
dPar |>
  ggplot(mapping = aes(x = Year,
                       y = Value,
                       col = Station)) +
  geom_point() +
  facet_grid(Parameter ~ month,
             scales = "free")

try <- dPar |>
  group_by(Station,
           month,
           Parameter) |>
  mutate(Avg = mean(Value)) |>
  ungroup() |>
  group_by(Station, 
           month,
           Year,
           Parameter) |>
  mutate(Anomalie = Value - Avg) |>
  ungroup() |>
  group_by(Station,
           month,
           Parameter) |>
  mutate(cumSum = cumsum(Anomalie)) |>
  ungroup() |>
  filter(Station == "BY31")


df <- dPar |>
  filter(month %in% seq(1,
                        12,
                        1))
# ---- Save the dataset ----
dPar |>
  write.table("Output/Data/Abiotic.txt",
              sep = ";",
              dec = ".")

rm(list = ls())
sessionInfo()
