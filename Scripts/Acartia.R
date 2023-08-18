rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                                Acartia                                       #
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
#-------------------------------------------------------------------------------

# Import the files -------------------------------------------------------------
# Bodymass.csv has the bodymass of all zooplankton
# (This can be found in the Sup. Table 1)
Bodymass <- read.csv("Data/Bodymass.csv") |> 
  # The imported dataset was a "wide" dataframe and we want it as a long dataframe
  # To follow the script final structure should be:
  # Taxa: <chr> Temora, Pseudocalanus, Centropages, Acartia tonsa, Acartia, ...
  # STAGE: <chr> AD, C1, C4, JV, NS
  # SEXCO: <chr> F, M, NS
  # Station: <chr> BY31, BY15, BY5
  # Group: <chr> Jan.Mar, Apr.Jun, Jul.Sep, Oct.Dec
  # Weigth: <dbl> Bodymass from Sup. Table 1
  gather(Group, Weigth, c(5, 6, 7, 8))  |>
  mutate(Location = ifelse(Station == "BY31",
                           "N",
                           "S")) |>
  dplyr::select(-Station)

# Abundance data of zooplankton
zooplankton <- readRDS("Data/zooplankton_02Aug23.rds")

# Filter and arrange the dataset zooplankton -----------------------------------
zp <- zooplankton |>
  # Arrange the data taxonomy as the bodymass data
  mutate(dev_stage_code = ifelse(dev_stage_code == "AD", "AD", ifelse(dev_stage_code == "C1",  "C1", ifelse(dev_stage_code %in% c("C3",  "C4"), "C4", as.character(dev_stage_code))))) |>
  # Filter years between 2007 and 2021
  filter(Year %in% 2007:2022,
         # at depth of 30 m
         Depth == 30,
         # corresponding to abundance data
         unit == "ind/m3",
         # Excluding Nauplii
         dev_stage_code != "NP",
 
         # from these 3 stations
         Station %in% c("BY5 BORNHOLMSDJ",
                        "BY15 GOTLANDSDJ",
                        "BY31 LANDSORTSDJ"),
         #And only Acartia
         Genus == "Acartia") |> 
  # Select only columns of interest 
  dplyr::select(Genus,
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
                dev_stage_code, 
                sex_code) |>
  # Arrange a new column that matches bodymass.csv
  mutate(Group = case_when(
    Month %in% 1:3 ~ "Jan.Mar",
    Month %in% 4:6 ~ "Apr.Jun", 
    Month %in% 7:9 ~ "Jul.Sep",
    Month %in% 10:12 ~ "Oct.Dec"))

# Merge bodymass and zooplankton data
zoo_table <- zp |>
  mutate(STAGE = dev_stage_code,
         SEXCO = sex_code,
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
         Value = Value / 1000, # <---- from ind/m3 to ind/L
         SEXCO = ifelse(SEXCO == "M",
                        "M", 
                        ifelse(SEXCO == "F",
                               "F",
                               "NS")),
         STAGE = ifelse(STAGE %in% c("AD",
                                     "C1", 
                                     "C4",
                                     "JV"),
                        as.character(STAGE), 
                        "NS")) |>
  # Now that everything matches the bodymass dataframe, we can merge
  merge(Bodymass,
        by = c("Taxa", 
               "Location", 
               "STAGE",
               "SEXCO",
               "Group")) |>
  # Biomass = Abundance * Weigth
  mutate(Biomass.ugL = Value * Weigth) |>
  
  dplyr::select(Taxa,
                Station,
                Genus,
                SDATE,
                Yr_mon,
                Month,
                Year, 
                Day,
                Biomass.ugL, 
                Depth) |>
  filter(Taxa %in% c("Acartia bifilosa",
                     "Acartia longiremis",
                     "Acartia tonsa" ))

names(zoo_table) <- c("Taxa", 
                      "Station",
                      "Genus",
                      "SDATE",
                      "Yr_mon", 
                      "Month", 
                      "Year",
                      "Day",
                      "Value", 
                      "Depth")
rm(Bodymass, zooplankton, zp)

ggplot(data = zoo_table,
       mapping = aes(x = SDATE,
                     y = log(Value),
                     col = as.character(Depth))) +
  geom_point() +
  facet_grid(Station ~
               Taxa)

zoo_table |>
  ggplot(mapping = aes(x = SDATE,
                       y = log(Value))) +
  geom_point() +
  geom_line() +
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
d1 <- zoo_table |>
  group_by(Year,
           Yr_mon,
           Month,
           SDATE,
           Depth,
           Genus,
           Station,
           Taxa) |>
   # Sum of each sampling event
   summarise(Value = sum(Value, na.rm=T)) |>
   ungroup() |>
   # Monthly average
   group_by(Year,
            Yr_mon,
            Month,
            Genus,
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
 
# Weekly Interpolation by year -------------------------------------------------

library(timetk)
d1 <- zoo_table |>
  
  group_by(SDATE,
           Station,
           Taxa) |>
  summarise_by_time(.by = "week",.week_start = min(as.Date(zoo_table$SDATE)), .date_var = SDATE,
                    Value = sum(Value))|>
  as.data.frame()

allDates <- seq.Date(
  min(as.Date(d1$SDATE)),
  max(as.Date(d1$SDATE)),
  "week") 

d6_end = data.frame()

for (i in unique(d1$Station)) {
  d2 <- d1 |>
    dplyr::filter(Station == i) |>
    dplyr::select( - Station) |>
    spread(Taxa, Value)
  
  d2
  # daily time series
  d3 <- merge(
    x = data.frame(SDATE = allDates),
    y = data.frame(d2),
    all.x = TRUE)
  d3
  #if (i == "BY15") {d3 <- d3 |> slice(14:n())}
  # Interpolation
  # Interpolation
  d3zoo <- zoo(d3, d3[,1]) 
  d3zoo
  d4 <- na.approx(d3zoo[,-1])
  d4
  d4 <- data.frame(cbind(d3zoo[,1], d4))
  colnames(d4)[1] <- "SDATE"
  d5 <- melt(d4, id = c('SDATE'), variable = 'Taxa', na.rm = T)
  
  d6 <- d5 |>
    mutate(Abundance = as.numeric(as.character(value)),
           Week = as.numeric(strftime(SDATE, format = "%V")),
           #  Week = as.numeric(strftime(SDATE, format= "%V")),
           WeekAb = Abundance * Week,
           # WAb = Abundance * Week,
           Year = as.numeric(strftime(SDATE, format = "%Y")),
           Station = i)
  
  
  d6_end <- rbind(d6_end, d6)
  
  rm(d2, d3, d3zoo, d4, d5, d6)
}


df_interpolated <- d6_end |>
  filter(Year > 2007,
         Year < 2023,
         Week < 52, Week >1)

rm(d6_end, d1)

# Bloom timing Center of gravity -----------------------------------------------
SumAb <- df_interpolated |>
  group_by(Year, Taxa, Station) |>
  summarise(Abundance = sum(Abundance))
SumWeekAb <- df_interpolated |>
  group_by(Year, Taxa, Station) |>
  summarise(WeekAb = sum(WeekAb))

d8 <- merge(SumAb,
            SumWeekAb)

peak <- d8 |>
  mutate(Timing = WeekAb/Abundance) |>
  dplyr::select(Year,
                Taxa,
                Station,
                Timing)

rm(d8, SumAb, SumWeekAb)

# Bloom Duration ---------------------------------------------------------------
d9 <- df_interpolated |>
  group_by(Taxa, Year, Station) |>
  mutate(cumab = cumsum(Abundance))

CsumYr <- d9 |>
  group_by(Year, Taxa, Station) |>
  summarise(max.value = max(cumab))

d10 <- merge(CsumYr, d9, all = T) |>
  transform(Quart = round(cumab / max.value * 100,
                          digits = 0))
d10 |>
  group_by(Taxa, Week, Station) |>
  mutate(Avg = mean(Quart, na.rm = T)) |>
  ungroup() |>
  
  ggplot(mapping = aes(x = Week,
                       y = Quart,
                       col = as.character(Year))) +
  geom_line(alpha = 0.5) +
  geom_line(mapping = aes(y = Avg),
            col = "black") +
  facet_grid(Taxa ~ Station) +
  theme_zp +
  geom_hline(mapping = aes(yintercept = 25),
             lty = 2,
             col = "grey50") +
  geom_hline(mapping = aes(yintercept = 75),
             lty = 2,
             col = "grey50") +
  theme(legend.position = "none")
                          
# Interpolation of the quartile ------------------------------------------------
# because some do not have a value for the 25 and 75th quartile
allQuart <- data.frame(Quart = seq(0, 100, 1))

d6_end_2 = data.frame()
for (i in unique(d10$Station)) {
  for(y in unique(d10$Year)){
    for(t in unique(d10$Taxa)){
      d2 <- d10 |>
        filter(Station == i,
               Year == y,
               Taxa == t) |>
        dplyr::select(Week, Quart)
      
      d3 <- merge(
        allQuart,
        d2,
        by = "Quart",
        all.x = TRUE)
      # Interpolation
      d4 <- na.approx(d3)
      d4 <- data.frame(cbind(d4))
      colnames(d4)[1] <- "Quart_approx"
      d4$Station = i
      d4$Year = y
      d4$Taxa = t
      d6_end_2 <- rbind(d6_end_2, d4)
      
      rm(d2, d3, d4)}}}

d10 <- merge(d10, d6_end_2, 
             by = c("Station", "Year", "Taxa", "Week"),
             all.y = T)
                          
day25 <- d10[which(d10$Quart_approx == 25),] |>
  group_by(Year, Taxa, Station) |>
  summarise(D25 = mean(Week))

day75 <- d10[which(d10$Quart_approx == 75),] |>
  group_by(Year, Taxa, Station)|>
  summarise(D75 = mean(Week))
day50 <- d10[which(d10$Quart_approx == 50),] |>
  group_by(Year, Taxa, Station)|>
  summarise(middle = mean(Week))
duration <- merge(day25, day75, by = c("Taxa", "Year", "Station")) |>
  merge(day50, by=c("Taxa", "Year", "Station")) |>
  transform(Dur = D75 - D25)
rm(day75, day50, day25, d10)

rm(allQuart, CsumYr, d9)

# Magnitude --------------------------------------------------------------------
magnitude <- df_interpolated |>
  merge(duration,
        by = c("Taxa", "Year", "Station")) |>
  filter(Week >= D25) |>
  filter(Week <= D75) |>
  group_by(Station, Taxa, Year) |>
  summarise(Max = max(Abundance),
            Avg = mean(Abundance, 
                       na.rm=T))|>
  as.data.frame()

# Merge all phenoloy variables -------------------------------------------------
ZP <- peak |>
  merge(magnitude,
        by = c("Station", "Taxa", "Year")) |>
  merge(duration,
        by = c("Taxa", "Year", "Station"))

names(ZP) <- c("Taxa",
               "Year",
               "Station",
               "Timing",
               "Maximum",
               "Magnitude",
               "Start",
               "End",
               "Middle",
               "Duration")

# Some GLM ---------------------------------------------------------------------
ABIOTIC <- read.delim("Output/Data/Abiotic.txt",
                      sep=";") |> 
  mutate(Parameter = ifelse(Parameter == "Temp_0.20",
                            "T", 
                            ifelse(Parameter == "Salinity",
                                   "S",
                                   ifelse(Parameter == "Salinity_bottom", "BS", as.character(Parameter)))),
         season = case_when(
           month %in% 1:3 ~ "winter",
           month %in% 4:6 ~ "spring",
           month %in% 7:9 ~ "summer",
           month %in% 10:12 ~ "fall"),
         Parameter_2 = paste(Parameter, season, sep = "_")) |>
  
  as_tibble() |> 
  filter(Parameter %in% c("T",
                          "BS",
                          "S"),
         Year %in% 2008:2022) |>
  group_by(Year, Station, Parameter_2) |> 
  summarise(Value = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  
  group_by(Station, Parameter_2) |> 
  mutate(z = (Value - mean(Value)) / sd(Value)) |> 
  ungroup() |>
  dplyr::select(-Value) |> 
  spread(Parameter_2, z)

acartia_df <- ZP |> 
  dplyr::select(Taxa, Year, Station, Timing, Magnitude) |>
  mutate(Magnitude = log(Magnitude),
         Taxa = case_when(
           Taxa == "Acartia.bifilosa" ~ "A. bifilosa",
           Taxa == "Acartia.longiremis" ~ "A. longiremis",
           Taxa == "Acartia.tonsa" ~ "A. tonsa"
         ),
         Station = factor(Station, levels = c("BY5", "BY15", "BY31"))) |> 
  gather(Parameter, Value, 4:5) |>
  group_by(Taxa, Station, Parameter) |>
  mutate(avg = mean(Value),
         anomalie = Value - avg,
         sd = sd(Value),
         z = anomalie / sd) |> 
  dplyr::select(Year,
                Station,
                Taxa, 
                Parameter,
                z) |>
  merge(ABIOTIC)

final_df <- read.delim("Output/Data/Weekly_Full_PP.txt", 
                     sep=";") |> 
  dplyr::select(Taxa, Year, Station, Timing, Magnitude) |>
  as.data.frame() |> 
  filter(Taxa == "Spring_bloom" | Taxa == "Cyanobacteria" | Taxa == "Summer_bloom")|>
  mutate(Magnitude = log(Magnitude)) |>
  gather(Parameter, Value, 4:5) |>
  mutate(Parameter = paste(Taxa, Parameter, sep = "_"))|>
  group_by(Station, Parameter)|>
  mutate(avg = mean(Value),
         anomalie = Value - avg,
         sd = sd(Value),
         z = anomalie / sd) |> 
  dplyr::select(Year,
                Station,
                Parameter,
                z) |>
  filter(Parameter %in% c("Cyanobacteria_Magnitude", "Spring_bloom_Magnitude", "Spring_bloom_Timing", "Summer_bloom_Timing", "Summer_bloom_Magnitude"))|>
  spread(Parameter, z) |> 
  merge(acartia_df, by = c("Year", "Station")) |>
  spread(Parameter, z) |>
  mutate(Mismatch = Timing - Spring_bloom_Timing)

# Here it comes ----------------------------------------------------------------  
NAMES <- c("Factor", "Estimate", "Error", "z.value", "p.value", "R2")
output1 = output2 = df1 = df2 = data.frame()
for(i in unique(final_df$Taxa)){
  df <- final_df |> 
    filter(Taxa == i)
  
  # Peak timing
  mod1 <- glm(Timing ~
               T_spring + 
               Spring_bloom_Timing + Summer_bloom_Timing,#Spring_bloom_Magnitude,
             df,
             family = "gaussian")
  
    
  par(mfrow = c(2, 2))
  print(plot(mod1,
             main = paste("Timing",
                          i,
                          sep = " ")))
  
  SUM1 <- summary(mod1)
  print(paste("Timing", i, sep = " "))
  print(SUM1)
  df1 <- SUM1$coefficients |> 
    as.data.frame() |> 
    tibble::rownames_to_column("Factor")|> 
    mutate(R2 =   with(summary(mod1), 1-deviance/null.deviance))
  names(df1) <- NAMES
  
  output1 <- df1 |> 
    mutate(GLM = "Timing") |>
    mutate(Taxa = i) |>
    filter(Factor != "(Intercept)") |>
    rbind(output1) |>
    mutate(GLM = "Timing")
  
  # Peak magnitude
  mod2 <- glm((Magnitude) ~
               Mismatch +
               Summer_bloom_Magnitude +
               T_spring + S_spring,
             df,
             family = "gaussian")
  par(mfrow = c(2, 2))
  print(plot(mod2,
             main = paste("Magnitude",
                          i,
                          sep = " ")))
  
  SUM2 <- summary(mod2)
  
  print(paste("Magnitude", i, sep = " "))
  print(SUM2)
  df2 <- SUM2$coefficients |> 
    as.data.frame() |> 
    tibble::rownames_to_column("Factor")|> 
    mutate(R2 =   with(summary(mod2), 1-deviance/null.deviance))
  names(df2) <- NAMES
  
  output2 <- df2 |>     mutate(GLM = "Magnitude") |>
    mutate(Taxa = i) |>
    filter(Factor != "(Intercept)") |>
    rbind(output2) |>
    mutate(GLM = "Magnitude")
}
plotAcartia <- output1 |>
  rbind(output2) |> 

  mutate(P_VALUE = case_when(
    p.value <= 0.05 ~ "p < 0.05",
    p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
    p.value > 0.1 ~ "p > 0.1"),
    Factor = case_when(
      Factor == "Cyanobacteria_Magnitude" ~ "CM",
      Factor == "Spring_bloom_Magnitude" ~ "SpBM",
      Factor == "Spring_bloom_Timing" ~ "SpBT",
      Factor == "Summer_bloom_Magnitude" ~ "SuBM",
      Factor == "Summer_bloom_Timing" ~ "SuBT",
      Factor %in% c("S_spring", "BS_spring") ~ "Salinity",
      
      Factor %in% c("T_spring") ~ "T°C",
      Factor == "Mismatch" ~ "TMI"),
    GLM = factor(GLM, levels = c("Timing", "Magnitude"))) |>
  mutate(P_VALUE= factor(P_VALUE, 
                         levels = c("ns",
                                    "p < 0.05",
                                    "p < 0.1",
                                    "p > 0.1")),

         Factor = factor(Factor, 
                         levels = c("Salinity",
                                    "T°C",
                                    "SpBT","SuBT",
                                    "SpBM","SuBM",
                                    "TMI",
                                    "CM")))

  
# Plot -------------------------------------------------------------------------
plotAcartia |> 
  ggplot(mapping = aes(x = Factor,
                     y = Estimate,
                     ymin = Estimate - Error,
                     ymax = Estimate + Error, 
                     col = P_VALUE,
                     fill = P_VALUE)) +
  geom_hline(yintercept = 0) +
  geom_linerange(size = 1,
                 col = 1) +
  geom_point(size = 4,
             shape = 21,
             stroke = 1,
             col = 1) +
  scale_fill_manual(values = c("#b2182b",
                               "#ef8a62",
                               "#fddbc7"),
                    "") +
  
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * 1, breaks = seq(-1,1,.25)), breaks = seq(-1,1,.25)) +
  # coord_flip() +
  facet_grid(Taxa ~ GLM,
             scales = "free", space = "free_x") +
  theme_classic() +
  theme(panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        panel.grid.major = element_line(color = "grey70"),
        axis.text.y = element_text(color = "black",
                                   size = 13),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.text = element_text(color = "black", 
                                   face = "italic",
                                   size = 13),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black",
                                    size = 13,
                                    face = "italic"),
        axis.text = element_text(color = "black",
                                 size = 13),
        axis.title = element_text(color = "black",
                                  size = 13)) +
  labs(y = "Standardized coefficients",
       x = NULL)
print(plotAcartia)
ggsave("Output/Figures/FigS4.pdf",
       height = 6,
       width = 6)
rm(list=ls())
sessionInfo()
