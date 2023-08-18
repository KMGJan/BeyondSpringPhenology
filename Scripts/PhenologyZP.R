################################################################################
#                                                                 18.08.2022   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                   Zooplankton Phenology metrics                              #
################################################################################
rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(roxygen2)
library(shaRk)
library(zoo)
library(xts)
library(reshape)
################################################################################
#1. Import the files
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
  gather(Group, Weigth, c(5, 6, 7, 8))

Bodymass <- Bodymass |> 
  mutate(Location = ifelse(Station == "BY31",
                           "N",
                           "S")) |> 
  dplyr::select(-Station)

# Abundance data of zooplankton
zooplankton <- readRDS("Data/zooplankton_02Aug23.rds")
################################################################################
# 2. Filter and arrange the dataset zooplankton
zp <- zooplankton |> 
  # Arrange the data taxonomy as the bodymass data
  mutate(dev_stage_code = ifelse(dev_stage_code == "AD",
                                 "AD",
                                 ifelse(dev_stage_code == "C1",
                                        "C1",
                                        ifelse(dev_stage_code %in% c("C3",
                                                                     "C4"),
                                               "C4",
                                               as.character(dev_stage_code)
                                               )))) |> 
  # Filter years between 2007 and 2021
  dplyr::filter(Year > 2006, 
         Year < 2023,
         # at depth of 30 and 60 m
         Depth %in% c(30, 60),
         # corresponding to abundance data
         unit == "ind/m3",
         # Excluding Nauplii
         dev_stage_code != "NP",
         # from these classes
         Class %in% c("Maxillopoda", # <----- Copepoda
                      "Branchiopoda",# <----- Cladocera
                      "Eurotatoria"), # <----- Rotifera
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
         dev_stage_code, 
         sex_code) |> 
  # Arrange a new column that matches bodymass.csv
  mutate(Group = case_when(
    Month %in% c(1, 2, 3) ~ "Jan.Mar",
    Month %in% c(4, 5, 6) ~ "Apr.Jun", 
    Month %in% c(7, 8, 9) ~ "Jul.Sep",
    Month %in% c(10, 11, 12) ~ "Oct.Dec"))

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
  transform(Genus = ifelse(Genus == "",  as.character(Class),  as.character(Genus)),
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
  mutate(Taxa = ifelse(Genus == "Acartia", 
                       "Acartia", 
                       ifelse(Genus == "Podon",
                              "Podon", 
                              as.character(Taxa))))
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
# 3. Remove Pseudocalanus from the surface as they are not abundant
zoo_table |>
  dplyr::filter(Taxa == "Pseudocalanus") |>
  ggplot(mapping = aes(x = SDATE, 
                       y = Value, 
                       col = as.character(Depth))) +
  geom_point() +
  facet_grid(Station ~ Taxa + Depth,
             scales = "free")
# We can see that Pseudocalanus has more record between 30 and 60m depth
# So let's keep Pseudocalanus that are between 30 and 60 m
D30 <- subset(zoo_table, Depth == 30 & Taxa !='Pseudocalanus')
D60 <- subset(zoo_table, Depth == 60 & Taxa =='Pseudocalanus')
ztable1 <- rbind(D30, D60)
rm(D30, D60, zoo_table)
################################################################################
# 4. Keep only the Taxa with high sampling frequencies
ztable2 <- ztable1 |>
  # We can assign the group copepoda, cladocera, and rotatoria to the genus
  mutate(Group = ifelse(Genus %in% c("Acartia",
                                     "Centropages",
                                     "Eurytemora",
                                     "Pseudocalanus",
                                     "Temora"),
                        "Copepoda",
                        ifelse(Genus %in% c("Bosmina",
                                            "Evadne",
                                            "Podon"),
                               "Cladocera",
                               "Rotatoria")))

# We can keep everything at BY31
BY31 <- ztable2 |> 
  dplyr::filter(Station == "BY31",
                Taxa != "Keratella", Taxa != "Eurytemora", Taxa != "Podon")
# But low frequency for Eurytemora, Keratella and Podon at BY15 and BY5
BY5 <- ztable2 |> 
  dplyr::filter(Station %in% c("BY5", "BY15"),
         Taxa != "Eurytemora",
         Taxa != "Keratella",
         Taxa != "Podon",
         Taxa != "Evadne",
         Taxa != "Bosmina")

ztable2 <- rbind(BY31, BY5)
rm(BY31, BY5)

ztable2 |>
  ggplot(aes(x=SDATE, y= log(Value)))+
  geom_point()+geom_line()+
  facet_grid(Station~Taxa, scales="free")
rm(ztable1)
# Save the data to plot sup. fig. 1
ztable2 |> 
  group_by(Year,
           Month,
           SDATE,
           Station) |>
  summarise(Value = sum(Value))|>
  group_by(Year,
           Month,
           Station)|>
  summarise(n = n()) |>
  mutate(Station = factor(Station,
                          levels = c("BY5",
                                     "BY15",
                                     "BY31"))) |>
  write.table("Output/Data/sup_fig1a.txt",
              sep = ";")
################################################################################
# 5. Data check
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
dGenus <- d1 |>
  group_by(Month, 
           Group,
           Genus, 
           Station,
           Taxa) |>
  summarise(Value = mean(Value,
                         na.rm = T))

dGenus |>
  ggplot(mapping = aes(x = Month, 
                       y = Value,
                       col = Taxa)) +
  geom_point() +
  geom_line() +
  facet_station +
  scale_x_month +
  theme_zp

# Same but per Group
dGroup <- dGenus |>
  group_by(Month, Group, Station)|>
  summarise(Value = sum(Value))
ggplot(dGroup, aes(x=Month, y=Value, col=Group))+
  geom_point()+
  geom_line()+
  scale_x_month+
  theme_zp +
  facet_station
################################################################################
# 6. Daily interpolation over the full time serie
dailyGenus <- ztable2 |>
  # Sum per DATE per Taxa
  group_by(SDATE,
           Station,
           Taxa) |>
  summarise(Value = sum(Value))
# Loop the interpolation per station

df = data.frame()
for (i in c("BY31", "BY5", "BY15")) {
  
  df_1 <- dailyGenus |> 
    dplyr::filter(Station == i,
                  year(SDATE) %in% 2008:2022)|>
    
    group_by(SDATE,Taxa)|>
    summarise(Value=mean(Value, na.rm=T))|>
    
    dailyInterpretation(taxa="Taxa")|>
    mutate(Station=i)
  
  df<- rbind(df_1, df)
  
  rm(df_1)
}
scale_x_DOY <- scale_x_month <- scale_x_continuous('Month',
                                  breaks = seq(0, 365, 30.5),
                                  limits = c(0, 366),
                                  labels = month.abb,
                                  expand = expand_scale(mult = c(0, 0), 
                                                        add = c(0, 0))) 


df |>
  ggplot(mapping = aes(x = DOY,
                       y = Abundance,
                       col = Taxa)) +
  geom_line() +
  theme_zp +
  scale_x_DOY +
  facet_station
################################################################################
# 7. Smoothing the values with the density and plotting the dynamics

CopInterp <- df |> 
  mutate(Taxa = factor(Taxa, 
                                    levels = c("Temora",
                                               "Acartia", 
                                               "Centropages",
                                               "Eurytemora", 
                                               "Pseudocalanus",
                                               
                                               "Bosmina",
                                               "Podon",
                                               "Evadne",
                                               "Keratella",
                                               "Synchaeta")),
                      Select= ifelse(Taxa %in% c(
                        "Acartia",
                        "Centropages",
                        "Pseudocalanus",
                        "Temora",
                        "Synchaeta"
                      ), "Yes", ifelse(Taxa %in% c("Evadne", "Bosmina") & Station == "BY31", "Yes", "No"))) |>
  dplyr::filter(Select %in% c("Yes")) |> ungroup() |>
  spread(Taxa, Abundance) |> 
  group_by(DOY, Station) |> 
  mutate(Total = sum(Temora, Acartia, Centropages, Pseudocalanus, Bosmina, Evadne, Synchaeta, na.rm = T)) |> ungroup() |>
  pivot_longer(4:11,
               names_to = "Taxa",
               values_to = "Abundance") |> na.omit()
col_order = c("DOY", "Taxa", "Abundance", "Station", "Select")
CopInterp <- CopInterp[, col_order]
Density = data.frame()
for( i in c("BY31", "BY5", "BY15")){
  Interp <- CopInterp |>
    dplyr::filter(Station == i)
  
  Density <- Interp[rep(row.names(Interp), Interp$Abundance), 1:2] |>
    mutate(Station = i) |>
    rbind(Density)
  
  rm(Interp)  
}

Density <- Density |>
  mutate(Group = ifelse(Taxa %in% c("Acartia", 
                                    "Temora",
                                    "Centropages",
                                    "Eurytemora",
                                    "Acartia",
                                    "Pseudocalanus"),
                        "Copepoda",
                        ifelse(Taxa %in%c("Bosmina",
                                          "Evadne",
                                          "Podon"),
                               "Cladocera",
                               "Rotatoria")))
Density |> write.table("Output/Data/Density_zp.txt", sep = ";")
Density |>
  ggplot(mapping = aes(x = DOY, y = after_stat(count))) +
  geom_density(alpha = 0.5,
               size = 1.1,
               fill = "black") +
  
  scale_y_continuous(expression(paste('Biomass'~('ug'~L^{-1}))),
                     expand = expand_scale(mult = c(0, 0.1), 
                                           add = c(0, 0))) +
  facet_grid(Taxa ~ Station, scales = 'free_y') +
  
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 9),
        strip.text.y = element_text(size = 10, color = "black", face = "italic"),
        legend.title = element_text(colour = "black", face = "plain"),
        legend.text = element_text(colour = "black", face = "italic"),
        panel.spacing = unit(0, "points")) +
  
  scale_x_month +
  theme_zp




# ---- 7. Interannual variation --------

d1 <- ztable2 |>
  group_by(Year, 
           Month,
           SDATE,
           Depth,
           Station,
           Taxa) |>
  summarise(Value = sum(Value)) |>
  
  group_by(Year,
           Month,
           Station,
           Taxa) |>
  summarise(Value = mean(Value,
                         na.rm = T))
av <- d1 |>
  group_by(Month,
           Station,
           Taxa) |>
  summarise(Value = mean(Value,
                         na.rm = T))
  
d1 |> 
  ggplot(mapping = aes(x = Month, 
                       y = Value)) +
  geom_point(mapping = aes(colour = factor(Year))) +
  scale_x_continuous('Month',
                     breaks = seq(1, 12, 1),
                     limits = c(1, 12)) +
  scale_y_continuous(expression(paste('ug'~L^{-1})),
                     breaks = seq(0, 200, 50), 
                     limits = c(0, 200)) + 
  labs(title = NULL)+
  facet_grid(Taxa ~ Station, scales = "free") + 
  geom_line(data = av,
            aes(x = Month,
                y = Value),
            size = 1.5)+
  theme_zp
rm(d1, av)


# ------- 8. Daily Interpolation by year --------

library(timetk)
d1 <- 
  ztable2 |>
  
  group_by(SDATE,
           Station,
           Taxa) |>
  summarise_by_time(.by = "week",.week_start = min(as.Date(ztable2$SDATE)), .date_var = SDATE,
                    Value = sum(Value))|> ungroup() |>
  pivot_wider(names_from = Taxa, values_from = Value) |>
  mutate(Acartia = ifelse(is.na(Acartia), 0, as.numeric(Acartia)),
         Centropages = ifelse(is.na(Centropages), 0, as.numeric(Centropages)),
         Pseudocalanus = ifelse(is.na(Pseudocalanus), 0, as.numeric(Pseudocalanus)),
         Synchaeta = ifelse(is.na(Synchaeta), 0, as.numeric(Synchaeta)),
         Temora = ifelse(is.na(Temora), 0, as.numeric(Temora)),
         Evadne = ifelse(is.na(Evadne), 0, as.numeric(Evadne)),
         Bosmina = ifelse(is.na(Bosmina), 0, as.numeric(Bosmina)),) |> # NA are replaced by 0 for the next operation (No 0 value was present in the dataset)
  mutate(Copepoda = Acartia + Centropages + Pseudocalanus + Temora,
         Cladocera = Evadne + Bosmina) |> 
  pivot_longer(3:11, names_to = "Taxa", values_to = "Value") |> 
  mutate(Value = ifelse(Value == 0, NA, as.numeric(Value))) |> # Change back 0 to NA
  
  as_tibble()

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
################################################################################
# 9. Bloom timing Center of gravity
SumAb <- df_interpolated |>
  group_by(Year, Taxa, Station) |>
  summarise(Abundance = sum(Abundance))

SumWeekAb <- df_interpolated |>
  group_by(Year, Taxa, Station) |>
  summarise(WeekAb = sum(WeekAb))

d8 <- merge(SumAb,
            SumWeekAb)

peak <- d8 |>
  mutate(Timing = round(WeekAb/Abundance),
         Group = ifelse(Taxa %in% c("Temora",
                                    "Centropages", 
                                    "Eurytemora", 
                                    "Acartia", 
                                    "Pseudocalanus",
                                    "Copepoda"),
                        "Copepoda",
                        ifelse(Taxa %in% c("Bosmina",
                                           "Evadne", 
                                           "Podon",
                                           "Cladocera"), 
                               "Cladocera",
                               "Rotatoria"))) |>
  dplyr::select(Year,
         Taxa,
         Station,
         Timing,
         Group)
# if you want to save this dataset, run the 3 lines below
#peak |>
#  write.table("./Output/ZP/Data/Peak.txt", 
#              sep = ";")
rm(d8, SumAb, SumWeekAb)
################################################################################
# 10. Bloom Duration
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
  ungroup()|>
  
  ggplot(aes(x = Week, y = Quart, col = as.character(Year))) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y = Avg), col = "black") +
  facet_grid(Taxa ~ Station) +
  theme_zp +
  geom_hline(mapping = aes(yintercept = 25), lty = 2, col = "grey50") +
  geom_hline(mapping = aes(yintercept = 75), lty = 2, col = "grey50") +
  theme(legend.position = "none")

# 10.a. Interpolation of the quartile
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
      
      rm(d2, d3, d4)
    }
  }
}
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
  transform(Dur = D75 - D25)|>
  mutate(Group= ifelse(Taxa %in% c("Temora",
                                   "Centropages", 
                                   "Eurytemora",
                                   "Pseudocalanus",
                                   "Acartia", "Copepoda"),
                       "Copepoda",
                       ifelse(Taxa %in% c("Bosmina",
                                          "Evadne",
                                          "Podon", "Cladocera"), 
                              "Cladocera",
                              "Rotatoria")))
rm(day75, day50, day25, d10)

# if you want to save this dataset, run the 4 lines below
#duration |>
#  na.omit() |>
#  write.table("./Output/ZP/Data/Duration.txt",
#              sep = ";")
rm(allQuart, CsumYr, d9)
################################################################################
# 11. Magnitude
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
# if you want to save this dataset, run the 4 lines below
#magnitude |>
#  as.data.frame() |>
#  write.table("./Output/ZP/Data/Magnitude.txt",
#              sep = ";")
################################################################################
# 12. Merge all phenoloy variables
ZP <- peak |>
  merge(magnitude,
        by = c("Station", "Taxa", "Year")) |>
  merge(duration,
        by = c("Taxa", "Year", "Station", "Group"))
names(ZP) <- c("Taxa",
               "Year",
               "Station",
               "Group",
               "Timing",
               "Maximum",
               "Magnitude",
               "Start",
               "End",
               "Middle",
               "Duration")
unique(ZP$Taxa)

ZP |> 
  write.table("Output/Data/Weekly_Full_ZP.txt",
              sep = ";")
################################################################################
# 13. Empty the environment
rm(list=ls())
sessionInfo()
