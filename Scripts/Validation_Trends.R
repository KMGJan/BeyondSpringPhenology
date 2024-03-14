#!/usr/bin/env Rscript
rm(list=ls())
################################################################################
#                                                                 26.01.2024   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
# Subsampling phytoplankton and zooplankton at BY31 followed by trend analysis #
#                                                                              #
################################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(zoo)
library(reshape)
################################################################################
# Read the phytoplankton data before the weekly interpolation
pp <- readRDS("Output/Data/Phytoplankton_before_interpolation.rds")
# Add a new column for the week
df <- pp %>%
  mutate(Week = week(SDATE),
         Year = year(SDATE),
         Y_W = paste(Year, Week, sep ="_"))
# Choose a reference entry (in this case, we want to have sampling date that are similar to the ones at BY15)
reference_entries <- df %>%
  filter(Station == "BY15") |> unique()

# Define a window of one week around the week of the reference entry
window_size <- 1

# Select entries close to the week of the reference entry
week_minus1<-paste(reference_entries$Year, reference_entries$Week - window_size, sep="_") |> unique()
week <-paste(reference_entries$Year, reference_entries$Week, sep="_") |> unique()

# Subsampling rule:
# in April, the sampling week is equal to the the week prior
# to the sampling date at BY15
# during the other months, the sampling can occur 
# the week or the week prior to the ones at BY15
selected_entries <- df %>%
  filter(Y_W %in% c(week, week_minus1)) 

d1 <- selected_entries |> 
  filter(month(SDATE) == 4,
         Y_W %in% c(week_minus1))|> 
  rbind(selected_entries |> filter(month(SDATE)!=4)) |> 
  dplyr::select(-c(Week, Year, Y_W)) |> 
  filter(Station == "BY31")

# Save the dates in a dataframe for the zooplankton
selected_entries |> 
  filter(month(SDATE) == 4,
         Y_W %in% c(week_minus1))|> 
  rbind(selected_entries |> filter(month(SDATE)!=4)) |> 
  write_rds("Output/Data/Week_Year_selected.rds")


d1 |> 
  dplyr::select(Station, SDATE) |>
  unique() |> 
  mutate(Year = year(SDATE), Month = month(SDATE)) |> 
  group_by(Year, Month, Station) |> 
  summarise(N = n()) %T>%
  
  #Save the data for the panel b
  write_rds("Output/Data/Sup_subsampled_pp.rds") %>%
  
  ggplot(aes(x=as.factor(Year), y = as.factor(Month), fill = as.factor(N)))+
  geom_tile() +
  facet_grid(Station~.)

################################################################################
# Following PhenologyPP Line 147
################################################################################
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
  
  
  # daily time series
  d3 <- merge(
    x = data.frame(SDATE = allDates),
    y = data.frame(d2),
    all.x = TRUE)
  
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

d6_end <- d6_end |>
  dplyr::filter(Year > 2007,
                Year < 2023,
                Week < 52, Week >1)
unique(d6_end$Taxa)
################################################################################
# 8. And save the daily interpolated data
dseason <- d6_end |>
  filter(Year > 2007) |>
  mutate(Season = ifelse(Week >= 5 & Week <= 22,
                         "Spring bloom",
                         ifelse(Week > 22 & Week < 37,
                                "Summer bloom",
                                "Other")))

dseason1 <- dseason |>
  filter(Season != "Other") |>
  mutate(select = ifelse(Taxa == "Total",
                         "yes",
                         "no")) |>
  filter(select == "yes") |>
  mutate(Taxa = ifelse(Season == "Spring bloom",
                       "Spring_bloom",
                       "Summer_bloom"))

df_interpolated <- d6_end |>
  filter(Year > 2007) |>
  filter(Taxa != "Total") |>
  mutate(Taxa = ifelse(Taxa == "Diatoms" & Week >= 5 & Week <= 22,
                       "Spring_Diatoms",
                       ifelse(Taxa == "Diatoms" & Week >= 37,
                              "Summer_Diatoms",
                              ifelse(Taxa == "Dinoflagellates" & Week >= 5 & Week <= 22,
                                     "Dinoflagellates",
                                     ifelse(Taxa == "Dinoflagellates" & Week > 22,
                                            "Not_included_Dinoflagellates",
                                            as.character(Taxa))))),
         Season = "NA",
         select = "yes") |> 
  filter(Taxa != "Diatoms",
         Taxa != "Not_included_Dinoflagellates") |>
  rbind(dseason1)

################################################################################
# 9. Peak Timing by center of gravity
SumAb <- df_interpolated |>
  group_by(Year, Taxa, Station, Season)|>
  summarise(Abundance = sum(Abundance))
SumWeekAb <- df_interpolated |>
  group_by(Year, Taxa, Station, Season)|>
  summarise(WeekAb = sum(WeekAb))

peak <- merge(SumAb,SumWeekAb) |>
  mutate(Timing = round(WeekAb / Abundance)) |> 
  dplyr::select( - c(Abundance, WeekAb))
# Check the data
peak |>
  ggplot(mapping = aes(y = Timing,
                       x = Taxa,
                       fill = Station)) +
  geom_boxplot(position = position_dodge(1),
               alpha = 0.1) +
  geom_point(mapping = aes(group = Station),
             color = "black",
             shape = 21,
             size = 1, 
             position = position_jitterdodge(jitter.width = .1,
                                             dodge.width = 1)) +
  theme_bw() +
  coord_flip()+
  labs(title = "Seasonal peak",
       x = "Taxa")
################################################################################
# 10. Bloom Duration
d9 <- df_interpolated |>
  filter(Taxa != "Diatoms") |>
  #filter(Season == "Growing_season") |>
  group_by(Taxa, Year, Station) |>
  mutate(cumab = cumsum(Abundance))

CsumYr <- d9 |>
  group_by(Year, Taxa, Station) |>
  summarise(max.value = max(cumab))

d10 <- merge(CsumYr, d9, all = T) |>
  transform(Quart = round(cumab / max.value * 100, digits = 0))
rm(d9, CsumYr)

d10 |>
  group_by(Taxa, Week, Station) |>
  mutate(Avg = mean(Quart,
                    na.rm = T)) |>
  ungroup()|>
  
  ggplot(mapping = aes(x = Week,
                       y = Quart,
                       col = as.character(Year))) +
  geom_line(alpha = 0.5) +
  geom_line(mapping = aes(y = Avg),
            col = "black") +
  facet_grid(Taxa ~ Station) +

  geom_hline(mapping = aes(yintercept = 25),
             lty = 2,
             col = "grey50") +
  geom_hline(mapping = aes(yintercept = 75),
             lty = 2,
             col = "grey50") +
  theme(legend.position = "none")
###########################
# Interpolation of the quartile
# Beacuse some do not have a value for the 25 and 75th quartile
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
      
      # daily time series
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
rm(d6_end_2)

day25 <- d10[which(d10$Quart_approx == 25),] |>
  group_by(Year, Taxa, Station) |>
  summarise(D25 = mean(Week))
day75 <- d10[which(d10$Quart_approx == 75),] |>
  group_by(Year, Taxa, Station)|>
  summarise(D75 = mean(Week))
day50 <- d10[which(d10$Quart_approx == 50),] |>
  group_by(Year, Taxa, Station)|>
  summarise(Middle = mean(Week))
duration <- day25 |>
  merge(day75,
        by = c("Taxa", "Year", "Station")) |>
  merge(day50,
        by = c("Taxa", "Year", "Station")) |>
  transform(Dur = D75 - D25)

################################################################################
# 11. Bloom Magnitude
magnitude <- df_interpolated |>
  dplyr::select( - Season)|>
  merge(duration,
        by=c("Taxa", "Year", "Station")) |>
  filter(Week >= D25) |>
  filter(Week <= D75) |>
  
  group_by(Station, Taxa, Year) |>
  summarise(Max = max(Abundance),
            Avg = mean(Abundance, 
                       na.rm=T)) |>
  as.data.frame()
# 12. Merge all and empty the environment
Full_pp <- duration |>
  merge(peak,
        by = c("Year", "Taxa", "Station")) |>
  merge(magnitude,
        by = c("Year", "Taxa", "Station")) |>
  mutate(Group = "Phytoplankton") |>
  dplyr::select(Taxa,
                Year,
                Station,
                Group,
                Timing,
                Max,
                Avg,
                D25,
                D75,
                Middle,
                Dur)
names(Full_pp) <- c("Taxa",
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

Full_pp |>
   mutate(Taxa = ifelse(Taxa == "Summer_Diatoms", "Fall_Diatoms", as.character(Taxa))) |>
   write.table("Output/Data/Weekly_Subsampled_Full_PP.txt",
               sep = ";")

rm(list=ls())
################################################################################
## Subsampling Zooplankton
zp <- readRDS("Output/Data/zooplankton_sampling_before_interpolation.rds")
# Add a new column for the week
df <- zp %>%
  mutate(Week = week(SDATE),
         Year = year(SDATE),
         Y_W = paste(Year, Week, sep ="_"))
# Select the same month as for the phytoplankton
month <- readRDS("Output/Data/Week_Year_selected.rds")

d1 <- df |> 
  filter(Y_W %in% month$Y_W,
         Station == "BY31") |> 
  dplyr::select(-c(Week, Year, Y_W))


# Print the result
d1 |> 
  dplyr::select(Station, SDATE) |>
  unique() |> 
  mutate(Year = year(SDATE), Month = month(SDATE)) |> 
  group_by(Year, Month, Station) |> 
  summarise(N = n()) %T>%
  
  #Save data for panel a
  write_rds("Output/Data/Sup_subsampled_zp.rds")%>%
  
  ggplot(aes(x=as.factor(Year), y = as.factor(Month), fill = N))+
  geom_tile()+facet_grid(Station~.)
################################################################################
# Continue the analysis from PhenologyZP.R Line 368
################################################################################
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
ZP |> 
  write.table("Output/Data/Weekly_Subsampled_Full_ZP.txt", 
              sep = ";")
rm(list=ls())
################################################################################
# Analysis --------
################################################################################
# Sampling frequency
sup1a <- readRDS("Output/Data/Sup_subsampled_zp.rds")|>
  mutate(facet = "Zooplankton")
sup1b <- readRDS("Output/Data/Sup_subsampled_pp.rds")|>
  mutate(facet = "Phytoplankton")
sup1a |> rbind(sup1b) |>  
  mutate(Month = case_when(Month == 1 ~ "Jan",
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
                                   "Jan")),
         facet = factor(facet, levels= c("Zooplankton", "Phytoplankton"))) |> 
  
  ggplot(mapping = aes(y = Month,
                       x = Year,
                       fill = as.factor(N))) +
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
ggsave("Output/Figures/Sup_Fig_S6_ab.pdf", width = 7, height = 6)
# relationship between subsampled dataset and complete dataset
Subsampled_zp <-
  read.delim("Output/Data/Weekly_Subsampled_Full_ZP.txt", sep =";") |>
  dplyr::select(Taxa, Year, Timing, Magnitude, Start, End, Station, Group)
names(Subsampled_zp) <-
  c("Taxa", "Year", "sub_Timing", "sub_Magnitude", "sub_Start", "sub_End", "Station", "Group")
Subsampled_pp <-
 read.delim("Output/Data/Weekly_Subsampled_Full_PP.txt", sep = ";") |> 
 dplyr::select(Taxa, Year, Timing, Magnitude, Start, End, Station, Group)
names(Subsampled_pp) <-
 c("Taxa", "Year", "sub_Timing", "sub_Magnitude", "sub_Start", "sub_End", "Station", "Group")
Subsampled_df <-
  Subsampled_pp |> 
  rbind(Subsampled_zp)
Normal_pp <-
 read.delim("Output/Data/Weekly_Full_PP.txt", sep =";") |>
 dplyr::select(Taxa, Year, Timing, Magnitude, Start, End, Station, Group)
Normal_zp <-
  read.delim("Output/Data/Weekly_Full_ZP.txt", sep =";") |>
  dplyr::select(Taxa, Year, Timing, Magnitude, Start, End, Station, Group)
Normal_df <-
  Normal_pp |> 
  rbind(Normal_zp)
long_merged_df <-
 Subsampled_df |> 
 merge(Normal_df) |> 
 filter(Taxa !="Other",
        Station == "BY31") |> 
 mutate(sub_Start = as.integer(sub_Start),
        sub_End = as.integer(sub_End),
        Start = as.integer(Start),
        End = as.integer(End),
        Magnitude = log(Magnitude),
        sub_Magnitude = log(sub_Magnitude)) |> 
 gather(Parameter, Value, 5:12) |>
 group_by(Taxa, Parameter, Station) |> 
 mutate(z = (Value - mean(Value)) / sd(Value)) |> ungroup()
long_merged_df |> 
 mutate(facet = ifelse(Parameter %in% c("Start", "End", "Magnitude", "Timing"), "orignial",
                       ifelse(Parameter %in% c("sub_Start", "sub_End", "sub_Magnitude", "sub_Timing"), "subsampled", "c")),
        Param = ifelse(Parameter %in% c("Start", "End", "Magnitude", "Timing"), as.character(Parameter),
                       ifelse(Parameter == "sub_Start", "Start", ifelse(Parameter == "sub_End", "End", ifelse(Parameter == "sub_Timing", "Timing", ifelse(Parameter == "sub_Magnitude", "Magnitude", "X")) ))),
        Param = factor(Param, levels=c("Start", "Timing", "End", "Magnitude")),
        Taxa = factor(Taxa, levels= c("Spring_bloom", "Spring_Diatoms", "Dinoflagellates", "Mesodinium", "Summer_bloom", "Synechococcales", "Cyanobacteria", "Fall_Diatoms",
                                      "Synchaeta", "Copepoda", "Acartia", "Centropages", "Temora", "Pseudocalanus", "Cladocera", "Evadne", "Bosmina"))) |> 
 
 ggplot(aes(x = Year, y = z, col = facet))+
 geom_line()+
 facet_grid(Taxa~Param)

# x-axis = subsampled dataset, y-axis = complete dataset
wide_merged_df <-
 long_merged_df |> 
 dplyr::select(-z) |> 
 spread(key = Parameter, value = Value) |> 
  mutate(Taxa = factor(Taxa, levels= c("Spring_bloom", "Spring_Diatoms", "Dinoflagellates", "Mesodinium", "Summer_bloom", "Synechococcales", "Cyanobacteria", "Fall_Diatoms",
                                       "Synchaeta", "Copepoda", "Acartia", "Centropages", "Temora", "Pseudocalanus", "Cladocera", "Evadne", "Bosmina")),
         Gr = ifelse(Group == "Phytoplankton", "Phytoplankton", "Zooplankton"))

cor(wide_merged_df$sub_Timing, wide_merged_df$Timing, method= "spearman")
d<-wide_merged_df |> 
  ggplot(mapping = aes(y = sub_Timing, x = Timing)) +
  geom_point(size = 3, alpha = 1, stroke = .5,shape = 21,
             mapping = aes(fill = Taxa)) +
  #geom_smooth(method = "lm", se = F, col = 1) +
  geom_abline(slope = 1, intercept = 0, lty = 2 ) +
  scale_fill_manual(values = c("#b2182b", "#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac",
                               "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  labs(x = "Seasonal timing (week)\nOriginal", y = "Seasonal timing (week)\nSubsampled") +
  theme_classic() +
  ylim(2,52)+xlim(2,52)

cor(wide_merged_df$sub_Magnitude, wide_merged_df$Magnitude, method= "spearman")
f<-wide_merged_df |> 
  ggplot(mapping = aes(y = (sub_Magnitude), x = (Magnitude))) +
  geom_point(shape = 21, size = 3, alpha = 1,
             mapping = aes(fill = Taxa)) +
  #geom_smooth(method = "lm", se = F, col = 1) +
  geom_abline(slope = 1, intercept = 0, lty = 2 ) +
  scale_fill_manual(values = c("#b2182b", "#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac",
                               "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  labs(x = "Peak magnitude (log(ugC L-1))\nOriginal", y = "Peak magnitude (log ugC L-1)\nSubsampled") +
  
  theme_classic() +
  ylim(-2,8)+xlim(-2,8)
cor(wide_merged_df$sub_Start, wide_merged_df$Start, method= "spearman")
c<-wide_merged_df |> 
  ggplot(mapping = aes(y = (sub_Start), x = (Start))) +
  geom_point(shape = 21, size = 3, alpha = 1,
             mapping = aes(fill = Taxa)) +
  #geom_smooth(method = "lm", se = F, col = 1) +
  geom_abline(slope = 1, intercept = 0, lty = 2 ) +
  scale_fill_manual(values = c("#b2182b", "#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac",
                               "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  labs(x = "Peak initiation (week)\nOriginal", y = "Peak initiation (week)\nSubsampled") +
  
  theme_classic()+
  ylim(2,52)+xlim(2,52)
cor(wide_merged_df$sub_End, wide_merged_df$End, method= "spearman")
e<-wide_merged_df |> 
  ggplot(mapping = aes(y = (sub_End), x = (End))) +
  geom_point(shape = 21, size = 3, alpha = 1,
             mapping = aes(fill = Taxa)) +
  #geom_smooth(method = "lm", se = F, col = 1) +
  geom_abline(slope = 1, intercept = 0, lty = 2 ) +
  scale_fill_manual(values = c("#b2182b", "#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac",
                               "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  labs(x = "Peak termination (week)\nOriginal", y = "Peak peak termination (week)\nSubsampled") +
  
  theme_classic()+

  ylim(2,52)+xlim(2,52)
library(patchwork)
(c+d)/(e+f)+plot_layout(guides = "collect")
ggsave("Output/Figures/Sup_Fig_S6_cf.pdf", width = 8, height=6)
################################################################################
# Trend analysis like in Fig. 3
library(mblm)
library(Kendall)
prediction = data.frame()
for(a in unique(long_merged_df$Parameter)){
 df_try <- long_merged_df |>
   filter(Parameter == a, Year <=2022)
 for(t in unique(df_try$Taxa)){
   df4 <- df_try |>
     filter(Taxa == t)

     
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
                             Parameter = a,
                             Taxa = t) |>
       rbind(prediction)
   
 }
}
unique(prediction$Parameter)
df_plot_updated <-
 prediction |> 
 dplyr::select(p_trend, p_slope, slope, conf_sup, conf_inf, Parameter, Taxa) |> 
 mutate(facet = ifelse(Parameter %in% c("Start", "End", "Magnitude", "Timing"), "orignial",
               ifelse(Parameter %in% c("sub_Start", "sub_End", "sub_Magnitude", "sub_Timing"), "subsampled", "c")),
        Param = ifelse(Parameter %in% c("Start", "End", "Magnitude", "Timing"), as.character(Parameter),
                       ifelse(Parameter == "sub_Start", "Start", ifelse(Parameter == "sub_End", "End", ifelse(Parameter == "sub_Timing", "Timing", ifelse(Parameter == "sub_Magnitude", "Magnitude", "X")) )))) |> 
 unique() |> 
 filter(facet != "c",
        Taxa %in% c("Temora", "Synchaeta", "Pseudocalanus", "Centropages", "Acartia", "Copepoda", "Evadne", "Cladocera", "Bosmina", "Fall_Diatoms", "Summer_bloom", "Spring_Diatoms", "Spring_bloom", "Mesodinium", "Dinoflagellates", "Synechococcales","Cyanobacteria")) |> 
 mutate(Param = factor(Param, levels= c("Start", "Timing", "End", "Magnitude")),
        P_slope = case_when(
          p_slope <= 0.05 ~ "P < 0.05",
          p_slope > 0.05 & p_slope <= 0.1 ~ "P < 0.1",
          p_slope > 0.1 ~ "P > 0.1"),
        P_trend = case_when(
          p_trend <= 0.05 ~ "P < 0.05",
          p_trend > 0.05 & p_trend <= 0.1 ~ "P < 0.1",
          p_trend > 0.1 ~ "P > 0.1"),
        Taxa = factor(Taxa, levels = c("Spring_Diatoms", "Spring_bloom", "Dinoflagellates", "Mesodinium", "Synchaeta", "Evadne", "Summer_bloom", "Cyanobacteria", "Cladocera","Acartia","Centropages","Copepoda","Temora","Pseudocalanus","Bosmina","Synechococcales", "Fall_Diatoms"))
 )

df_plot_updated |> 
  filter(facet == "subsampled") |> 
  ggplot() +
 geom_hline(yintercept = 0) +
 geom_errorbar(
               mapping = aes(y = slope, ymin = conf_inf, ymax= conf_sup, x = Taxa),
               width = 0) +
 geom_point(
            size = 4,
            mapping = aes(y = slope, x = Taxa, fill = P_trend),
            shape = 21) +
 
 scale_shape_manual(values = c(21, 21, 21, 21))+
 scale_y_continuous() +
 facet_grid(Param~facet) +
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
ggsave("Output/Figures/Sup_Fig_S6_gj.pdf", width = 5, height=8.4)

sessionInfo()
