rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                   Phytoplankton Phenology Metrics                            #
################################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(roxygen2)
library(zoo)
library(shaRk)
library(xts)
library(reshape)
library(grid)
library(xts)
################################################################################
# Before we start create a folder in the Output folder
# to save the phytoplankton outputs
# 1. Import the files and some function
phytoplankton <- readRDS("Data/phytoplankton_02Aug23.rds")

scale_x_month <- scale_x_continuous(breaks = seq(0, 365, 30.5),
                                    limits = c(0, 366),
                                    labels= month.abb,
                                    expand = expand_scale(mult = c(0, 0), 
                                                          add = c(0, 0)))
theme_zp <- theme_classic() +
  theme(panel.border = element_rect(fill = "transparent"),
        axis.text = element_text(color = "black"))
################################################################################
# 2. Filter the data
phyto_table <- phytoplankton |>
  filter(Station %in% c("BY31 LANDSORTSDJ","BY5 BORNHOLMSDJ", "BY15 GOTLANDSDJ"),
         Year > 2006,
         Year < 2023,
         unit == "ugC/l",
         trophic_type_code %in% c("AU", "MX")) |>
  filter(Phylum !="<NA>") |>
  mutate(Taxa = ifelse(Phylum == "Bacillariophyta",
                       "Diatoms",
                       ifelse(Phylum == "Miozoa",
                              "Dinoflagellates",
                              ifelse(Genus == "Mesodinium",
                                     "Mesodinium", 
                                     ifelse(Phylum == "Cyanobacteria" & Order %in% c("Nostocales"), 
                                            "Cyanobacteria", ifelse(Phylum == "Cyanobacteria" & Order == "Synechococcales", "Synechococcales","Other")
                                            ))))) |>
  mutate(Station = ifelse(Station == "BY31 LANDSORTSDJ", "BY31",ifelse(Station == "BY15 GOTLANDSDJ", "BY15", "BY5")))
phyto_table|>dplyr::select(SDATE, Station) |> unique() |> mutate(month = month(SDATE), day = day(SDATE), year = year(SDATE), Week = as.numeric(strftime(SDATE, format = "%V"))) |> unique() |> 
  ggplot(aes(x= year, y = Week, fill = as.character(month)))+
  geom_tile()+
  geom_text(aes(label = Week)) +
  facet_grid(~Station)
dplyr::select(-SDATE) |> unique() |> filter(year != "2007") |> View()
phyto_table |>
  ggplot(aes(x=SDATE, y = log(Value), col=Depth))+
  geom_point()+
  facet_grid(Taxa~Station)
D10 <- subset(phyto_table, Depth == 20 & Station %in% c('BY31') & Month != 11)
D20 <- subset(phyto_table, Depth == 10 & Station != 'BY31')
D20.2 <- subset(phyto_table, Depth == 10 & Station == "BY31" & Month%in% c(1,2,11,12))
ptable1 <- rbind(D10,D20, D20.2)
ptable1 |>
  mutate(Week = as.numeric(strftime(SDATE, format = "%V"))) |>
  dplyr::select(Week, Taxa, Phylum, Class, Order, Family, Genus) |>
  mutate(Taxa = ifelse(Taxa == "Diatoms" & Week >= 5 & Week <= 22,
                       "Spring_Diatoms",
                       ifelse(Taxa == "Diatoms" & Week >= 37,
                              "Summer_Diatoms",
                              ifelse(Taxa == "Dinoflagellates" & Week >= 5 & Week <= 22,
                                     "Dinoflagellates",
                                     ifelse(Taxa == "Dinoflagellates" & Week > 22,
                                            "Not_included_Dinoflagellates",
                                            as.character(Taxa)))))) |> unique() |> filter(Taxa =="Synechococcales") |>  dplyr::select(-Week) |> unique()
rm(D10, D20)
################################################################################
# 3. Arrange the data to have taxa carbon content per day
d1 <- ptable1 |>
  group_by(Year,
           Month,
           SDATE,
           Taxa,
           Station,
           Depth) |>
  summarise(Value = sum(Value)) |>
  
  group_by(Year,
           Month,
           Taxa,
           Station) |>
  summarise(Value = mean(Value, 
                         na.rm = T))

ptable1 |>
  group_by(Month, Taxa, Station)|>
  summarise(Value = mean(Value, na.rm=T)) |>
  ggplot(mapping = aes(x = Month,
                       y = Value,
                       col = Taxa)) +
  geom_point()+
  geom_line()+
  facet_grid(.~Station)

ptable1 |> 
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
  write.table("Output/Data/sup_fig1b.txt",
              sep = ";")


dailyT <- ptable1 |>
  group_by(SDATE,
           Taxa,
           Station,
           Depth) |>
  summarise(Value = sum(Value)) |>
  
  group_by(SDATE,
           Taxa,
           Station) |>
  summarise(Value = mean(Value, 
                         na.rm = T))
################################################################################
# 4. Daily interpolation over the full time serie
df = data.frame()
for (i in unique(dailyT$Station)) {
  df <- dailyT |> 
    filter(Station == i,
           year(SDATE) > 2007) |>
    group_by(SDATE, Taxa) |>
    summarise(Value = mean(Value,
                           na.rm=T)) |>
    dailyInterpretation(taxa = "Taxa") |>
    mutate(Station = i)|>
    rbind(df)
}
df
df |>
  ggplot(aes(x=Week, y=Abundance, col=Taxa))+
  geom_line()+
  facet_grid(Station~., scales="free")

################################################################################
# 5. Plot the smoothed dynamics
Blooms <- data.frame(
  x = c(mean(c(42, 150)), 
             mean(c(151, 258))),
  y = 170,
  Station = "BY31",
  label = c("Spring bloom", "Summer bloom")) |>
  transform(Station = factor(Station,
                             levels = c("BY31", "BY5")))
df_try <- df |>
  group_by(Station, DOY)|>
  mutate(Total = sum(Abundance))|>
  spread(Taxa, Abundance) |> 
  gather("Taxa", "Abundance", 3:9) |>
  dplyr::select(DOY, Taxa, Abundance, Station)

Density = data.frame()
for( i in unique(df$Station)){
  Interp <- df_try|>
    filter(Station == i)
  
  Density <- Interp[rep(row.names(Interp), Interp$Abundance), 1:2] |>
    mutate(Station = i)|>
    rbind(Density)
}
Density |> 
  filter(Taxa !="Total") |>
  transform(Station = factor(Station, 
                             levels= c("BY31","BY15", "BY5")),
            Taxa = factor(Taxa, 
                          levels = c("Other",
                                     "Mesodinium",
                                     "Cyanobacteria",
                                     "Diatoms",
                                     "Dinoflagellates", "Synechococcales"))) |>
  ggplot(mapping = aes(x = DOY,
                       y =after_stat(count))) +
  geom_density(mapping = aes(col = Taxa, 
                             fill = Taxa),
               alpha = 1,
               size = 1.1,
               position="stack") +
  scale_color_manual(values = c("black", rep('transparent', 5))) +
  scale_fill_manual(values = c("#cccccc",
                               "#636363",
                               "#78c679",
                               "#d9f0a3",
                               "#006837",
                               "black")) +
  
  scale_y_continuous(expression(paste('Carbon content'~('ug'~L^{-1}))),
                     expand = expand_scale(mult = c(0, 0.1), 
                                           add = c(0, 0))) +
  facet_grid(Station~.) +
  theme_zp +   scale_x_month +

  geom_vline(xintercept = c(5*7, 22*7),
             lty = "dotted") +
  geom_vline(xintercept = c(37*7),
             lty = "dotted") +
  geom_text(data = Blooms,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            size = 6)
################################################################################

Density |> write.table("Output/Data/Density_pp.txt", sep = ";")
################################################################################
# 6. Visualise the interannual variability
d1 <- ptable1 |>
  group_by(Year,
           Month,
           SDATE,
           Taxa,
           Station,
           Depth) |>
  summarise(Value = sum(Value)) |>
  group_by(Year,
           Month, 
           Taxa, 
           Station) |>
  summarise(Value = mean(Value, 
                         na.rm = T))
av <- d1 |>
  group_by(Month, Taxa, Station) |>
  summarise(Value = mean(Value,
                         na.rm = T))

d1 |> ggplot(mapping = aes(x = Month,
                                  y = Value)) +
  geom_line(mapping = aes(colour = factor(Year)),
            size = 0.3) +
  geom_point(mapping = aes(colour = factor(Year))) +
  facet_grid(Taxa~Station, scales="free") +
  geom_line(data = av,
            mapping = aes(x = Month,
                          y = Value), 
            size = 1.5)
rm(av, d1)
################################################################################
# 7. Interpolate the data daily
library(timetk)
d1 <- ptable1 |>
  group_by(SDATE,
           Taxa,
           Station,
           Depth) |>
  summarise(Value = sum(Value)) |>
  group_by(SDATE, 
           Taxa,
           Station) |>
  summarise(Value = mean(Value, 
                         na.rm = T)) |>
  group_by(SDATE, Station) |>
  mutate(Total = sum(Value)) |>
  spread(Taxa, Value) |> 
  gather(Taxa, Value, 3:9) |>
  as.data.frame() |> 
  group_by(SDATE,
           Station,
           Taxa) |>
  summarise_by_time(.by = "week",.week_start = min(as.Date(ptable1$SDATE)), .date_var = SDATE,
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
# if you want to save this dataset, run the 3 lines below
#df_interpolated |>
#  write.table("./Output/PP/Data/daily_interpolation.txt", 
#              sep = ";")

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
# if you want to save this dataset, run the 4 lines below
#peak |>
#  write.table("./Output/PP/Data/Timing.txt",
#              sep = ";",
#              dec = ".")

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
  theme_zp +
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
# if you want to save this dataset, run the 3 lines below
#duration |>
#  write.table("./Output/PP/Data/Duration.txt",
#              sep=";")

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
# if you want to save this dataset, run the 3 lines below
#magnitude |>
#  write.table("./Output/PP/Data/Magnitude.txt",
#              sep = ";")
rm(Density, df_interpolated, theme_zp, allDates, ptable1, day50)
################################################################################
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
  write.table("Output/Data/Weekly_Full_PP.txt",
              sep = ";")

rm(list=ls())
sessionInfo()
