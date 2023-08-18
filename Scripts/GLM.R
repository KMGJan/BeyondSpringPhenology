################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                     GLM with Normalised values                               #
################################################################################
rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(patchwork)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(MASS)
library(knitr)
################################################################################
zp <- read.delim("Output/Data/Weekly_Full_ZP.txt",
                 sep=";")

abiotic <- read.delim("Output/Data/Abiotic.txt",
                      sep=";")
timing <- read.delim("Output/Data/Weekly_Full_PP.txt", 
                     sep=";") %>% 
  dplyr::select(Taxa, Year, Station, Timing, Magnitude) %>% 
  as.data.frame()

################################################################################
# 2. Prepare the environmental variables
# 2.a. Temperature between 0 and 20m in Summer and Spring
ABIOTIC <-
  abiotic %>% 
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
         Parameter_2 = paste(Parameter, season, sep = "_")) %>%
  
  
  
  as_tibble() %>% 
  filter(Parameter %in% c("T",
                          "BS",
                          "S"),
         Year %in% 2008:2022) %>%
  group_by(Year, Station, Parameter_2) |> 
  summarise(Value = mean(Value, na.rm = T)) |> 
  ungroup() |> 
  
  group_by(Station, Parameter_2) %>% 
  mutate(z = (Value - mean(Value)) / sd(Value)) %>% 
  ungroup() |>
  dplyr::select(-Value) |> 
  spread(Parameter_2, z)
################################################################################

################################################################################
#3. Process biotic variables
zp <- zp %>%
  dplyr::select(Year,
                Station,
                Taxa,
                Timing,
                Magnitude) %>%
  mutate(Magnitude = log(Magnitude)) %>% 
  gather(Parameter, Value, 4:5) %>%
  group_by(Station, Taxa, Parameter) %>%
  
  mutate(avg = mean(Value),
         anomalie = Value - avg,
         sd = sd(Value),
         z = anomalie / sd) %>% 
  dplyr::select(Year,
                Station,
                Taxa, 
                Parameter,
                z) %>% 
  spread(Parameter,
         z)
df_ab <- ABIOTIC %>% 
  merge(zp, by = c("Year", "Station"))
df_final <- timing %>%
  dplyr::select(Taxa,
                Year,
                Station,
                Timing,
                Magnitude) %>% 
  mutate(
         Magnitude = log(Magnitude)) %>%  
  gather(Metrics,
         Value,
         4:5) %>%
  mutate(Metrics = paste(Taxa,
                         Metrics,
                         sep = "_")) %>%
  dplyr::select(- Taxa) %>% 
  group_by(Station,
           Metrics) %>%
  
  mutate(avg = mean(Value),
         anomalie = Value - avg,
         sd = sd(Value),
         z = anomalie / sd) %>% 
  dplyr::select(Year,
                Station, 
                Metrics,
                z) %>% 
  
  spread(Metrics,
         z) %>%
  as.data.frame() %>% 
  merge(df_ab,
        by = c("Year",
               "Station")) %>%
  mutate(Mismatch = Timing - Spring_bloom_Timing) %>%
  mutate(include = ifelse(Taxa %in% c("Acartia",
                                      "Temora",
                                      "Centropages",
                                      "Pseudocalanus",
                                      "Synchaeta"),
                          "Yes",
                          ifelse(Taxa == "Bosmina" &
                                   Station %in% c("BY31"),
                                 "Yes",
                                 ifelse(Taxa %in% c("Evadne") &
                                          Station %in% c("BY31"),
                                        "Yes",
                                        "No")))) %>%
  filter(include == "Yes") %>%
  dplyr::select(- include)
################################################################################
Mat <- df_final %>% 
  filter(Taxa %in% c("Acartia",
                     "Centropages",
                     "Temora",
                     "Pseudocalanus",
                     "Synchaeta",
                     "Evadne",
                     "Bosmina")) %>%
  dplyr::select(Year,
                Taxa,
                Timing,
                Station,
                Spring_bloom_Timing,
                Spring_bloom_Magnitude,
                T_spring, 
                S_spring, 
                Summer_bloom_Magnitude) %>% 
  unique()%>%
  spread(Taxa,
         Timing) %>%
  na.omit()
Mat
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Define the panel.cor function by running the code below:
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete.obs")
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r), col = ifelse(r<0,"red","blue"))
}
print(pairs(Mat[,3:13], diag.panel = panel.hist,
            upper.panel = panel.smooth, lower.panel = panel.cor,
            main = "Timing"))

################################################################################

NAMES <- c("Factor", "Estimate", "Error", "z.value", "p.value", "R2")
outputa = data.frame()
for(i in c("Evadne", "Bosmina", "Acartia", "Centropages", "Temora","Pseudocalanus", "Synchaeta")){
  df <- df_final %>% 
    filter(Taxa == i)|> 
    arrange(Year, Station)
  if(i %in% c("Acartia", "Centropages",  "Pseudocalanus", "Synchaeta", "Temora")){
    mod <- glm(Timing ~
                 T_spring + 
                 Spring_bloom_Timing + Summer_bloom_Timing,#Spring_bloom_Magnitude,
               df,
               family = "gaussian")
  }




  if(i %in% c("Bosmina", "Evadne")){
    mod <- glm(Timing ~
                 T_spring,
               df,
               family = "gaussian")
    
  }
  par(mfrow=c(2,2));print(plot(mod, main = paste("Timing", i, sep =" ")))
  SUM <- summary(mod)
  print(paste("Timing", i, sep = " "))
  print(SUM)
  ks<-ks.test(mod |> residuals(), "pnorm")
  
  print(paste(i, "One-sample Kolmogorov-Smirnov test p value =", ks$p.value))
  df <- SUM$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Factor") |> 
    mutate(R2 =   with(summary(mod), 1-deviance/null.deviance))

  df
  names(df) <- NAMES
  
  outputa <- df %>% 
    mutate(Taxa = i) %>%
    
    rbind(outputa)
}

################################################################################
mat2 <- df_final %>%
  
  filter(Taxa %in% c("Acartia",
                     "Temora",
                     "Centropages",
                     "Pseudocalanus", 
                     "Synchaeta",
                     "Bosmina",
                     "Evadne")) %>%
  
  dplyr::select(c(Year,
                  Cyanobacteria_Magnitude,
                  Spring_bloom_Magnitude,
                  Mismatch,
                  Taxa,
                  Magnitude,
                  S_spring,
                  T_spring)) %>%
  spread(Taxa,
         Magnitude)

print(pairs(mat2[,c(2:12)], diag.panel = panel.hist,
            upper.panel = panel.smooth, lower.panel = panel.cor,
            main = "Magnitude"))

outputb = data.frame()
for(i in c("Bosmina", "Evadne","Pseudocalanus", "Acartia", "Centropages", "Temora", "Synchaeta")){
  df <- df_final %>%
    filter(Taxa == i) %>%
    na.omit() |> 
    arrange(Year, Station)

  if(i != "Evadne" & i != "Bosmina"){
    mod <- glm((Magnitude) ~
                 Mismatch +
                 Summer_bloom_Magnitude +
                 T_spring + S_spring,
               df,
               family = "gaussian")

    }

  if(i %in% c("Bosmina", "Evadne")){
    mod <- glm(Magnitude ~
                 Summer_bloom_Magnitude, df, family = "gaussian")
    }
  par(mfrow=c(2,2));print(plot(mod, main = paste("Magnitude", i, sep =" ")))
  SUM <- summary(mod)
  print(paste("Magnitude", i, sep = " "))
  print(SUM)
  ks<-ks.test(mod |> residuals(), "pnorm")
  
  print(paste(i, "One-sample Kolmogorov-Smirnov test p value =", ks$p.value))
  df <- SUM$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Factor") |> 
    mutate(R2 =   with(summary(mod), 1-deviance/null.deviance))
  names(df) <- NAMES
  
  outputb <- df %>% 
    mutate(Taxa = i) %>%
    rbind(outputb)
}



output_b <- outputb %>% 
  mutate(GLM = "Magnitude")

output_a <- outputa %>%
  mutate(GLM = "Timing")
output_b %>%
  rbind(output_a) |> 
  print()
df_combined <- 
  output_b %>%
  rbind(output_a) %>%
  filter(Factor != "(Intercept)") %>%
  #filter(Taxa != "Copepoda") %>%
  mutate(P_VALUE = case_when(
    p.value <= 0.05 ~ "p < 0.05",
    p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
    p.value > 0.1 ~ "p > 0.1"),
    Factor = case_when(
      Factor == "Cyanobacteria_Magnitude" ~ "CM",
      Factor == "Spring_bloom_Magnitude" ~ "SpBM",
      Factor == "Summer_bloom_Magnitude" ~ "SuBM",
      Factor == "Spring_bloom_Timing" ~ "SpBT",
      Factor == "Summer_bloom_Timing" ~ "SuBT",
      Factor %in% c("S_spring", "BS_spring") ~ "Salinity",
      Factor %in% c("Magnitude_lag1", "Timing_lag1") ~ "Lagged value",
      
      Factor %in% c("T_spring") ~ "T°C",
      Factor == "Mismatch" ~ "TMI"),
    GLM = factor(GLM, levels = c("Timing", "Magnitude"))) %>% 
  mutate(P_VALUE= factor(P_VALUE, 
                         levels = c("ns",
                                    "p < 0.05",
                                    "p < 0.1",
                                    "p > 0.1")),
         Taxa = factor(Taxa,
                       levels = c("Synchaeta",
                                  "Bosmina",
                                  "Evadne",
                                  "Acartia",
                                  "Centropages",
                                  "Pseudocalanus",
                                  "Temora")),
         Factor = factor(Factor, 
                         levels = c("Salinity",
                                    "T°C",
                                    "SpBT",
                                    "SuBT",
                                    "SpBM",
                                    "TMI",
                                    "CM",
                                    "SuBM",
                                    "Lagged value")))
  


#  -------- Plot -----------
ggplot(data = df_combined,
       mapping = aes(x = Factor,
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

ggsave("Output/Figures/Fig5.pdf",
       height = 8,
       width = 6)
rm(list=ls())
sessionInfo()