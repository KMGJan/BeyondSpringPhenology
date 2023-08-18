rm(list=ls())
################################################################################
#                                                                 18.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                         Difference station                                   #
################################################################################
library(dplyr)
library(ggplot2)
library(tidyr)
################################################################################
zp <- 
  read.delim("Output/Data/Weekly_Full_ZP.txt",
                 sep=";") |> 
  filter(Taxa %in% c( "Copepoda",
                      "Acartia",
                      "Centropages",
                      "Pseudocalanus",
                      "Temora",
                      "Synchaeta")) |>
  mutate(Taxa = factor(Taxa,
                       levels = c("Copepoda",
                                  "Acartia",
                                  "Centropages",
                                  "Pseudocalanus",
                                  "Temora",
                                  "Synchaeta")),
         Station = factor(Station,
                          levels = c("BY31",
                                     "BY15",
                                     "BY5")))


out_ANOVA <- out_TukeyHSD <- data.frame()
par(mfrow = c(2, 2))
for(i in unique(zp$Taxa)){
  df <- zp |>
    filter(Taxa == i)
  
  mod <- aov(Timing ~  Station, data = df) ; plot(mod, main = paste("Timing", i))
  SUM <- anova(mod)
  P <- SUM$`Pr(>F)`[1]
  if(P <= 0.05){
    tuk <-TukeyHSD(mod)
    out_tuk <- tuk$Station |>
      as.data.frame() |>
      mutate(Taxa = i) |>
      tibble::rownames_to_column("Stations") |>
      mutate(p.adj = `p adj`) |>
      dplyr:: select(-`p adj`)
  } else {
    out_tuk = data.frame(Stations = NA,
                         diff = NA,
                         lwr = NA,
                         upr = NA,
                         p.adj = NA,
                         Taxa = i)
    }
  
  out_ANOVA <- SUM |>
    as.data.frame() |>
    mutate(Taxa = i) |>
    rbind(out_ANOVA)
  
  out_TukeyHSD <- out_tuk |> 
    rbind(out_TukeyHSD)
}

outt <- out_ANOVA |>
  tibble::rownames_to_column("Parameter") |>
  mutate(Parameter = ifelse(Parameter %in% c("Station",
                                             "Station1",
                                             "Station2",
                                             "Station3",
                                             "Station4",
                                             "Station5"),
                            "Station",
                            "Residuals")) |>
  filter(Parameter == "Station")

Tuk <- out_TukeyHSD |>
  mutate(tukeyP = p.adj) |>
  dplyr::select(-p.adj)

ZP_Tim <- outt |>
  mutate(p.adj = p.adjust(outt$`Pr(>F)`,
                          method = "fdr")) |>
  right_join(Tuk) |>
  mutate(Parameter = "Timing")
ZP_Tim |>
  dplyr::select(-c(diff, lwr, upr))|>
  knitr::kable(digits = 2) |>
  print()

out_ANOVA = out_TukeyHSD = data.frame()
for(i in unique(zp$Taxa)){
  df <- zp |>
    filter(Taxa == i)
  mod <- aov(log(Magnitude) ~ Station,
             df) 
  par(mfrow=c(2,2)); plot(mod , main = paste("Magnitude", i))
  SUM <- anova(mod)
  P <- SUM$`Pr(>F)`[1]
  if(P <= 0.05){tuk <-TukeyHSD(mod)
  out_tuk <- tuk$Station|>as.data.frame()|>
    mutate(Taxa = i) |>
    tibble::rownames_to_column("Stations")|>
    mutate(p.adj = `p adj`)|>
    dplyr:: select(-`p adj`)
  }
  if(P > 0.05){out_tuk = data.frame(Stations = NA,
                                    diff = NA,
                                    lwr = NA,
                                    upr = NA,
                                    p.adj = NA,
                                    Taxa = i)}
  
  out_ANOVA <- SUM|>as.data.frame()|>
    mutate(Taxa = i) |>
    rbind(out_ANOVA)
  out_TukeyHSD <- out_tuk |> 
    rbind(out_TukeyHSD)
}
outt<-out_ANOVA |>
  tibble::rownames_to_column("Parameter") |>
  mutate(Parameter = ifelse(Parameter %in% c("Station",
                                             "Station1",
                                             "Station2",
                                             "Station3",
                                             "Station4",
                                             "Station5"),
                            "Station",
                            "Residuals"))|>
  filter(Parameter == "Station")
Tuk <- out_TukeyHSD |>
  mutate(tukeyP = p.adj) |>
  dplyr::select(-p.adj)
ZP_Mag <- outt |>
  mutate(p.adj = p.adjust(outt$`Pr(>F)`,
                          method = "fdr")) |>
  right_join(Tuk) |>
  mutate(Parameter = "Magnitude")

ZP_Mag |>
  dplyr::select(-c(diff, lwr, upr))|>
  knitr::kable(digits = 2) |>
  print()
# Phytoplankton ----------------------------------------------------------------
pp <- read.delim("Output/Data/Weekly_Full_PP.txt",
                 sep=";")

pp2 <- pp |>
  filter(Taxa %in% c("Cyanobacteria",
                     "Synechococcales",
                     "Mesodinium",
                     "Dinoflagellates",
                     "Spring_bloom",
                     "Spring_Diatoms",
                     "Summer_bloom",
                     "Fall_Diatoms")) |>
  mutate(Taxa = factor(Taxa,
                       levels = c("Cyanobacteria",
                                  "Synechococcales",
                                  "Dinoflagellates",
                                  "Mesodinium",
                                  "Spring_bloom",
                                  "Spring_Diatoms",
                                  "Summer_bloom",
                                  "Fall_Diatoms")),
         Station = factor(Station,
                          levels = c("BY31",
                                     "BY15",
                                     "BY5")))

out_ANOVA = out_TukeyHSD = data.frame()
for(i in unique(pp2$Taxa)){
  df <- pp2 |>
    filter(Taxa == i)
  mod <- aov(Timing ~ Station, df) 
  par(mfrow=c(2,2)); plot(mod, main = paste("Timing", i))
  SUM <- anova(mod)
  P <- SUM$`Pr(>F)`[1]
  if(P <= 0.05){
    tuk <- TukeyHSD(mod)
    tuk
    out_tuk <- tuk$Station |>
      as.data.frame() |>
      mutate(Taxa = i) |>
      tibble::rownames_to_column("Stations") |>
    mutate(p.adj = `p adj`) |>
    dplyr:: select(-`p adj`)
  out_tuk
    }
  if(P > 0.05){out_tuk = data.frame(Stations = NA,
                                    diff = NA,
                                    lwr = NA,
                                    upr = NA,
                                    p.adj = NA,
                                    Taxa = i)}
  
  out_ANOVA <- SUM|>as.data.frame()|>
    mutate(Taxa = i) |>
    rbind(out_ANOVA)
  out_TukeyHSD <- out_tuk |> 
    rbind(out_TukeyHSD)
}

outt <- out_ANOVA |>
  tibble::rownames_to_column("Parameter") |>
  mutate(Parameter = ifelse(Parameter %in% c("Station",
                                             "Station1",
                                             "Station2",
                                             "Station3",
                                             "Station4",
                                             "Station5", "Station6"),
                            "Station",
                            "Residuals")) |>
  filter(Parameter == "Station")
Tuk <- out_TukeyHSD |>
  mutate(tukeyP = p.adj) |>
  dplyr::select(-p.adj)
PP_Tim <- outt |>
  mutate(p.adj = p.adjust(outt$`Pr(>F)`,
                          method = "fdr")) |>
  right_join(Tuk) |>
  mutate(Parameter = "Timing")
PP_Tim |> 
  dplyr::select(-c(diff, lwr, upr))|>
  knitr::kable(digits = 2) |>
  print()
out_ANOVA = out_TukeyHSD = data.frame()
for(i in unique(pp2$Taxa)){
  df <- pp2 |>
    filter(Taxa == i)
  mod <- aov(log(Magnitude) ~ Station, df) 
  par(mfrow=c(2,2)); plot(mod, main = paste("Magnitude", i))
  SUM <- anova(mod)
  P <- SUM$`Pr(>F)`[1]
  if(P <= 0.05){tuk <-TukeyHSD(mod)
  out_tuk <- tuk$Station |>
    as.data.frame() |>
    mutate(Taxa = i) |>
    tibble::rownames_to_column("Stations") |>
    mutate(p.adj = `p adj`) |>
    dplyr:: select(-`p adj`)
  }
  if(P > 0.05){out_tuk = data.frame(Stations = NA,
                                    diff = NA,
                                    lwr = NA,
                                    upr = NA,
                                    p.adj = NA,
                                    Taxa = i)}
  
  out_ANOVA <- SUM|>as.data.frame()|>
    mutate(Taxa = i) |>
    rbind(out_ANOVA)
  out_TukeyHSD <- out_tuk |> 
    rbind(out_TukeyHSD)
}
outt <- out_ANOVA |>
  tibble::rownames_to_column("Parameter") |>
  mutate(Parameter = ifelse(Parameter %in% c("Station",
                                             "Station1",
                                             "Station2",
                                             "Station3",
                                             "Station4",
                                             "Station5", "Station6"),
                            "Station",
                            "Residuals")) |>
  filter(Parameter == "Station")
Tuk <- out_TukeyHSD |>
  mutate(tukeyP = p.adj) |>
  dplyr::select(-p.adj)
PP_Mag <- outt |>
  mutate(p.adj = p.adjust(outt$`Pr(>F)`,
                          method = "fdr")) |>
  right_join(Tuk) |>
  mutate(Parameter = "Magnitude")
PP_Mag |> 
  dplyr::select(-c(diff, lwr, upr))|>
  knitr::kable(digits = 3) |>
  print()
Summary <- rbind(ZP_Mag,
                 ZP_Tim,
                 PP_Mag,
                 PP_Tim)

Summary |> 
  mutate(Taxa = ifelse(Taxa == "Summer_Diatoms", "Fall Diatoms", ifelse(
    Taxa == "Spring_Diatoms", "Spring Diatoms", ifelse(
      Taxa == "Summer_bloom", "Summer bloom", ifelse(
        Taxa == "Spring_bloom", "Spring bloom", as.character(Taxa))))),
    Taxa = factor(Taxa,
                  levels = c("Spring Diatoms",
                             "Spring bloom",
                             "Summer bloom",
                             "Dinoflagellates",
                             "Cyanobacteria",
                             "Fall Diatoms",
                             "Synchaeta",
                             "Mesodinium",
                             "Copepoda",
                             "Acartia",
                             "Temora",
                             "Centropages",
                             "Pseudocalanus"))) |>
  filter(Taxa %in% c("Fall Diatoms",
                     "Summer bloom",
                     "Pseudocalanus",
                     "Temora",
                     "Centropages",
                     "Acartia",
                     "Copepoda",
                     "Cyanobacteria",
                     "Mesodinium",
                     "Synchaeta",
                     "Dinoflagellates",
                     "Spring bloom" ,
                     "Spring Diatoms")) |>
  dplyr::select(Parameter, Taxa, p.adj, Stations, tukeyP) |>
  write.csv("Output/Data/Station_Differences.csv")

rm(list=ls())
sessionInfo()