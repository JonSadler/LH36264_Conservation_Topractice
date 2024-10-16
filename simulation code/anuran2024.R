library(tidyverse)
library(car)
library(performance)
library(ggfortify)
library(MuMIn)
# amphibian richness in urban ponds
# 2024
anuran <- read_csv("anuran_richness.csv") %>% 
  mutate(forest_perc_scaled = scale(forest_perc),
         traffic_scaled = scale(traffic),
         log_mean = 3.42 - 0.6*traffic_scaled + 0.3*forest_perc_scaled,
         sp_rich = round(rpois(36, exp(log_mean)), 0))

#write datafile
write_csv(anuran %>% select(site, edge, forest_perc, road, traffic, sp_rich), "anuran_richness2024.csv")

#load generated datafile

anuran_mod <- glm(sp_rich ~ scale(edge) + scale(traffic) +scale(forest_perc), family = "poisson", data = anuran)
summary(anuran_mod)

anuran <- read_csv("anuran_richness2024.csv") 
vif(anuran_mod)

anuran_mod$deviance /anuran_mod$df.residual

scatterplotMatrix(~sp_rich + edge + traffic +forest_perc , data = anuran, diag = list(method = "boxplot"))

