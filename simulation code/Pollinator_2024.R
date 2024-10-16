library(tidyverse)
library(car)
library(performance)
library(ggfortify)
library(MuMIn)
# 2024 simulation
# pollinator richness and fragmentation
# Responses: species richness (max 252), richness of good dispersers, richness of poor dispersers
# Predictors: patch size (1ha to 10ha, poisson distribution), proportion of suitable habitat in 1km of site (0.01 - 0.7), flower forb richness (normal distribution with mean = 20, sd = 5)
df <- tibble(
  patch_size = rpois(100, 6),
  patch_size_scaled = scale(patch_size),
  prop_habitat = runif(100, min = 0.01, max = 0.7),
  prop_habitat_scaled = scale(prop_habitat),
  forb_richness = round(rnorm(100, 20, 5)),
  forb_richness_scaled = scale(forb_richness),
) %>% 
  mutate(sp_rich_mean = 4+(0.04*patch_size_scaled)+(0.25*prop_habitat_scaled) + (0.14*forb_richness_scaled),
         error_good = rnorm(100, 1, 20),
         error_bad = rnorm(100, 1, 25),
         abund_good = round(80 + 5*patch_size_scaled + 3*forb_richness_scaled + error_good, 0) %>% as.numeric(), 
         abund_bad = round(100 + 5*prop_habitat_scaled + 1*patch_size_scaled + error_bad, 0) %>% as.numeric(),
         sp_rich = round(rpois(100, exp(sp_rich_mean)), 0)
  )

# write datafile
write_csv(df %>% select(sp_rich, 
                        abund_good,
                        abund_bad,
                        patch_size,
                        prop_habitat,
                        forb_richness),
          "pollinator_fragmentation2024.csv")

#load data
df <- read.csv("pollinator_fragmentation2024.csv", header=T)

sp_rich_mod <- glm(sp_rich ~ scale(patch_size) + scale(prop_habitat) + scale(forb_richness), family = "poisson", data = df)
summary(sp_rich_mod)
vif(sp_rich_mod)
sp_rich_mod$deviance /sp_rich_mod$df.residual

scatterplotMatrix(~sp_rich + patch_size + prop_habitat +forb_richness , data = df, diag = list(method = "boxplot"))

abund_good_mod <- glm(abund_good ~ scale(patch_size) + scale(forb_richness) + scale(prop_habitat), data = df,family="poisson")
scatterplotMatrix(~abund_good + patch_size + prop_habitat +forb_richness , data = df, diag = list(method = "boxplot"))

summary(abund_good_mod)
vif(abund_good_mod)
abund_good_mod$deviance /abund_good_mod$df.residual

abund_bad_mod <- glm(sp_rich ~ scale(patch_size) + scale(prop_habitat) +scale(forb_richness), family="poisson", data = df)
summary(abund_bad_mod)
vif(abund_bad_mod)
abund_bad_mod$deviance /abund_bad_mod$df.residual

write_csv(df %>% select(sp_rich, 
                        abund_good,
                        abund_bad,
                        patch_size,
                        prop_habitat,
                        forb_richness),
          "pollinator_fragmentation2024.csv")
