library(tidyverse)
library(car)
library(performance)
library(ggfortify)
library(MuMIn)
# Brownfield Land scenario
# patch age (years) - 3 to 20 - uniform
# patch size (ha) - 1 to 30 - uniform
# patch isolation (m) - 10 - 1500 - normal
# management (artificial disturbance - yes/no) - random 
# Seed Forb abundance - normal
# n = 150
# Response  = Beetle SR

# model structure - patch age * patch isolation + patch size + management
df <- tibble(
patch_age =  round(rpois(150,25),0),
 patch_size = round(runif(150, 2, 40),0),
 patch_isolation = abs(rnorm(150, 1000, 400)),
 seed_A = round(runif(150, 20, 250),0),
) %>% 
  mutate(log_mean = 0.93 - 0.2*scale(patch_age) + 0.1*scale(patch_size) + 0.4*scale(seed_A)+ -0.4*scale(patch_isolation) +-0.4*scale(patch_age)*scale(seed_A),
         SR = round(rpois(150, exp(log_mean)), 0)) 

write_csv(df %>% select(-5), file = "derelict_abund2024.csv")

#load file
df <- read.csv("derelict_abund2024.csv", header=T)
scatterplotMatrix(~SR + patch_age + patch_size + seed_A + patch_isolation, data = df, diag = list(method = "boxplot"))


mod <- glm(SR ~ patch_age*patch_isolation + patch_age*seed_A + patch_size + patch_isolation, data = df, family = "poisson")
summary(mod)

vif(mod) # remove interaction

mod <- glm(SR ~ seed_A + patch_size+ patch_isolation+patch_age, data = df, family = "poisson")
summary(mod)
vif(mod)

par(mfrow=c(2,2)) # set to 2x2 plot panel
plot(mod)
par(mfrow=c(1,1)) # set to 1x1 plot panel i.e. default


#  OD calculation
mod$deviance /mod$df.residual # close and likely okay

mean(df$SR)
var(df$SR)


#MuMin code

options(na.action=na.fail) # set options in Base R concerning missing values

# run the full model
Full <- glm(SR ~ seed_A + patch_size+ patch_isolation+patch_age+seed_A*patch_age, data = df, family = "poisson")
# you could get away with this one....
Full$deviance /Full$df.residual
par(mfrow=c(2,2)) # set to 2x2 plot panel
plot(Full) 
par(mfrow=c(1,1))

# run model averaging/selection
mod_Mum <- model.avg(dredge(Full, rank = "AICc")) # code introduces model.avg(), get.models and dredge functions
summary(mod_Mum)

# select final model
final <- glm(SR ~ patch_age+patch_isolation+seed_A+patch_age:seed_A, data = df, family = "poisson")
summary(final)
final$deviance /final$df.residual
par(mfrow=c(3,2)) # set to 2x2 plot panel
plot(final,which=1:6) 
par(mfrow=c(1,1))

#plot residuals v explanatories
res <- resid(final, type='pearson')

par(mfrow=c(2,2))
plot(df$seed_A)
plot(df$patch_isolation)
plot(df$patch_age)
par(mfrow=c(1,1))

# marginal_effects for partials
install.packages("marginaleffects")
library(marginaleffects)
#from marginaleffects
plot_predictions(final, condition = c("seed_A"))
plot_predictions(final, condition = c("patch_age"))
plot_predictions(final, condition = c("patch_isolation"))


# using ggeffects
install.packages("ggeffects")
install.packages("sjPlot")
install.packages("broom.helpers")
install.packages("effects")
install.packages("ggpubr")
install.packages("cowplot")
library(broom.helpers)
library(sjPlot)
library(effects)
library(ggeffects)
library(ggpubr)
library(cowplot)
plot_model(final, type = "pred", terms = c("seed_A","patch_age")) #ggeffects?

df$patch_age <- as.numeric(df$patch_age)
df$patch_size <- as.numeric(df$patch_size)
df$patch_isolation <- as.numeric(df$patch_isolation)
str(df)

# plot partials from the effect package - ???
# par(mfrow=c(2,2)) does not work
jim <- plot(Effect("patch_age", final))
bob <- plot(Effect("patch_isolation", final))
me <- plot(Effect("seed_A", final))

ggarrange(jim, bob, rremove("x.text"),# from gridExtra
           labels = c("A", "B"),
           ncol = 2, nrow = 2)

plot_grid(A, B, C, rremove("x.text"),#from cowplot
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
           