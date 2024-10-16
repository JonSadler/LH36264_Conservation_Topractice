# Formative exercise solution
# Jon Sadler 2nd March 2023

# Load libraries we need.
library(tidyverse)
library(car)
library(ggsignif) # new library you'll need to install it.
library(dplyr) 
library(tidyr)

# Load data
pred <- read.csv(file.choose())

# Look data
glimpse(pred)
head(pred)
View(pred)
# We see that the bullhead/loach columns are character fields - you'll not get all the validation plots if you 
# don't use factors

# change character fields to factors 
pred$Bullhead <- as.factor(pred$Bullhead)
pred$Loach <- as.factor(pred$Loach)
glimpse(pred) # check it worked

# Check for balance necessary for a two-way ANOVA
table(pred$Bullhead, pred$Loach)
# Fine so we can go with 2 way ANOVA (as we have one response and two FACTORS)
# Note - the fish are either present or absent....

# Draw some pictures
# Boxplots
op <- par(mfrow = c(1, 2)) # 1 row , 2 columns for the images
boxplot(Density ~ Bullhead, col = "lightblue", data = pred, xlab = "Bullhead")
boxplot(Density ~ Loach, col = "lightblue", data = pred, xlab = "Loach")
par(op)

# Or in ggplot - need to transform the data to a long format to do this easily
pred_long <- pred %>% gather(species,treatment, 1:2)
glimpse(pred_long) # looks fine we'll flip new variables to factors as we're going to create error bars plots 
pred_long <- pred_long %>% mutate(species = factor(species),treatment = factor(treatment)) # use glimpse(pred_long) to check it worked

ggplot(pred_long, aes(y=Density, x=species, fill=treatment)) + geom_boxplot() +
  labs(x = "Fish species", y = "Prey Density", fill = "Treatment") 
# Variances look okay and the pattern indicates that Loach reduce midge density but Bullheads don't.

# Check assumptions
# normality
qqnorm(pred$Density); qqline(pred$Density)
shapiro.test(pred$Density)
# Data are normally distributed....

# Homogeneity of variance
leveneTest(pred$Density ~ pred$Bullhead * pred$Loach) # No issues visible
#plot interaction

interaction.plot(pred$Bullhead, pred$Loach, pred$Density, col=c(2,3), xlab = "Predators", ylab = "Midge Density", trace.label = "")
# Indicates no interaction but it is sensible to include the interaction in the plot anyway

# Run ANOVA
pred_aov <- aov(Density ~ Loach * Bullhead, data = pred)

# Look at the output
summary(pred_aov)

# Results indicate that Loach reduces midge density but bullheads don't. There is no interaction between 
# the two explanatories (or the fish), so the addition of bullheads to the cages did not result in greater impact on midge density.

# Confirm all diagnostics [Remember you can use the autoplot function too, but you'll need to load the ggfortify library]
op <- par(mfrow = c(2, 2)) # set graphics device to plot four graphs in a 2 x 2 matrix
plot(pred_aov)
par(op)
# No big issues. There are no worrying patterns in residuals data point 13 stands out.
# so we'll look at it via leverage/Cook's Distance outlier plots.

plot(pred_aov, which=6) # plots leverage and cook's plot
# It's dancing on the margin but okay.....

# plot barchart with error bars to show the pattern for a report - we'll go for a publication level type file
# and output it to a file at high resolution. We'll use ggplot and the reshaped dataframe pred_long.
# Calculates mean, sd, se and IC
pred_sum <- pred_long %>%
  group_by(species,treatment) %>%
  summarise( 
    n=n(),
    mean=mean(Density),
    sd=sd(Density)) %>%
  mutate( se=sd/sqrt(n))  %>% # computes the standard error
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) # computes 95% CI

# using 2 standard error [at 95% or p=0.05 level]. Note: errors don't overlap for Loach
ggplot(pred_sum, aes(x=species, y=mean, fill=treatment)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), width=.2,position=position_dodge(.9)) + 
  labs(x = "Fish species", y = "Prey Density", fill = "Treatment") + theme_bw()

# or displayed with confidence intervals - see they don't overlap for loach but are little larger
ggplot(pred_sum, aes(x=species, y=mean, fill=treatment)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic),width=.2,position=position_dodge(.9)) + 
  labs(x = "Fish species", y = "Prey Density", fill = "Treatment")
# NOTE you need the position=position.dodge function to locate the error bars over the bars.
# The width function scales the width of the error bar = trying playing with it to see.

# Creating and saving a publication ready plot
ggplot(pred_sum, aes(x=species, y=mean, fill=treatment)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic),width=.2,position=position_dodge(.9)) + 
  theme_bw() + # remove default grey background
  scale_fill_manual(values = c("grey90","grey50")) + # override colours for treatment making them grey and white bars
  theme(axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x  = element_text(size=10, colour="black"),
        axis.text.y  = element_text(size=10, colour="black"),
        plot.caption=element_text(hjust = 0)) +
  labs(x = "Species", y = expression(Prey~Density~(m^2)), fill = "Treatment", 
       caption = "Figure 1. Species impacts on prey density in experiment enclosures.\nn = 10 per treatment group (total = 40) ** = p<0.01.\nError bars are 95 and 5% CIs.") +
  # \n = new line in caption
  # to add significant level use the annotate function. Check the aov call for sig levels
  annotate("text", x=2, y=4.2, label= "**") +
  annotate("text", x=1, y=4.2, label= "NS")

# save the plot [will be save to your working directory]
ggsave("Fish_pred_plot.png", height = 15, width=15, units="cm", dpi = 500)
 