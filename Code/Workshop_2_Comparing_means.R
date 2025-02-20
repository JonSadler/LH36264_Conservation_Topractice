  # *******************************************************************************
  # This is the source script for Workshop 2: Testing for differences between groups *
  # JPS 13th Feb 2023
# ******************************************************************************
  
# Code filename: Workshop2_comparing_means.R
# Datafiles: 
# Ozone.csv # Use for a two sample comparison
# Algae.csv # Use for > than 2 sample comparison (only one grouping variable - so Single Factor Anova)
# Limpet.csv # Use for > than 2 sample comparison (two grouping variables - so Two-way Anova) with not interaction
# Grazing.csv # Used for > than 2 sample comparison (two grouping variables - so Two-way Anova) with an interaction

# Read Beckerman et al. Chapter 5 for T-tests, single factor ANOVA and Chapter 6 for two-way ANOVA
# Note they use lm() function while we'll use the aov() function. They do the same thing

# DATAFILES FOR THE WORKSHOP
# Load in the CVS files at once. We're are going to use a different command. THE FILE MUST BE IN THE WORKING DIRECTORY!
  
Ozone <- read.csv("ozone.csv") # going to use for a 2-sample test
Algae <- read.csv("Algae.csv")
Limpet <- read.csv("Limpet.csv") # going to use for a two-way ANOVA (no interaction)
Graze <-read.csv("Grazing.csv") # going to use for a two-way ANOVA (with an interaction)

# NOTICE THEY ARE ALL IN LONG DATA FORMAT (DESCRIBED IN BECKERMAN ET AL'S BOOK)

# **********************************************************
# LIBRARIES NEEDED:
# Tidyverse
# Car
# **********************************************************
library(tidyverse) # install if not there - check in packages (bottom right pane)
library(car) # install if not there - check in packages (bottom right pane)
library(dplyr)

# ************************************************
# PART 1: Comparisons of two independent samples *
# ************************************************

# Look at the Ozone datafile....
glimpse(Ozone)
str(Ozone)
dim(Ozone)
View(Ozone) #NOTE - this is an RStudio function call so notice it is capitalised....

# See we have a character field; we need this to be a factor
Ozone$Garden <- as.factor(Ozone$Garden)

# Basic boxplot to look for structure and patterns in the data
boxplot(O3 ~ Garden, data = Ozone,
        ylab = "O3 concentration",
        xlab = "Garden", col = "blue")
# Looks like the gardens have different ozone levels

# Repeat using ggplot package
ggplot(Ozone, aes(x=Garden, y=O3))+ geom_boxplot() + labs(x = "Garden", y = "Ozone mg/3")

# We wish to know if they differ significantly we use a means test but there assumptions on the use of these: 
# i. normality of data, 
# ii. homogeneity of variances

# (i) plotting a qqplot (use help ?qqplot)# (ii) plotting a QQPlot READ the help file!
# These test normality like - histograms....
qqnorm(Ozone$O3); qqline(Ozone$O3) # The data sit on the line so we assume normality. 

# Repeat it using ggplot
ggplot(Ozone, aes(sample = O3)) + stat_qq() + stat_qq_line()

# We can test for this using the shapiro-wilks test
shapiro.test(Ozone$O3) # The test returns p=0.6495 so we reject the accept hypothesis (p is >0.05)
# NOTE - the null here is that the data are drawn from a normal distribution
# So we want a non-significant result! The opposite of most other tests!!!!

# (ii) test for heterogeneity of variances using levenes test (You need to load the 'car' library first; then select ??leveneTest)
library(car) # if you haven't already loaded it...
leveneTest(O3 ~ Garden, data = Ozone) # P value - 1 so we reject the NULL hypothesis
# The NULL here is that the variance is homogeneous
# Same as the shapiro-wilks we want a non-significant outcome
# Outcome of testing indicates normality of data and homogeneity of variance so we 
# use parametric tests. For a two sample comparison this is a T-test.
                                         
# Testing for difference between the gardens using a t-test. t-test with using a grouping factor
t.test(O3 ~ Garden, data = Ozone) # Note - the data argument and '~' (tilde) operator (you'll see a lot of this)

# The p value here is 0.001115; we reject the Null (i.e. the means are the same)
# Our interpretation is that the gardens have very different levels of Ozone.
# If either normality or homogeneity of variance tests are false then we use a non-parametric test
# which for 2 samples is a U-Tests (Wilcoxon tests). We need to install a new library to do this called coin:
install.packages("coin")                                           

# Load the library 
library(coin)

#.....and run the test
wilcox_test(O3 ~ Garden, data = Ozone, distribution = "exact")

#********************************************************
# PART TWO: Comparisons of more than two samples        *
#********************************************************
# We will start with the case where we only have ONE grouping variable
# Or Single Factor Anova. 
# We'll be using the aov() function to this (but both both lm and glm will work)
# Use Algae dataset. This is algal biomass on three different substrates Marble, Slate and Granite in tanks in an experiment
# Standard filechecks

glimpse(Algae)
dim(Algae) 
View(Algae) 

# Notice that substrate is a character field and it needs to be a factor. 
# So we convert it using the as.factor() function
Algae$Substrate <- as.factor(Algae$Substrate)
# Check it's worked....
glimpse(Algae)
# Yup - it is now a factor....
                                         
# Draw some pictures - checks for normality....
hist(Algae$Biomass, xlab = "Sample", main = "", col = "grey") # Looks okay....

# Use a boxplot to look at the data structure - look pretty different
boxplot(Biomass ~ Substrate, data = Algae, xlab = "Substrate", ylab = "Algal Biomass",
                                                 col = "grey")
                                         
# And in ggplot
ggplot(Algae,aes(x=Substrate, y=Biomass)) + 
  geom_boxplot() + labs(x = "Substrate type", x = "Algal Biomass")
                                         
# normality / heterogeneity tests
# (i) Check for normality, QQplot, Shapiro-Wilks test
qqnorm(Algae$Biomass); qqline(Algae$Biomass) # Yup looks good for normality...
shapiro.test(Algae$Biomass) # P vale = 0.6285 so test confirms normality is good

#(ii) Check for heterogeneity of variances using a levene test. NOTE there are graphical means of doing this but it's a fiddle and we'll validate things differently once we fixed these assumptions in your mind.
leveneTest(Biomass ~ Substrate, data = Algae) # P value = 0.1139 so we assume homogeneity of variance

# NOTE: There is considerable debate over normality tests 
# (see: http://www.r-bloggers.com/normality-tests-don%E2%80%99t-do-what-you-think-they-do/)
# Our tests indicate normality and homogeneity of variance for we use parametric approaches
# This is single factor ANOVA
# Run single factor ANOVA using the aov function
 
Anova.run <- aov(Biomass ~ Substrate, data = Algae)

# Check results
summary(Anova.run)

# P value is 0.00000118 (= 1.18e-06) so a highly significant different
# We can now validate the ANOVA model using R's inbuilt features (you'll recognise these from SPSS)
# This is a key final thing you need to do. YOU DO NOT WANT TO SEE ANY PATTERNS IN THESE PLOTS

# These plots are readily interpretable with practice
# Read: Zuur et al. 2007 ANALYSING ECOLOGICAL DATA pp. 63-67 
# Read Beckerman et al. 2017 Chapter 5, p.113 - See your resources list. 
par(mfrow = c(2, 2))
plot(Anova.run)

# reset graphics
dev.off()

# This is what I see in these plots:
# In brief....top left plot - residuals v Fitted. Shows no patterns
# i.e. no increase of residual spreads with fitted values
# Top right plot - Q-Q plot. All on the line so good (but you know that already - you've done it)
# Bottom left plot. Scale - Location plot. Root of the standardised residuals. It accentuates patterns in spread of residuals against fit. Again no pattern, so no problem.
# Bottom right plot - Leverage plot. Shows if there are any influential data points in the groups
# No clear patterns of excessively important datapoints so we are good

# We infer from this that is the we've applied the correct model
# Our interpretation is that algal biomass differs across different types of substrate
# We can also test for the group means that are driving the overall test result
                                                    
TukeyHSD(Anova.run) # This shows us that all the group pairs differ significantly
# Again our boxplots indicated this so it isn't too surprising.

# If either normality or variance is an issue then we use a non-parametric approach
# For more than 2 samples (in the grouping factor) we use a Kruskal Wallis test
KW.test <- kruskal.test(Biomass ~ Substrate, data = Algae)

# Look at the results
KW.test 
# P value is 0.0000205 so highly significant difference between substrate types
# NOTE - the P value is less significant than ANOVA because the test is more conservative

#********************************************************
# PART THREE: 2-way and factorial ANOVA                 *
#********************************************************
# Use the dataframe you called Limpet

# Standard filechecks
glimpse(Limpet) 
dim(Limpet) 

# We need to change the character fields to factors again
Limpet$SEASON <- as.factor(Limpet$SEASON)
Limpet$DENSITY <- as.factor(Limpet$DENSITY)

# Check it worked
View(Limpet) 

# IMPORTANT FIRST TASK IS ESTABLISHING WHETHER YOUR ANALYSIS IS BALANCED 
# THIS IS CRUCIAL WHEN USING THE aov and lm() functions   
# WHERE YOU ARE COMPARING TWO FACTORS                                   

# Check this using the table function
table(Limpet$DENSITY, Limpet$SEASON) # it's balanced 3 values in every comparison

#You can also 'test' for this using the replications function
replications(EGGS ~ DENSITY * SEASON , data = Limpet)
!is.list(replications(EGGS ~ DENSITY * SEASON , data = Limpet))
# Returns a TRUE so We're safe !!!! If it was unbalanced then it would have return FALSE

# Draw some pictures

hist(Limpet$EGGS,
     xlab = "Number of Eggs",
     col = "grey")
# Looks promising for normality

# Boxplots to look at difference of factors
boxplot(EGGS ~ DENSITY, data = Limpet,
        ylab = "Number of Eggs",
        xlab = "Density of Limpets in Quadrat",
        col = "grey")

# in ggplot
ggplot(Limpet,aes(x=as.factor(DENSITY), y=EGGS)) +
  geom_boxplot() + labs(x = "Density of Limpets", y = "Number of Eggs")

# Looks like the number of eggs decreases as the density of limpets increasesperhaps due to competition for space?
boxplot(EGGS ~ SEASON, data = Limpet,
        ylab = "Number of Eggs",
        xlab = "Season of sample",
        col = "grey")

# in ggplot
ggplot(Limpet,aes(x=SEASON, y=EGGS))+geom_boxplot()+labs(x="Season of Sample", y= "Number of Eggs")
# More eggs are produced in spring than summer

#Coplot to look at their nteraction of the two factors (with added smoother line)
coplot(EGGS ~ DENSITY | SEASON, panel = panel.smooth, 
       xlab = "Density of limpets", 
       ylab = "Number of Eggs", 
       col = "grey", data = Limpet)

# Notice how R orders the factors alphabetically on the X axis. Spring first then summer  i.e. alphabetically
# Plot shows that eggs decrease with density in both spring (left) and summer (right); but the number of eggs is higher in spring at all density levels
# We are not going to use either a shapiro-wilks or levenes test (we'll validate the model instead)

# Run the two-way ANOVA
limpetaov1 <- aov(EGGS ~ DENSITY + SEASON, data = Limpet)

# look at the results of the ANOVA
summary(limpetaov1)

# We have two factors (DENSITY and SEASON); 
# both are significant with P values < 0.001
# This confirms that the patterns we've seen in the plots are significant

# Now let's consider the interaction between the two factors. 
# The * symbols adds in the interaction effects but it will also compute the one and two way tests too.
limpetaov2 <- aov(EGGS ~ DENSITY * SEASON, data = Limpet)

# Look at the results
summary(limpetaov2)

# Notice that the P values differ from the first test because we've introduced a test for the 
# interaction of the two factors
# Notice also that the interaction DENSITY:SEASON has a P value of 0.823;
# So it isn't significant

# We can visualise the interaction plot using the interaction.plot function
interaction.plot(Limpet$DENSITY, Limpet$SEASON, Limpet$EGGS, col=c(2,3), 
      xlab = "Density of Limpets", ylab = "Number of eggs", trace.label = "Season")

# The lines for spring and summer do not cross or converge; this confirms that the factors DO NOT interact

#Let's check some assumptions using R's in-build plot functions....
par(mfrow = c(2, 2)) # plot four pictures per page
plot(limpetaov2)

# reset graphics
dev.off()

# We see no patterns in the residuals in any plot (the data points are evenly distributed);
# The Q-Q plot shows points sitting close to and on the line, so we assume normality
# no issues with influential points (bottom right plot)

# Our interpretation is that the model is good (normality of data and homogeneity of variance);
# Showing that limpets produce more eggs in spring than summer and that as limpet density increases the numbers 
# of eggs reduces.

# NOTE - if normality or homogeneity fails then there are fixes but the easiest is to linearise the response data using a log(10) function
# If you have this issue we need a chat :)

#____________________________________________
# Another example of two-way ANOVA with a significant interaction
#____________________________________________

glimpse(Graze)						# look at the structure

# Notice that the string data (cols Field and Grazing has been imported as character data 
# not as factors.Some functions will interpret the field as factors other won't. We'll convert them.
Graze$Field <- as.factor(Graze$Field)
Graze$Grazing <- as.factor(Graze$Grazing)

# Check this using the table function. 
table(Graze$Field, Graze$Grazing)

# You can also 'test' for this using the replications function
replications(Abund ~ Grazing * Field , data = Graze)
!is.list(replications(Abund ~ Grazing * Field , data = Graze)) # We're safe !!!!

# Draw some pictures - class exercise
# Boxplots to look at difference of factors
boxplot(Abund ~ Field, data = Graze)
boxplot(Abund ~ Grazing, data = Graze)

# We can also combine the factor as an interaction. NOTE the * symbol indicates the interaction
boxplot(Abund ~ Field * Grazing, ylab =  "Abundance of Rye Grass", xlab = 
        "Contrasts of field and grazing level", data = Graze)

# Coplot to look at the interaction of the factors (with added smoother line)
coplot(Abund ~ Grazing | Field, panel = panel.smooth, 
       xlab = "Grazing Level", ylab = "Rye grass abundance", data = Graze)

# Notice:
# how R orders the factors alphabetically on the X axis. Unhelpful because the (High, Low, Mid) are 
# relational to each other - indicating a known order 

# Compare the means and errors on a line plot
# To do this we need to summarise the data to get the means and standard errors
# There are numerous ways of doing this (tapply etc) but we are going to do it using a package called dplyrv- it's in the tidyverse suite of programmes

# Reorder the categorical variable. R plots factorial variables alphabetically - not useful when your factors relate to a quantity of something!!!!
Graze$Grazing <- factor(Graze$Grazing, levels = c("Low", "Mid", "High"), ordered = TRUE)

# Run the two-way ANOVA with interaction
Grazeaov <- aov(Abund ~ Field * Grazing, data = Graze)
summary(Grazeaov)

# Let's check some assumptions using R's in-build plot functions....
# these do the same thing as your tests (e.g. Levenes) and QQ-plots
# YOU WILL BE USING THIS COMMAND ALOT. PLEASE READ (COPY AND PASTE INTO YOUR BROWSER):
# https://www.oxfordscholarship.com/view/10.1093/acprof:oso/9780198787839.001.0001/acprof-9780198787839-chapter-5
# This whole book is a HUGE must read - we have an online library subscription (check the resources tab on the CANVAS page):
# Getting Started with R: An Introduction for Biologists
# Andrew Beckerman, Dylan Childs, and Owen Petchey

# We can generate the validation plots
par(mfrow = c(2, 2)) # plot four pictures per page
plot(Grazeaov)

# reset graphics
dev.off()

# Look okay apart from plot 4 which indicates a lumped pattern.

# Now let's try the interaction between the two factors to see if it improves the fit.
# We can visualise the interaction plot using the interaction.plot function
interaction.plot(Graze$Grazing, Graze$Field, Graze$Abund, col=c(2,3), 
        xlab = "Grazing Regime", ylab = "Rye Grass abundance", trace.label = "Field")

# lines cross indicating an interaction between the two factors - as shown in the p values

#•••••••••••••••••••••••••••••••••••••••••••
## PART FOUR: CLASS EXERCISES               •
#•••••••••••••••••••••••••••••••••••••••••••

# Do the following for all data files:

# 1. Load in the data and examine it's structure (including experimental balance)
# 2. Posit your hypotheses (null and alternative)
# 3. play with some pictures
# 4. draw a contingency table to figure out how the data are structured
# 5. Select an appropriate ANOVA-based model (NOTE: we have more than 2 samples...so no t tests!)
# 6. Validate the model
# 7. Briefly interpret the results - in your script file!!!!

# EXERCISE 1
# -----------------------------------------------------------------------------------------
# Filename: Hoglouse.csv - a file of water louse distribution along a rivers in Devon
# response - hoglouse numbers
# explanatory variable - Upper, Mid and lower sites (i.e.  longitudinal profile)
# see Gardner's book
# ----------------------------------------------------------------------------------------


# EXERCISE 2
# -----------------------------------------------------------------------------------------
# Filename: Medley.csv
# Medley and Clements (1998) investigated the impact of zinc contamination (and other
# heavy metals) on the diversity of diatom species in the USA Rocky Mountains (from
# Box 8.1 of Quinn and Keough (2002))
# File contents:
# DIATOM - number of different species of diatoms on the a known area of rocks in streams (continous variable)
# ZINC - mpm of zinc in the water column (background, low, medium, high) (factor - explanatory variable)
# ----------------------------------------------------------------------------------------

# EXERCISE 3
# -----------------------------------------------------------------------------------------
# Filename: Quinn.csv (data from By G. P. Quinn and M. J. Keough, 2002 - "Experimental Design and Data Analysis for Biologists" )
# File contents:
# DENSITY - urchin density treatment (L1 = 8 individuals per 225 cm2 enclosure, L2 = 15, L3 = 30 and L4 = 45) (factor - explanatory variable)
# SEASON - Season of the year (Spring or summer) (factor - explanatory variable)
# EGGS - egg production by limpets (continous response variable)
# ----------------------------------------------------------------------------------------