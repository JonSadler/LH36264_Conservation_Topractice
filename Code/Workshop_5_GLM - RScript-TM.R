
# ******************************************************************************
# This is the source script for WEEK 7: Generalised Linear Modelling           *
# ******************************************************************************
# Tom Matthews and Jon Sadler Feb 2024


#load / install and load the necessary R packages
library(dplyr)
library(car)
library(MuMIn)
library(performance)

# Create a directory on your desktop called 'RWork' (or whatever you want) 
# and download the data and script files from the Canvas website)

# **************************
# Setting the home directory
setwd("~/Desktop/RWork")         # For Apple Macs
setwd("C:\\Users\\Tom\\Desktop\\Rwork")          # For Windows machines

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then 
# "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# PART ONE: Poisson Regression
# We'll start with an example of poisson regression using
# multiple predictors that is not overdispersed!

# Load data file gotelli.csv Gotelli and Everson (2002)
# investigated the biographical determinants of ant species
# richness at a regional scale. We're going to replicate their
# poisson regression of ant species richness against latitude,
# elevation and habitat

gotelli <- read.csv(file.choose())

# Look at the structure  - you know the commands by now......
glimpse(gotelli)
head(gotelli)

######################################################
###EXPLORATORY MODELLING##################################
#####################################################

#Here we are just going to focus on the GLM modelling and
#validation but don't forget the exploratory modelling / analyses
#that you covered with Jon in previous sessions. For your
#assignment, you will want to have a look at that as a first
#step.

##########################################################

# fit a Poisson GLM
gotelli.glm <- glm(Srich ~ Habitat + Latitude + Elevation, 
                   family = poisson, data  = gotelli)

# Check for multicollinearity VIFs
vif(gotelli.glm)

# Check for influential data points and outliers
# first we'll use influence measures to do it....
influence.measures(gotelli.glm)   # There are a couple of large cook values but they are not near 1!

# graphically plot the Cooks distances (remember, we want them to be < 1)
plot(gotelli.glm, which = 4)  # few biggies but not too worrying

#check for over dispersion - recall we are looking for values
#around 1 (i.e. certainly not over 2 nor under 0.5)
check_overdispersion(gotelli.glm)
# Rule of thumb here is that it needs to be around 1!
# So we see it's okay not over dispersed

#Lets look at the results
summary(gotelli.glm) 
#We can infer from this that ant species richness is greater in
#forest habitats than bogs (see 2nd line) and that it decreases
#with increasing latitude (i.e. closer to the poles) and with
#increasing elevation

######HOW TO INTERPRET POISSON GLM COEFFICIENTS
#Remember, in a Poisson GLM the actual relationship is
#non-linear, and thus coefficients have different meaning to a
#standard (Gaussian) linear regression. In the latter, the
#coefficient of a variable indicates how much the response
#variable increases with a increase of one unit in the predictor.
#It is thus 'additive', e.g. in the model y ~ x, if the
#coefficient of x is 5, and the value of y at x = 1 is 10, to
#work out the value of y at x = 2 (i.e. an increase in x of 1) we
#ADD the coefficient: 10 + 5 = 15. A Poisson coefficient is
#multiplicative rather than additive and we also have to take the
#exponential - exp() - of it. So using the same example but for a
#Poisson model, to find how much the response changes following a
#one unit increase in the predictor, we multiply the response
#variable by the exp(coefficient): 10 * exp(5) = 1484.132

# Estimate Pseudo R2 (here, Nagelkerke's R2) value
#remember these range from 0 (no fit) to 1 (perfect fit)
r2(gotelli.glm)

# We can have a go at comparing different models and model averaging using MuMIn
options(na.action=na.fail) # set options in Base R concerning missing values

summary(model.avg(dredge(gotelli.glm), fit = TRUE, subset = TRUE))

#Best model (model with highest weight and lowest AICc value) is
#our full model including latitude, elevation and habitat.

options(na.action = "na.omit") # reset base R options

############################################
# Check diagnostic plots
#######################################################
par(mfrow = c(2, 2))
plot(gotelli.glm) # looking good

#in top left, it is using the deviance residuals, as we expect
#the spread of the standard residuals to increase with increased
#predicted values in a Poisson model. The residuals vs fitted
#plot should have a horizontal straightish red line and no
#patterns in the residuals (e.g. no fan or wedge shapes).

#the QQ plot should only be roughly normal - small deviations for
#Poisson GLMs are not unexpected

#The scale-location plot should have a roughly horizontal red line

#####################################################################
######You can make simple plots of the model fit####
###########################################################

#Note the non-linear lines (this is a Poisson GLM)
dev.off()
xs <- seq(40, 45, l = 1000)
plot(Srich ~ Latitude, data = gotelli, xlab = "Latitude", ylab = "Ant Species Richness")
# Plot the points and predicted trends
points(Srich ~ Latitude, data = gotelli, subset = Habitat == "Forest", pch = 16)
pred <- predict(gotelli.glm, type = "response", se = TRUE, newdata = data.frame(Latitude = xs, Habitat = "Forest", Elevation = mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
points(Srich ~ Latitude, data = gotelli, subset = Habitat == "Bog", pch = 21)
pred <- predict(gotelli.glm, type = "response", se = TRUE, newdata = data.frame(Latitude = xs, Habitat = "Bog", Elevation = mean(gotelli$Elevation)))
lines(pred$fit ~ xs)
legend("topright", legend = c("Forest", "Bog"), pch = c(16, 21), title = "Habitat", bty = "n")
box(bty = "l")

##################################################################################
# PART TWO: over dispersion and negative binomial models 
# Now what do we do if the model is overdispersed? We are going
# to use a dataset on amphibian roadkills (from Zuur et al.
# 2009). It has 17 explanatory variables. We are going to use
# nine of them and the response variable is TOT.N (the total
# number of kills).
# The dataset is called RoadKills.csv
# Load the data and do the normal run of look sees - i.e. exploration 
# - call the file Road
###################################################################################

Road <- read.csv(file.choose())

# Plot......
plot(Road$D.PARK, Road$TOT.N,xlab="Distance to park",
     ylab="Road kills")

#try a standard Poisson GLM
Road.glm1 <- glm(TOT.N~D.PARK,family=poisson,data=Road)
summary(Road.glm1)

#plot distance to park relationship and model fit
G <- predict(Road.glm1, type="response") 
lines(Road$D.PARK, G, lty=1)

#check overdispersion - quite high!!
check_overdispersion(Road.glm1)

#perhaps it is because of an unspecified model and missing predictors,
#so lets add some others
Road.glm2 <- glm(TOT.N ~ OPEN.L + MONT.S + POLIC +
         SHRUB + WAT.RES + L.WAT.C + L.P.ROAD +
         D.WAT.COUR + D.PARK, family=poisson, data=Road)
summary(Road.glm2)

# Check for collinearity using VIFs
vif(Road.glm2)			# Looks okay...... 

# check again for over dispersion 
check_overdispersion(Road.glm2)
# This still doesn't look good....way over 1!

# Check diagnostic plots
par(mfrow = c(2, 2))
plot(Road.glm1) # There is a wedge shape in the residuals vs fitted and some high values....
	# So we have some problems

# if we ignore the over dispersion we get a lot of significant
# covariates.... So how do we deal with it. Adding additional
# covariates didn't work....

##There are a couple of different error structures we could look
#at: quasi-poisson and negative binomial. In this module, we will
#just focus on the latter

# a negative binomial model - note that it needs a different library
library(MASS)
Road.nb1 <- glm.nb(TOT.N ~ OPEN.L + MONT.S + POLIC +
         SHRUB + WAT.RES + L.WAT.C + L.P.ROAD +
         D.WAT.COUR + D.PARK, link = "log", data=Road)
summary(Road.nb1)

# We have a number of non-significant terms so start model selection
# We have an AIC because we used a log link function so MuMIn should work
options(na.action=na.fail) # set options in Base R concerning missing values
summary(model.avg(dredge(Road.nb1), fit = TRUE, subset = TRUE))
options(na.action = "na.omit") # reset base R options

# our best model includes variables 1,3,4,6 (the variables these numbers relate
# to are found under the first table in the Term codes section) refit the
# model....
Road.nb2 <- glm.nb(TOT.N ~ D.PARK + L.P.ROAD + L.WAT.C + OPEN.L, link = "log", data=Road)
summary(Road.nb2)

#check pseudo R2
r2(Road.nb2)

# Check diagnostic plots
par(mfrow = c(2, 2))
plot(Road.nb2) 

#Still not perfect but good enough and better validation plots
#than with our Poisson model, so this would be a sensible model
#choice for this dataset

#Note- you DONT need to check the overdispersion of the
#negative binomial models


##NOW HAVE A GO WITH YOUR OWN DATA!