# ************************************************************************************
# This is the source script for Workshop 3 [week 6]: Linear Regression + model validation + ANCOVA *
# ************************************************************************************
# Jon Sadler Feb 2021 modifed Feb 2024

# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)

# **************************
# Set the home directory

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# So this is basically ANOVA but using a line so all the same assumptions hold....
# But additionally we need to know if the data are linearly structured...to do this well make a picture
# Load your libraries
library(tidyverse)

# Load compensation.csv datafile from Beckerman and Petchey's book.....see readings
Growth <- read.csv(file.choose())

  # Look at the file
Growth 
# and its structure
str(Growth)
# Notice there is another character field - we need it to be a factor. Change it using the as.factor()

# [add your code in here]

#We can introduce a new way of looking at the data files using the glimpse function (from Tidyverse in the dplyr library)

library(tidyverse)
glimpse(Growth) # Fill in the correct object name.....!!!!

# now let's plot it using R base plot() function; we're interested in knowing whether
# Root biomass (Root) has an influence or correlates with Fruit production (Fruit)

plot(Growth$Root, Growth$Fruit)
# Notice we're using the filename and $ operator to indicate the variables and that Y (response variable comes last)

# A better and more intuitive way to do it uses the data = argument

plot(Fruit ~ Root, data = Growth) # This fits the model specification for aov and linear models

# so we can see that the relationship looks linear! But also that it has two components.
# Let's tidy it up (or pimp it as Beckerman and Petchey would say)

# Add some axis labels and you can vary their size using the list(cex combination of arguments)
plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1.5), # add half to the size of the default font
	ylab = list("Fruit Production",cex = 0.5)) # halves the default font

# Now let's consider the points on the graph
# ?par gives you help on the graphics parameters
# ?points specifically on the points.

plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1), # reset size to default of 1
	ylab = list("Fruit Production",cex = 1), # reset size to default of 1
	cex = 2, pch = 21, bg = "grey") # the pch argument controls the type 21 = filled circle, bg = colour.

# You can see that there are two clusters of points here relating to the treatment - so let's explore that.
# It would be instructive to give them different colours to illustrate the patterns.

# First let's create an object that tags the colours using an ifelse() function = if / else

culr <- ifelse(Growth$Grazing == "Grazed", "green", "blue")

#look at it
culr

# to add the colours just substitute the object name into the colour argument
plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1), # reset size to default of 1
	ylab = list("Fruit Production",cex = 1), # reset size to default of 1
	cex = 2, pch = 21, bg = culr) # add culr object to the bg call

# now let's finish the graphic by adding a legend
legend("topleft", legend = c("Grazed", "Ungrazed"),
	pch = 21, pt.bg = c("Green", "Red"), pt.cex = 2)

 # Repeat in ggplot2
library(ggplot2)
ggplot(Growth, aes(x=Root, y=Fruit, colour = Grazing)) + geom_point() +
  labs(title = "", x = "Root Biomass", y = "Fruit Production")
# NOTICE HOW MUXH EASIER THIS IS TO DO!

#**************************************************
# CLASS exercise: create a high quality plot      *
# Datafile: Nelson.csv                            *
#**************************************************

# First linear regression output
# use the Nelson.csv data
# This is an experiment on 9 batches of flour beetles assessing their weight loss measued in mg
# at different humidity levels ranging from 0-93% humidity
# The experiment lasted 6 days

# Load data (you should have already done this)
Flour <- read.csv(file.choose())

# Look at it
str(Flour)
glimpse(Flour)

# Draw some pictures to assess linearity - you've plotted this so you can see it is!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "blue", bg = "blue")

# boxplots for variability using the car package
# load package car
require(car)

scatterplot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle Weight loss (mg)")

# The boxplot indicate normal distribution of data - it's quite symmetrical
# There is no indication of increasing spread of measured points around the green linear regression line
# So we can assume homogeneity of variance is likely....

# checking assumptions using ggplot2
#QQ-plot for normality. HINT: remember Y axis variable is WEIGHTLOSS (sample = ) and data file = Flour

ggplot(aes(sample = WEIGHTLOSS), data = Flour) + stat_qq() +
  stat_qq_line()

# Run a linear model using the lm() function from base R
Flour.lm <- lm(WEIGHTLOSS ~ HUMIDITY, data = Flour)    # don't mix up your response and explanatory variables!

# Look at the output
summary(Flour.lm)   # refer to lecture PDF to see what the numbers mean i.e. for interpretation
anova(Flour.lm)     # lists the tests on the data of response to explanatory

#So what does this show us?
#First we see our model call usig the lm() function
#Then we have the spread of residuals lists from min (generally a -ve) to positive 
#The coefficients are listed next. The intercept estimate (~8.7) is the point that the regression line passes ths Y axis. Then we have the standard  error, a T value and a significance level Pr (note it's highly significant).
#The next line is your explanatory variable in this case HUMIDITY. The estimate this time is the slope of the line (it's a negative so is slopes down from left to right); the we have the same other elements. Note it is highly significant too.
#The next table is the ANOVA table and it shows that the covariate is highly significant using 1 df (there is only one covariate). And you have 7df of residuals. Which is 8-1.


# Let' look at the model more carefully by analysising the objects (see top left panel in RStudio)
# Or just type:
names(Flour.lm)

coef(Flour.lm) # show the intercept and slope values (or the betas)
residuals(Flour.lm)   # shows the residuals or errors for the fitted values fitted.values(Flour.lm)
# these should not show any patterns if graphed - more on this later in the session
fitted.values(Flour.lm) # shows the fitted values of y for every measured x

# recreate the plot with a regression line
# Draw some pictures to assess linearity - you've plotted this so you can see it is!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "blue", bg = "blue")
	abline(Flour.lm) # add the line using the abline() function

# Now let's use the model to do something - i.e. prediction
# First we'll calculate the confidence intervals
confint(Flour.lm)

#Now we'll predict the mean beetle weight expected at 25%, 50%, 75% and 100% humidity levels
# These were not measured so it's a prediction using the regression equation
# weightloss - -0.053 + 8.704

predict(Flour.lm, data.frame(HUMIDITY = c(25, 50, 75, 100)),  # tells it what data you want 25% etc
	interval = "prediction", se = T) # uses a prediction interval and sets standard errors to TRUE

# To complete the analysis we predict across the dataset and create a new plot
# with regression equation, r-squared and line and CIs at 95%

# Recreate your plot (version 1)
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "grey", bg = "grey", axes = F) # We've turned off the default axes so need to recreate them
# You don't need to turn them off - the predicted values and CIs will work with default values (see below)
# Add x axis and reduce axis labels a little (i.e. the number on the axis not the axis label!)
axis(1, cex.axis = 0.8)
# Add the Y axis in a similar manner using horizontal tick labels
axis(2, las = 1)
# add the regression line from the model (Flour.lm) using abline.....
abline(Flour.lm, col="black")
# add the equation
text(98,9, "WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos = 2)
# add r-square value
text(98,8.6, expression(paste(R^2 == 0.9078)), pos = 2) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(Flour$HUMIDITY), max(Flour$HUMIDITY), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(Flour.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=3, col="black") # This function add the CIs, lty = line type (dashed)
#put an L-shaped box to complete the axis
box(bty="l") # rather than a square which is the default

# Recreate your plot (version 2) - I actually prefer this version!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
     xlab = "% Humidity",
     ylab = "Beetle weight loss (mg)",
     pch = 21, col = "grey", bg = "grey") # We've left the axes on
# add the regression line from the model (Flour.lm) using abline.....
abline(Flour.lm, col="black")
# add the equation
text(98,9, "WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos = 2)
# add r-square value
text(98,8.6, expression(paste(R^2 == 0.9708)), pos = 2) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(Flour$HUMIDITY), max(Flour$HUMIDITY), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(Flour.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=3, col="black") # This function add the CIs, lty = line type (dashed)

# ggplot....
library(ggplot2)

ggplot(Flour, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") # USING LM call. You can pass the model predicts through as well but this is way more simple

# add some text
p <- ggplot(Flour, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

p + annotate("text", x = 75, y = 9, label = "WEIGHTLOSS = -0.053HUMIDITY + 8.704") +
  annotate("text", x = 56, y = 8.6, label = "R2 = 0.9975")

# NOTE: x and y relate to the scale on the plots! Very useful. x = midpoint of text.
# Now let's look at model validation
# We're going to do this visually although you can use the tests we introduced last week and earlier
# To start we'll use simulated data to do this, based on post at:
# http://stats.stackexchange.com/questions/52089/what-does-having-constant-variance-in-a-linear-regression-model-mean/52107#52107

# There is a R blogger post on this as well (not for the faint of heart!):
#http://www.r-bloggers.com/model-validation-interpreting-residual-plots/

set.seed(5) # use R's simulation tools
N  = 500 # 500 data points
b0 = 3 # set b0 or the intercept
b1 = .4 # set the slope or B1

s2 = 5 				# variance parameter
g1 = 1.5			# seed for heterogeneous variability
g2 = .015			# ditto

x        = runif(N, min=0, max=100) # create x
y_homo   = b0 + b1*x + rnorm(N, mean=0, sd=sqrt(s2            )) # groups points around the line
y_hetero = b0 + b1*x + rnorm(N, mean=0, sd=sqrt(exp(g1 + g2*x))) # increase variability in Y with measured X


# First we compare raw data
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns and allocates it to object op
plot(y_homo ~ x)
plot(y_hetero ~ x)
# Indicate a likely problem as Y increases with measured X on the right hand plot
par(op) # sets graphics back to default without shutting the graphics device (unlike dev.off())

# Now let's track that through to the models. Create the linear models:
mod.homo   = lm(y_homo~x)
mod.hetero = lm(y_hetero~x)

#look at the plots
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns
plot(mod.homo$resid ~ mod.homo$fitted.values, main = "Homoscedastic")
plot(mod.hetero$resid ~ mod.hetero$fitted.values, main = "Heteroscedastic")
par(op)
# The we can clearly see a wedge shape - left to right on the right hand plot

# And the same with a Scale-Location plot
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns
plot(rstandard(mod.homo) ~ mod.homo$fitted.values, main = "Homoscedastic")
plot(rstandard(mod.hetero) ~ mod.hetero$fitted.values, main = "Heteroscedastic")
par(op)

# Let's repeat this with a field dataset......
# Load a new dataset mussel.csv
# data are derived from Peake and Quinn (1993) and analyse in Quinn and Keough 2002 and Logan 2010
# The study investigated abundance-area effects for invertebrates living in mussel beds in intertidal areas
# 25 mussel beds
# respone = number of invertebrates (INDIV)
# Explanatory = the area of each clump (AREA)
# additional possible response - Species richness of invertebrates (SPECIES)
# Logan models abundance but we're going to look at species richness

# load data file
Mussel <- read.csv(file.choose())

# Look at it
str(Mussel)
head(Mussel)

scatterplot(SPECIES ~ AREA, data = Mussel)
# This indicates that the data are not normally distributed (especially AREA)
# The species richness data don't look too good either. Peaked in the middle.

# Let's fit a linear model nonetheless
mussel.lm <- lm(SPECIES ~ AREA, data = Mussel)

summary(mussel.lm)

# Now check assumption by using R's inbuilt model validation plot defaults
# set graphics parameters because we want all the plots on one graphic
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images and allocates it to object op
# Plot the diagnostics
plot(mussel.lm)
par(op)    # turns graphics device back to default of one plot per page!

# Residuals v Fitted indicate a problem. It's wedge shaped and humped!
# qqplot is a bit dodgy but might is okay
# Scale-Location plot is variable
# Cook distance / leverage looks okay - no massive outliers (ie. cooks distances >1). But is it very clumped

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area",
	ylab = "Residuals")
# This indicates a few large values and a slight wedge due to numerous small patches

# So what do we do?
# We can linearise the variables by transforming them and re-run the model
# I am not a fan of this - we'll look at other approaches next week!

mussel.lm1 <- lm(SPECIES ~ log10(AREA), data = Mussel)
# notice I chose not to log the response as it looked okay in the qqplot
# check the results
summary(mussel.lm1)  # look at the differences between this and unvalidated model in terms of R-square etc

# validate the model
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
plot(mussel.lm1)
par(op)

# These look okay…..so we accept the model and should tabulate the results
library(ggfortify)
autoplot(mussel.lm1) # same validation plot....

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm1$resid ~ log10(Mussel$AREA),
	xlab = "Log10 of Area",
	ylab = "Model residuals")

# And create the plot....some homework for you..! Quicker to use ggplot - see code above!

# The are SIX in built validation plots
# You can call these individually using the 'which' argument within plot(my model object name)
# So here they are all six in one plot
op <- par(mfrow = c(2, 3))  # this gives us a 2 x 3 panel with six images...
plot(mussel.lm1, which = 1) 	# residual v fitted
plot(mussel.lm1, which = 2) 	# Normal Q-Qplot
plot(mussel.lm1, which = 3)		# Scale-Location plot
plot(mussel.lm1, which = 4)		# Cook's Distance by observation (sample number). Some texts state D of >1 are an issue but we need to be more conservative
# so we can use 4/(N−k−1), where N is the number of observations and k the number of explanatory variables
plot(mussel.lm1, which = 5)		# Residuals v Leverage (with Cook's contours)
plot(mussel.lm1, which = 6)		# Cooks v leverage (not easy to interpret so seldom used)
# set graphics to default
par(op)

#************************************************
# PART THREE: Class Exercises                   *
#************************************************
#1. Datafile = old faithful. It is in the base system already as a dataframe called "faithful". Type str(faithful) to see its structure.
# There are two observation variables in the dataset.
# The first one, called eruptions, is the duration of the geyser eruptions (mins).
# The second one, called waiting, is the length of waiting period until the next eruption (mins).
# Response = eruptions
# Explanatory = waiting
# 272 rows v 2 columns see dim(faithful)

# 2. Use the mussel data (mussel.csv) but with response variable as abundance (Mussel$INDIV)
# not species richness (Mussel$SPECIES)

#************************************************
# TASKS - repeat on all datasets:
# 1. Create plots to assess the assumptions:
#	(a) On the raw data;
#	(b) Run the model;
#	(c) Validate it.
# 2. Create a final model plot with predicted data, 95% CIs, regression equation, R-squared value;
# 3. Interpret the model output.
#************************************************
