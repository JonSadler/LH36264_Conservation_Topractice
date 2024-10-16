# Class exercise - introduction to R and Rstudio

# load in compensation dataset
compensation<- read.csv("compensation.csv", header = TRUE)
# These data differ from the 'Gradient' data; here you have three columns
# but only one of the them 'Grazing' is a character variable. The other two: 'Fruit' and 'Root' are numeric
# the compensation data are derived from a study looking at the production of fruit (apples, kg) 
# on rootstocks of different widths (mm; the tops are grafted onto rootstocks). 
# Furthermore, some trees are in parts of the orchard that allow grazing by cattle, 
# and others are in parts free from grazing. Grazing may reduce the amount of grass, 
# which might compete with the apple trees for resources.

# Remember to make the character variable a factor if it already isn't. Check using str()

#look at the datafile
str(compensation)
# we have two numeric varaibles and one factor.
# Im my case it imported as a factor character fields so no need to change one using the as.factor() function.
# If is a character in your case them the following code will make it a factor variable
compensation$Grazing <- as.factor(compensation$Grazing)
# the as.factor() function does the work

# check normality using histograms
hist(compensation$Root) # looks good
hist(compensation$Fruit) # looks good

# Confirm patterns using boxplot
boxplot(Fruit~Grazing, data=compensation, col="powderblue", xlab="Cattle grazing treatment", ylab="Apple weight kgs")
boxplot(Fruit~Grazing, data=compensation, col="powderblue",xlab="Cattle grazing treatment", ylab="Apple tree root mass kgs")

# Repeat using ggplot
ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) + geom_boxplot() + xlab("Cattle grazing treatment") + ylab("Apple weight kgs") 
ggplot(compensation, aes(x=Grazing,y=Root,fill=Grazing)) + geom_boxplot() + xlab("Cattle grazing treatment") + ylab("Apple weight kgs")
# We cannot use facetwrap to compare these because we only have one factor, but we can use 
# the ggpubr package

#install the new package
install.packages("ggpubr")

#open package
library(ggpubr)

# create two boxplot plots A and B and one X-Y plot
A <- ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) + geom_boxplot() + xlab("Cattle grazing treatment") + ylab("Apple weight kgs") 
B <- ggplot(compensation, aes(x=Grazing,y=Root,fill=Grazing)) + geom_boxplot() + xlab("Cattle grazing treatment") + ylab("Apple weight kgs") 

# arrange them on one page
ggarrange(A, B, labels = c("A", "B"), ncol = 2, nrow = 1)
#ggarrange is a function in the ggpubr package that allows you to arrange the figures to make one composite figure
#Just pull in the figure objects, this case A and B,
# Add the labels using the labels command and
# tell ggarange how you want the columns, row arranged (ncol = nuber of columns, nrow=number of rows)


# We also might want to display the X/Y relationship against the grazing treatment, so we create another plot 
C <- ggplot(compensation, aes(x=Root,y=Fruit, colour=Grazing)) + geom_point() +xlab("Root biomass") + ylab("Fruit biomass")
C #Look at the plot

# plot all three figures
ggarrange(A, B, C, labels = c("A", "B", "C"), ncol = 2, nrow = 2)


