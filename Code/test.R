# Toy data setup 
set.seed(63)
d <- rnorm(100)
hist(d)       # works as expected
p1 <- hist(d) # saves as a "List of 6" with class "histogram"
p1; print(p1) # both print all data in the list but no histogram plot
plot(p1)      # shows histogram plot
#this is my comment