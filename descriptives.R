# cross tabs
table(data$var1, data$var2) # example code (fill in your actual dataframe name and variable names)
xtabs(~ var1 + var2, data=data) # another function that does exactly the same thing, just with slightly nicer formatting
?xtabs # the help documentation

# example dataset that has some factors
View(mtcars)
?mtcars
xtab <- xtabs(~ cyl + carb, data=mtcars) # this does exactly the same thing as table(), it just formats the output slightly better
xtab # print it, so you can see the table :)
summary(xtab) # to get the chi-squared test of independence for the above cross tabs


# for plotting in particular, we'll want to make sure that the variable types are all correct
str(mtcars) # we should make some of these into factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)


# descriptives
library(psych)
describe(mtcars)
?describeBy
describeBy(mtcars, group=mtcars$cyl) # gives you the descriptive from describe(), but breaks it down by levels of a factor! :)

install.packages("ggplot2") # only need to run this one time, then you can just use library(ggplot2) to load it
library(ggplot2)
# an example of a plot that shows the relationship between 3 variables
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + # this line sets up which variables will be used how
  geom_point() # this line tells it what plot to actually draw (point means scatterplot)
p # since we saved the plot as a variable named p, you can just enter p to display it
# save that plot in your working directory, so you can look at it without having to open R (and you can send to your advisor, etc.)
getwd() # what's your working directory? this is where the plot will save
ggsave("plot_filename.pdf", p, width=8, height=8, units="in") # save it as a pdf

# visualizing a relationship between 4 variables
p_facet <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + # this line sets up which variables will be used how
  geom_point() + # this line tells it what plot to actually draw (point means scatterplot)
  facet_wrap(~ carb) # this tells it to make a new plot for each level of the factor carb
p_facet # to print the plot you just saved
ggsave("plot_filename_facet_by_carb.pdf", p, width=8, height=8, units="in") # save it as a pdf

