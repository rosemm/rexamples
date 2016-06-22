# GETTING YOUR DATA INTO R
# you'll use the read.spss function, which is in the foreign package, so you need to install and open that package first.
install.packages("foreign")
library(foreign)

data <- read.spss("your_file_name.sav", to.data.frame = TRUE) # this reads in your spss datafile and saves it as an R object called "data"

str(data) # a summary of the variables in data. it tells you what type each variable is as well (num means numeric, factor means categorical, etc.)
View(data) # look at it as a spreadsheet
summary(data) # some nice summary statistics about the variables in your dataset


# EXAMPLES
# this is a dataset that comes automatically with R, so I can demonstrate some stuff on it for you:
str(iris) # a built-in dataframe about a bunch of flowers...
View(iris) # open it as a spreadsheet
summary(iris)
summary(iris$Species) # you can also get a summary of just one variable at a time. for factors, it tells you how many observations you have in each level.

# EX1: make a string variable into a factor
# first I'll make Species into a string variable, so we can change it back
iris$Species <- as.character(iris$Species) # use $ to point to one column within a dataframe
str(iris) # see how it's not a factor anymore?
iris$Species <- as.factor(iris$Species) # change it into a categorical variable (factor)
str(iris) # all better :)

# EX2: change the order of levels of a factor
# first I'll add a messy factor, then we can clean it
iris$Lickert <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree")
str(iris)
iris$Lickert <- as.factor(iris$Lickert) # make it a factor
str(iris)
levels(iris$Lickert) # prints the levels of the factor, in order. they're alphabetical. :(
iris$Lickert <- factor(iris$Lickert, levels=c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"))
str(iris)
levels(iris$Lickert) # yay!


data_new <- select() %>% 
  mutate_each(funs(as.character)) %>% 
  mutate_each(funs(as.numeric))

trauma_sxs$trauma_type_collapse <- ifelse(as.character(trauma_sxs$trauma_type) == "physsex", "physsexemo", as.character(trauma_sxs$trauma_type))
trauma_sxs$trauma_type_collapse <- as.factor(trauma_sxs$trauma_type_collapse)

hist(data_full$distress)
