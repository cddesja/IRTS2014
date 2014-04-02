# ---------------------- #
# IRT workshop 30/1/2014 #
# ---------------------- #

# The pound sign is used to indicate a comment.
# You should use comments a lot! Especially if you don't use
# R very often as it will help you remember what you did last time
# and why.

# You should also use scripts too! This will save you a ton of time
# next time and equally as important they will ensure reproducibility.

# Install some important IRT libraries
# install.packages("ltm")
# install.packages("irtoys")
# install.packages("lme4")
# install.packages("plyr")
# install.packages("ggplot2")

# Load these libraries
library(ltm)
library(irtoys)
library(lme4)
library(reshape)
library(ggplot2)
library(MASS)

# List the functions in a particular package
lsf.str("package:ltm")

# To find out information about a function:
?ltm

# To search for a keyword
??item

# What data sets are available?
data()

# Which data sets belong to ltm, irtoys, and lme4?
data(package=c("lme4","ltm","irtoys"))

# ----- #
#  Data # 
# ----- #

# Examine state.x77 data sets
data(state.x77)

# Find out information about the data set
?state.x77

# Show the first few cases
head(state.x77)

# and the last few
tail(state.x77)

# If you want to see a specific observation, say #20, and you want to see responses from variables 2 through 3
state.x77[20,2:3]

# Or if you want to see variables 1, 3, and 6 for observations 19 through 21.
state.x77[19:21, c(1,3,6)]


# QUESTION: How would you show observation 14 and 26 for variables 3, 7

# Can also call them by name
state.x77["Alabama",]

# To write the data to a text file
write.table(state.x77,file = "statex77.txt")

# To read it back in
read.table(file="statex77.txt")

# But we need to save it as an object
state <- read.table(file="statex77.txt")

# Similar functions exist to read data directly from SPSS, CSV files, etc #
# Check out the foreign package #

# To summarize the data there are various commands
summary(state.x77)

# To find out the type of variables they are
str(state.x77)

# Says this is a list
is.list(state.x77)

# Convert to a data.frame
state.df <- as.data.frame(state.x77)
str(state.df)

# Examine one variable as a table
table(state.df$Life)
table(state.x77$Life)

# Data.frame is also a list
is.list(state.df)


# To call a specific variable from a data.frame you need to use the $ command
state.df$Population

# QUESTION: How would you list the contains of the Income variable? 

# Say that Alabama should have had a Population score of 3700 instead of 3615
state.df[1,]
state.df[1,1] <- 3700
state.df[1,]

# QUESTION: What if we found out that California should have a Illiteracy of 3.6. How can we change this?

# Conditional summaries
table(state.df$Population,state.df$Income)

# Not terribly useful. More useful for factors

# How many different HS Graduation are there?
table(state.df$HS) 

# Which state has the lowest HS graduation rate?
which.min(state.df$HS) 

# Which state? 
state.df[which.min(state.df$HS),]

# Notice we can nest functions

# QUESTION: Which state has the highest murder rate?

# Let's examine Income a littler more
summary(state.df$Income)

# Let's create a factor for income, that seperates low and high income depending on whether they fall below or above the median
state.df$Income.f <- ifelse(state.df$Income > median(state.df$Income), "High","Low")
state.df$Income.f <- as.factor(state.df$Income.f)
str(state.df)

# QUESTION: How would we divide the states into high and low life expectancy based on the mean?

# How many levels of factor are there
nlevels(state.df$Income.f)

# Create lowest, low, high, higher income based on quantile splits
Quants <- quantile(state.df$Income)
state.df$Income.f2 <- ifelse(state.df$Income < Quants[2],"Lowest",
		      ifelse(state.df$Income > Quants[2] & state.df$Income < Quants[3], "Low",
		      ifelse(state.df$Income > Quants[3] & state.df$Income < Quants[4], "High","Highest")))

# Remove variables created
state.df$Income.f <- NULL
state.df$Income.f2 <- NULL

# ---------- #
# Regression #
# ---------- #

# Scatterplot matrix
pairs(state.df)

# Correlation matrix
cor(state.df)

# Try to predict murder rate from Income
m0 <- lm(Murder ~ Income, data = state.df)

# Summary information
summary(m0)
anova(m0)

# This also contain information
str(summary(m0))
str(anova(m0))

# So if you just want the F-value for Gender
f <- anova(m0)$F[1]

# And the t-test from Gender
t <- summary(m0)$coef[6]

# Aren't these two related?
round(t^2) == round(f)

# You can get a lot of diagnostic information from plotting model
plot(m0)

# Box-Cox transformation
boxcox(m0)

# Plot Murder vs. Income
qplot(y =state.df$Murder, x = state.df$Income) + geom_smooth() # Looks like there is one outlier perhaps? Alaska

# Let's drop Alaska
m0a <- lm(Murder ~ Income, data = state.df[-2,])
plot(m0a)

# You can extract residuals and predicted values
residuals(m0)
# To specify the type of residuals see ?residuals
 
# Get predicted values
predict(m0)

# To get the r squared
summary(m0)$r.squared

# Another option would be to create that factor of Income again
state.df$Income.f2 <- ifelse(state.df$Income < Quants[2],"Lowest",
		      ifelse(state.df$Income > Quants[2] & state.df$Income < Quants[3], "Low",
		      ifelse(state.df$Income > Quants[3] & state.df$Income < Quants[4], "High","Highest")))


# Multiple regression
m1 <- lm(Murder ~ Income.f2 + Illiteracy, data = state.df)
summary(m1)$r.squared

# QUESTION: What kind of model is m1?

# Examine our ANCOVA
anova(m1)
summary(m1)

# Strange, none of the differences are significant. And our baseline group is high, let's make it highest
state.df$Income.f3 <- factor(state.df$Income.f2, levels = c("Highest","High","Low","Lowest"))

# Wait if we want to throw all everything in there, except id!
m2 <- lm(Murder ~ . - Income -Income.f3, data = state.df)
summary(m2)

# Want to do stepwise
stepAIC(m2)

# ----- #
# Plots #
# ----- #

# Can attach data frame
attach(state.df)

# Plot Murder vs. Life Expectancy
(g0 <- qplot(x = `Life Exp`, y = Murder) + stat_smooth(size = 2))

# Notice the cumbersome `Life Exp` ... don't use spaces in your coding

# Let's add in the Income Factor into this
(g1 <- g0 + geom_point(aes(color = Income.f3)))

# Throw in gender
(g2 <- g1 + stat_summary(aes(group=Income.f3, color =Income.f3 ), fun.y=mean, geom="line", size=1))

# Change the background colors
(g3 <- g2 + theme_bw())

# Remove the legends and change the x-label 
(g4 <- g3 + theme(legend.position="none") + xlab("Life Expectancy"))

# QUESTION: How would you change the y-label to say Murder Rate?

# Create histogram
qplot(x = `Life Exp`)

# Create a box plot
qplot(y = Murder, x = Income.f3, geom = "boxplot")

# Explore in R with another data set or explore with this one further! 
