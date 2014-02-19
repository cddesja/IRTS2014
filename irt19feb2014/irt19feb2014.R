# MIRT - Multidimensional Item Response Theory package

# Install the package
install.package("mirt")

# Load the package
library("mirt")

# Let's use the LSAT7 data
# The Law School Admissions Test, Section
data(LSAT7)
head(LSAT7)

# We notice that there are 5 items from the LSAT7 and the final column corresponds to the number of times 
# this pattern was observed. These items are dichotomously scored. There should be a total of 32 patterns. 
# 2^5.

# Always good to look at the data 
summary(LSAT7)

# The main function we'll be using is mirt()
# mirt() can do everything ... literally ...
?mirt

# The main argument we are concerned with is itemtype
# The other arguments matter too but will be used on a case by case manner

# First, we need to expand this data set so that it's represented of all the subjects 
# taking the test
lsat.data <- expand.table(LSAT7)
head(lsat.data)
dim(lsat.data) # 1000 subjects
sum(LSAT7$freq) # should match the sum of the frequency

# Let's first fit a Rasch model to the data
rasch.mirt <- mirt(lsat.data, 1,itemtype = "Rasch") 

# Let's look at the estimated parameters

# a1, corresponds to the item discrimation
# d, corresponds to the item difficulty
# g, is the guessing parameter
# u, is the sqrt of the factor uniqueness
coef(rasch.mirt)

rasch.mirt
# The provides various fit output measures



# Let's fit a 2-PL model
mirt.2pl <- mirt(lsat.data,1,itemtype = "2PL")
coef(mirt.2pl)

# Finally, let's fit a 3-PL
mirt.3pl <- mirt(lsat.data,1,itemtype = "3PL")
coef(mirt.3pl)

# But was it appropriate to run unidimensional IRT?
oneF <- mirt(lsat.data,1)
twoF <- mirt(lsat.data,2)

summary(oneF)
summary(twoF)


# You can rotate the structure to make is more understandable and suppress loadings
# By default you see it does an oblimin rotation, which is an oblique rotation
# varimax is a common orthogonal rotation
# There are a whole slew of rotations available
# ?GPArotation
# We'll also suppress the loadings that are 0.25

summary(twoF,rotate="varimax", suppress = 0.25) 

## Notice the factor correlation below

# If you want empirically compare the two models
# H_0 is that fit is the same
# reject H_0 means that we favor the more complex model 

anova(oneF,twoF)

# Given this, is it appropriate to even use the one factor model and unidimensional IRT?

# Recall that one of the assumptions is local independence
# i.e. two items are only related to one another through the factor
# and any residual errors should be uncorrelated.
# This can be formally examined

residuals(oneF)

# This prints a local dependence pairwise statistic between the items
# This statistic has a chi-square distribution with 1 df under H_0.
# Formally, extreme values larger than ...

qchisq(.95,df=1)

# Indicate violations of local independence
# This is along the bottom of the triangle

# Standardized Cramer's V, similar to correlations 
# are above the triangle
# This is again evidence that a one-factor model may be in sufficient

# Ability estimates

# Defaults to EAP
# This is expected a posteriori, or Bayes mean estimate
# This is takes the mean of the posterior distribution of the person ability estimates
fscores(rasch.mirt)

# MAP scores
# This is maximum a posteriori, or Bayes modal estimate
# This is takes the mode of the posterior distribution of the person ability estimates
fscores(rasch.mirt, method = "MAP")
# Empircial reliability is the ratio of the variance of the MAP (EAP) thetas to the
# sum of the variance of the thetas and error variance


# The prior distribution is a multivariate normal distribution with a mean vector of 0s
# and an identity matrix for the covariance matrix.
# The mean and covariances can be specified, but it doesn't appear
# as though you are able to change the actual distribution.

# These two estimates are affected by the choice of the prior distribution on the person abilities
# Therefore, if the prior distribution doesn't reflect reality, then these estimates will be biased


# Finally, ML scores
# These are maximum likelihood estimates and are based solely on the data, i.e. the likelihood
fscores(rasch.mirt, method = "ML")
# There is a problem here ...

# Can also set quadrature points here, but again how many?

# Let's see how well these models relate to one another
rs <- as.data.frame(fscores(rasch.mirt, method = "MAP"))
s2pl <- as.data.frame(fscores(mirt.2pl, method = "MAP"))
s3pl <- as.data.frame(fscores(mirt.3pl, method = "MAP"))
scores <- data.frame(rs$F1,s2pl$F1,s3pl$F1)
cor(scores)


#########
# Plots #
#########

# To plot the test information
plot(rasch.mirt)

# To plot the item response functions
plot(rasch.mirt,type = "trace")

# To plot just the irf for item 1
plot(rasch.mirt,type = "trace",which.items= 1)

# To plot the item information functions
plot(rasch.mirt,type = "infotrace", facet_items = TRUE)

# Remove the facet
plot(rasch.mirt,type = "infotrace")

# To plot just the irf for item 1,2 & 5
plot(rasch.mirt,type = "infotrace",which.items= c(1:2,5))

# test response function, i.e. the expected total score
plot(rasch.mirt,type="score")

#######################
# Comparing model fit #
#######################

# First compare the rasch to the 2PL
anova(rasch.mirt,mirt.2pl)

# Compare the 2PL to the 3PL
anova(mirt.2pl,mirt.3pl) 

# There is no improvement in fit
# With these test also notice there 
# are various fit criteria such as AIC and BIC

# We can plot the 2PL as we did with the rasch
plot(mirt.2pl)
plot(mirt.2pl,type = "trace") 
# From this plot we can see that items 4 and 5 have the lowest discrimation
# and item 3 has the highest

# Let's examine information
plot(mirt.2pl,type = "infotrace") 
# How does this relate to discrimation, a?

# Interactive shiny plot
install.packages("shiny")
library(shiny)
itemplot(shiny = TRUE)
 
