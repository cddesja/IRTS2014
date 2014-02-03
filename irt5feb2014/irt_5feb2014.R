library(irtoys)

###data set of 75 items, scored dichotomously for 1000 individuals
person.resp <- read.csv(file.choose(),header=T)

# Omit the first column because it's really just an ID variable and will
# give the model a headache
person.resp <- person.resp[,-1]

# Make sure the responses are "matrix" in form, not "dataframe":
person.resp <- as.matrix(person.resp)

?est

## We will do this a different way as well but it will be the same thing.

# The irtoys function to estimate item parameters: est (seems obvious)

# Here are some of the arguments of the est command:
#   1) The first command tells R which response matrix contains the (0,1)
#      responses where persons go down the rows and items go across the columns.
#   2) Next, we tell R that we want to assume the responses came from a 1PL,
#   3) We specify WHO (program or function) is doing the estimation:
#       In this case, we're using "ltm," built into R!
#   4) We need to tell R the number of quadrature points, to integrate out the
#      ability! This is really something that you can usually just leave at the default value.
#   5) Finally, R needs to know whether to force a = 1 (rasch = T) or let
#

###setting up the 1PL model  
Try.1PL <- est(person.resp, model="1PL", engine="ltm")
Try.1PL

##rows are per item, the columns are are a, b, and c, respectively
## column 1 then is the item discrimination, notice this is the same for all the parameters
## column 2 is the item difficulty, notice this changes for each item
## column 3 is the guessing parameter, notice this is set to zero 

# Plot 3 items from this data set
MAX <- which.max(Try.1PL$est[,2])
MIN <- which.min(Try.1PL$est[,2])
CloseZero <- which.min(abs(0 - Try.1PL$est[,2]))

## Let's plot the item reponse functions
plot(irf(Try.1PL$est[c(MAX,MIN,CloseZero),], x= seq(from=-10,to=10,by=.1)),co=NA)

## Question: Which item (color) is the easist item? Which item (color) is the most difficult item?
     
## Question: What do we expect the parameter a to equal if we set the argument rasch = TRUE model?

## Question: Are these lines parallel?

## Question: What is going with x?

## Question: How would I make this the Rasch model?

## Question: Assume you know the item parameters (from calibration). How would you plot an item with a = 1, b = 0, c = 0?

############
# Item Fit #
############

# First, we want to check the fit of the 1PL (ltm) parameters:
# To make everything uniform, we will change the name of the params! :)
ltm.params <- Try.1PL

# We're going to calculate fit statistics from a for loop!!!
# We will pull out an item, calculate fit on that item, store it ...

# Though, we need to set up the matrix to store the items:
print(n.it <- ncol(person.resp))

# We will set up a blank matrix:
#   1) The rows will store each item,
#   2) The column will store each of the fit statistics for the items,
#   3) stat (the statistic!), dfr (degrees of freedom), pval (ummm ... p-value?)
item.fit <- matrix(0, nrow = n.it, ncol = 3)

# We want to repeat this for EACH item:
for (i in 1:n.it){

# i is our item index, telling R which item to pull out:
item <- i

# Now, we can call "item" instead of "i" (to make things easier to read)

# For that item, we want to calculate fit statistics:
#   1) Plug in the (0, 1) response matrix
#   2) Plug in the estimated parameters (now, we're trying the 1PL stuff)
#   3) Tell R which item for which to calculate statistics (i or item)
#   4) Say what type of fit statistic (we'll do "likelihood ratio")
#   5) Finally, tell R whether to plot the items and statistics (or not):
fit.temp <- itf(resp = person.resp, ip = ltm.params$est, item, stat = "lr",
                do.plot = FALSE, main = paste("Item Fit: Item", item))

# Pull out the "lr" statistic and put it into the 1st column of the ith variable
# Pull out the "df" and put it into the 2nd column of the ith variable
# Pull out the "pval" and put it into the 3rd column of the ith variable
item.fit[i, 1] <- fit.temp[1]
item.fit[i, 2] <- fit.temp[2]
item.fit[i, 3] <- fit.temp[3]
}

# So, now we have a matrix of 75 rows (one for each item),
#     and 3 columns (one for each statistic), and maybe a couple warnings :)!

# Next, let's look at the how well "ltm" did with the 1PL:
# First, round to make things easier to see!
ltm.item.fit <- round(item.fit, 3)

# What was the mean Likelihood Ratio Statistic?
mean(ltm.item.fit[ , 1])

# What was the maximum Likelihood Ratio Statistic?
max(ltm.item.fit[ , 1])

# Which item HAD the maximum Likelihood Ratio Stat (Worse Fitting Item!!!)
which.max(ltm.item.fit[ , 1])

# Plot That Item!
itemH <- which.max(ltm.item.fit[,1])
itf(person.resp, ltm.params$est, itemH, stat = "lr",
    do.plot = TRUE, main = paste("Item Fit: Item", itemH))
# Oh how ugly!!!

# Which item HAD the minimum Likelihood Ratio Stat (Worse Fitting Item!!!)
which.min(ltm.item.fit[ , 1])

# Plot That Item!
itemL <- which.min(ltm.item.fit[,1])
itf(person.resp, ltm.params$est, itemL, stat = "lr",
    do.plot = TRUE, main = paste("Item Fit: Item", itemL))
## Much nicer!


# Plot them side by side
par(mfrow=c(1,2))
itf(person.resp, ltm.params$est, itemH, stat = "lr",
    do.plot = TRUE, main = paste("Item Fit: Item", itemH))
itf(person.resp, ltm.params$est, itemL, stat = "lr",
    do.plot = TRUE, main = paste("Item Fit: Item", itemL))

# Remember: High LR -- Line Not Fitting Probabilities
#           Low LR -- Line Pretty Good Est of Probabilities
#           Pretty Sensitive to Slight Deviations from Fit!

# Activity: Chose any item to investigate. Does it fit based on the likelihood ratio test? How does the line fit
# the probabilities?

# Remember the definitions of Information in Estimation:
#	 For Everything: expected negative second derivative of log likelihood
#	 For IRT, this becomes: (pprime)^2/(pq)

# The "expected negative second ... " just means the "curvature" of the
# likelihood function!

# Now, we want to work with the function "iif" in irtoys!
# iif takes two (and only two) arguments:
#	1) The item parameters in "matrix" form ... or
#	1) AN item parameter as a "vector"
#	2) A sequence of theta values on which to calculate information ... or
#	2) Nothing, and irtoys will pick the theta values: 99 between [-4, 4]


params.iif <- iif(ltm.params$est)

# Let's see what this object gives us:
names(params.iif)

# There are two drawers (sub-objects):
#	1) x: the thetas at which R evaluated information
#	2) f: the actual information values!

# However, staring at information is mindnumbing ... (as usual)
# But, we don't need to stare ... R can actually plot this object!  Neat!               

plot(params.iif)  
## These are all the information functions! 

# For item 1 
plot(y=params.iif$f[,1],x=params.iif$x, type = "l")

# QUESTION: What does this tell you about the item difficulty (i.e where is it located?)?      
ltm.params$est[1,2]

# QUESTION: Plot the 50th item. Where is the information at a maximum?

#############################
# Item and Test Information #
#############################

# Remember, Item Info is Summative: TIF = sum(IIF)

# Let's find information for a short test!
# First, let's choose the items that we want to be apart of the test:
test.items <- c(10, 30, 35, 40, 45, 50, 70)

# Above are numbers corresponding to the "item numbers", not the items themselves!

# Now, let's take those items out of the "item.params" object:
test.params <- ltm.params$est[test.items, ]

# So we have a combination function:
#	1) "test.items" is a vector of numbers corresponding to item numbers on the test
#	2) [test.items, ] takes ROWS out of item.params corresponding to item numbers
#		So: pulling out the 10th row, the 30th row, ...
#	3) [test.items, ] keeps ALL of the columns of item.params
#		So: pulling out the 10th row (all columns), the 30th row (all columns), ...
#	4) We're taking the items we want, keeping all the parameters of those items, and
#		sending the whole thing to "test.params"

nrow(test.params)

# test.params contains all of the items we wanted!  All seven of them!

# Let's pick a few thetas to run:
thet <- c(-3, -2, -1, 0, 1, 2, 3)

# Next, let's run the function "iif" with a matrix to see what happens:
info.20 <- iif(test.params, thet)

# What is contained in the object?
names(info.20)

# What does the object look like?
info.20

# So, it's the same thing as before, only "x" has changed (to OUR thet), and
# NOW we have a column corresponding to EVERY item in our test!

# Let's use the "apply" command to sum all of the info:
#	1) apply(object, margin, function)
#	2) So, we stick in our matrix of information: info.20$f
#		(we're summing "f", NOT "x", so we don't have to worry about "x")
#	3) Rows are thetas, columns are items, so we want to sum ACROSS the rows
#	4) Our function is "sum", which is a "vector-function"

tinfo.20a <- apply(info.20$f, 1, sum)

# Try the whole thing using the "tif" command instead of the "iif" command:
# (the "tif" is the SAME as "iif", only it finds "test information")

# So, include the SAME parameters we used AND the thetas at which we evaluated:
tinfo.20b <- tif(test.params, thet)

# Let's see if they're the same:
tinfo.20a
tinfo.20b$f

# We can also plot test information in EXACTLY the same way as item information,
# only with a "test" of responses rather than an item of responses:
plot(tif(test.params))

# QUESTION: How does the test look?  Is it a good test?  Is it a bad test?

# Compare the "short test" (of 7 items) with the ENTIRE test (of 75 items)
plot(tif(ltm.params$est))

# QUESTION: Why is the TIF located around the values they are?

####################
# Person Estimates #
####################

# There are 3 estimation methods
# The expected a posteriori estimate 
# Get estimates of person abilities
th.eap <- eap(person.resp,ltm.params$est, qu = normal.qu())

# Examine first few cases
head(th.eap)
# Est corresponds to their estimate person ability
# SEM is the standard error of measurement
# n is the number of non-missing responses

# Getting the estimate using marginal maximum likelihood
th.mle <- mlebme(person.resp,ltm.params$est)
head(th.mle)

# Can also get the maximum a posteriori estimate
th.map <- mlebme(person.resp,ltm.params$est,method = "BM")
head(th.map)

# These estimates are very close, as they should be. 
      