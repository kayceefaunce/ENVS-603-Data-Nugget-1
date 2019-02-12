########################################################################
########################################################################

# Step 1: Install and load the libraries for some R packages that will be 
# useful as we start to look at and analyze the data.

install.packages("readr")
install.packages("primer")
install.packages("outliers")

library(readr)
library(primer)
library(outliers)

########################################################################
########################################################################

# Step 2: Import the moths data from the .csv file.

## This portion of the code reads the .csv file and assigns it to the variable "moths." 
## Note that any header value can be used individually in an analysis simply 
## by calling it after the "moths" variable using a "$" (e.g., moths$spp).

moths <- 
  read.csv("moths.csv",
           header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

## Call the moths variable to view the data table. 

moths

########################################################################
########################################################################

# Step 3. Visualize the distribution of the response variable using a histogram.

hist(moths$spp, col="lightblue")

## What are your thoughts about the distribution of our moth species data from looking at the histogram? 
## Does it look normal?

########################################################################
########################################################################

# Step 4. Run a Shapiro Test on the response variable.

shapiro.test(moths$spp)

## This tests the null hypothesis that the sample being tested is normally distributed.
## If the p-value is less than our significance level (p < 0.05), then we DON'T reject the null hypothesis.

########################################################################
########################################################################

# Step 5: Log-transform the data and re-run the Shapiro Test.

hist(log(moths$spp, 10), col="lightblue")
shapiro.test(log(moths$spp,10))

## How did log-transforming the data change its histogram and the results of the initial Shapiro Test?

########################################################################
########################################################################

# Step 6: Make a histogram of the continuous variable.

hist(log(moths$Isolation, 10), col="lightyellow")

########################################################################
########################################################################

# Step 7: Run a Shapiro Test on the continuous variable.

shapiro.test(moths$Isolation)

## What do the results of the Shapiro Test indicate regarding the distribution of this variable?

########################################################################
########################################################################

# Step 8: Run a Dixon's Q Test.

## The Dixon's Q test tells us whether one of the data points is an outlier. 

dixon.test(moths$Isolation)

## The Drake site from the NCT region is an outlier.
## We can look at this more closely later.

########################################################################
########################################################################

# Step 9: Perform a linear regression.

## "lm" stands for linear model; in R, this is how we run a linear regression
## The variables in the model are named and connected using a tilde (~).

plot(log(spp,10) ~ log(Isolation,10), moths, col="blue")
model <- lm(log(spp,10) ~log(Isolation,10), data=moths); title("Moth spp vs. Isolation")
abline(model)

## Does there appear to be a relationship? If so, what type?


########################################################################
########################################################################

# Step 10: Regression diagnostics

plot(model)

## Hit <return> to go through each of the four regression diagnostic plots.

########################################################################
########################################################################

# Step 11: Statistically summarize the model.

summary(model)

## Is there a significant linear relationship between moths and degree of habitat isolation?

########################################################################
########################################################################

## Test your knowledge by splitting up the study regions and running a linear regression for both, 
## with isolation as the continuous variable. 
## Does isolation appear to have more of an effect on one region over the other?

## Here's the code for splitting up the sample regions to get you started:

NCTmoths<-subset(moths, region =="NCT")
NCTmoths
WAPmoths<-subset(moths, region=="WAP")
WAPmoths
