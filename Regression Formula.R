# How to Run Regression ---------------------------------------------------


# Step 1: Understand the data ---------------------------------------------

## Load data
library(readxl)
GBS_MBA2 <- read_excel("C:/Users/seanj/Desktop/MSBA Bootcamp/Business Stats/Module 1/GBS_MBA2_R.xls", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))
View(GBS_MBA2)

## Install pysch package
library(psych)

## Simple statistics 
describe(GBS_MBA2)
sd(GBS_MBA2$Weight) # Generates NA (We need to change)

## Check and change types of inputs if needed
class(GBS_MBA2$Weight)
GBS_MBA2$Weight <- as.numeric(GBS_MBA2$Weight)
GBS_MBA2$Music <- as.character(GBS_MBA2$Weight)

## Look at a histogram of the y variable
hist(GBS_MBA2$Weight)

## Play with the bin size of the histogram 
hist(GBS_MBA2$Weight, nclass=50)

## Find the correlation between y variable and x variables
cor(GBS_MBA2[,c(2:5)])

## plot interesting correlations (x, y)
plot(GBS_MBA2$Age, GBS_MBA2$Weight)

## add AB line (y, x)
abline(lm(GBS_MBA2$Weight~GBS_MBA2$Age), col="red")

## move y variable to column 1 of correlations
cor(GBS_MBA2[,c(4,2,3,5,19)])

## use pairs to run a scatterplot matrix
pairs(GBS_MBA2[,c(4,2,3,5,19)])

## new subset
newdata <- GBS_MBA2[,c(4,2,3,5,19)]

# Step 2: Create a linear regression model --------------------------------

## Attach dataset (makes it immediately searchable)
attach(newdata)

## Run regression
fitlinear <- lm(Weight~Height+Gender)
fitlinear

## Get summary of new model
summary(fitlinear) # Look at coefficients, std error, t-value, and p-value

## Detach dataset
detach(newdata)

## If n is really big, what's the lower value of the central 95%?
qt(.025,218)
#### use qt(probability, degrees of freedom)
#### .025 or .975 will work - removing outermost 2.5% from either side
#### 218 comes from 219 inputs minus 1 (n-1)
#### compare to t-score - is the abs of qt bigger?

## Multiple R-squared and adjusted R-squared
sd(GBS_MBA2$Weight)^2 #original variance
20.12^2 #new variance after regression
1-(20.12^2)/(sd(GBS_MBA2$Weight)^2) # this is adjusted R-squared

# GOAL IS TO REDUCE VARIANCE AS MUCH AS POSSIBLE 
## Trying to add causal explanatory variables 
### Create a model that is useful and repeatable 
#### Model reduces the chance of variation from prediction


# Step 3: Refine the model to reduce variance and improve fit -------------

## Generate a prediction of best fit (with min and max) for a particular set of variables
attach(GBS_MBA2)
fitlinear1 <- lm(Weight ~ Height)
predict.lm(fitlinear1,newdata=data.frame(Height=70)) # We ran regression of just height as it impacts weight, then use that regression to predict weight based only on height

## Use "predict.lm" to create prediction column
GBS_MBA2$predict <- predict.lm(fitlinear1)

## Use prediction column to calculate error 
GBS_MBA2$err <- GBS_MBA2$Weight-GBS_MBA2$predict
summary(GBS_MBA2$err)

GBS_MBA2$error <- resid(fitlinear1) # Alternative way
summary(GBS_MBA2$error)

## Look at histograms 
hist(GBS_MBA2$err)
hist(resid(fitlinear1)) # Alternative way

## Look at correlation between ERROR and other variables
summary(resid(fitlinear1))
cor(GBS_MBA2) # There should be no correlation between variables that are already accounted for and error term

## Compare model to former model - has standard error increased or decreased?
summary(fitlinear1) # We want to reduce error as much as we can with the variables
