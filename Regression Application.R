# Import the data
library(readxl)
HOUSE <- read_excel("Final/House_Data.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"))

# Create a copy (in case data is irreversibly damaged)
house <- HOUSE

# View the data
View(house)

# Load 'psych' package
library(psych)

###############################################################################
######## 1. Calculate simple descriptive statistics for "Sales Price" #########
###############################################################################

# Summary statistics
summary(house$s_p)
sd(house$s_p)
range(house$s_p)

  # Minimum: 29,864 / Maximum: 222,680
  # 1st Quartile: 59,278 / 3rd Quartile: 90,741
  # Median: 70,360
  # Mean: 79,037
  # Range : 222,680 - 29,864 = 192,816
  # Standard Deviation: 29,169
  # Measures of central tendency describe the center of the data 
  # They are often represented by the mean, median, and mode.

  # Mean: Mean represents the arithmetic average of the data. It is calculated by taking the sum of the values and dividing by the number of observations.
  # Median: The middle most value of a variable in a data is its median value.
  # Mode: Mode represents the most frequent value of a variable in the data
  # Standard Deviation: Standard deviation is a measure used to quantify the amount of variation of a set of data values from its mean. 
  # Variance: Variance is the square of the standard deviation and the variance of the random variable with itself.
  # IQR: The Interquartile Range (IQR) is calculated as the difference between the upper quartile (75th percentile) and the lower quartile (25th percentile). 

describe(house$s_p)

  # Kurtosis is a measure of whether the data are heavy-tailed or light-tailed relative to a normal distribution
  # When kurtosis is equal to 0, the distribution is mesokurtic. This means the kurtosis is the same as the normal distribution
  # Negative values of kurtosis indicate that a distribution is flat and has thin tails
  # Positive values of kurtosis indicate that a distribution is peaked and possess thick tails.

describeBy(house, group = house$s_p)

  # Specifying describeBy function with group argument calculates descriptive statistics by group, in this case, each sales price columns.


###############################################################################
############### 2. Create histogram over Sales Price variable #################
###############################################################################

hist(house$s_p,
     main="Frequency Distribution of Sales Price of 950 single-family homes in Springfield, MA",
     xlab="Sale price in dollars",
     xlim=c(20000,250000),
     col="darkmagenta",
     freq=TRUE)

  # This histogram is heavily right-skewed, meaning tail of the distribution lies on the right side.
  # As it can be inferred from the summary statistics, there exists a disparity between Median and Mean value of data in this specific column.
  # This suggests that the distribution of the sales price data does not resemble that of normal distribution.

# Play around with different bin size
hist(house$s_p, nclass = 30)

  # By looking at the histogram with more bin sizes, it becomes more apparent that most houses are priced between 50,000 to 100,000 USD. 

###############################################################################
###################### Generate other visualizations ##########################
###############################################################################

# Generate Scatterplot Matrix
pairs(~s_p + inv + ltsz + hssz + bsemt, data=house, main="Simple Scatterplot Matrix")

# Find correlation among each variables
install.packages("corrplot")
library(corrplot)
house_cor <- cor(house)
corrplot(house_cor)

  # Based on the output of this code, there seem to be some correlation 
  # between s_p and bath, ltsz, hssz, f_place, dw, dr, and bdrms.

###############################################################################
##### 3. Build a regression model to predict the selling price for a home #####
###############################################################################

# First linear model
house_1 <- lm(s_p ~ bath + ltsz + hssz, data= house)
summary(house_1)
  # Both T and P values are satisfactory; thus, statistically significant
  # Though, residual standard error is 22,340, indicating the need for improvement

# Predict
house_1$predict <- predict.lm(house_1, newdata=house)

# Error
house_1$error <- house_1$s_p - house_1$predict

# Residual
resid(house_1)

# Histogram
hist(resid(house_1), nClass=30)

# Describe
describe(resid(house_1))


house$predict <- predict(house_1, newdata=house)
house$resid<- resid(house_1)
house$resid <- house$s_p - house$predict
cor(house$resid,house$s_p, use = "complete.obs")
smaller <- house[,c(22,1:15)]
cor(smaller, use ="complete.obs")
pairs(smaller)
summary(house$error)

# Residual
house$resid <- house$s_p - house_1$predict

# Correlation between Residual and `s_p`
cor(house$resid, house$s_p, use="complete.obs")
  # correlation of 0.765 -> moderate relationship

# Plot
plot(house$ltsz, house$s_p)
abline(lm(house$s_p~house$ltsz), col="red")

GBS_MBA2$predict <-  predict.lm(fitlinear1)
GBS_MBA2$err <- GBS_MBA2$Weight- GBS_MBA2$predict
resid(fitlinear1)
hist(resid(fitlinear1),nclass = 50)

# Look at the correlation again
cor(house)
  # All the first five variables except `inv` seems to have some correlation

# Which significant variables are missing?
house$resid <- resid(house_1)
missing <- house[, c(1:22)]
cor(missing, use="complete.obs")
  # We found no correlation between error and other variables
  # We shall add more models based on this result to get lower standard errors


################################################################################

# Second linear model (Adding all other variables to the model)
house_2 <- lm(s_p ~ inv + bath + ltsz + hssz 
              + factor(bsemt) + factor(a_c) + factor(f_place) 
              + factor(garsz_a) + factor(dw) + factor(dr) + factor(fr) 
              + factor(age5) + factor(stl10) + factor(bdrms), data = house)

summary(house_2)
# Both T and P values are satisfactory except `inv`, `a_c`, `garsz_a`, `fr`, and `bdrms` variable
# Residual standard error is down to 19,160, after adding all the variables into the model.
# There are still rooms to improve in this model. Rather than linearly adding variable, let's create new variables.

# Install `car` package to use VIF function, calculating multi-colinearity
install.packages("regclass")
library(regclass)
VIF(house_2)
hist(resid(house_2),nclass=50)
  # From the GVIF values, all of the values are high, suggesting its interaction effects
  # residuals of this model ranges widely, suggesting its sub-optimal prediction capability


house$predict <- predict(house_2, newdata=house)
house$resid<- resid(house_2)
house$resid <- house$s_p - house$predict
cor(house$resid,house$s_p, use = "complete.obs")
smaller <- house[,c(22,1:15)]
cor(smaller, use ="complete.obs")
pairs(smaller)


###############################################################################
########################## Let's transform the data ###########################
###############################################################################

##################### LOGIC BEHIND VARIABLE TRANSFORMATION ####################
###### I created three new variables based on the purchaser's preference ######
###############################################################################

attach(house)

View(house)

house$size <- hssz * ltsz 
  # `size` column includes the size of the property.
  # Since this is considered a main criteria for purchasing a property, 
  # it is combined in a way to convey a multiplicative effect.

house$rooms <- bath * bdrms
  # `room` consists of ordinal variables that are considered an important criteria when purchasing the house
  # Therefore, it is also combined in a way to denote a multiplicative effect

house$hvac <- a_c + f_place
  # `hvac` consists of ordinal variables that are considered somewhat important criteria when purchasing the house
  # Therefore, it is also combined in a way to denote an additive effect

house$extra <- (garsz_a * stl10) + bsemt
  # `extra` consists of ordinal variables that  considered somewhat important criteria when purchasing the house
  # Therefore, it is also combined in a way to denote additive effect
  # Addition to denote non-essential criteria and multiplication to denote essential criteria

house$kitchen <- dw * (dr + fr)
  # `Kitchen` consists of ordinal variables that are mainly located in the kitchen
  # Addition for non-essential items and multiplication for essential items


final_model <- lm(s_p ~ size 
                  + inv 
                  + factor(age5)
                  + factor(rooms) 
                  + factor(hvac) 
                  + factor(extra) 
                  + factor(kitchen), 
                  data = house, na.action=na.exclude)

summary(final_model)

    # Residual Standard Error: 18,360
    # Adjusted R-Squared: 0.6039
    # P-value: < 2.2e-16
    # F-statistics: 46.21
  
View(house)

# Correlation among s_p and other transformed variables
cor(house[,c(1, 16:20)],use = 'complete.obs')
  # Moderate relationship between s_p and size, rooms, hvac, and kitchen.


hist(resid(final_model),nclass=50)
VIF(final_model)

  # This model is considered usable given its p-value (2.2e-16) and R-squared value (0.6172)
  # Residual standard error is down to 18,360, after adding all the variables into the model.
  # Additionally, histogram over the residual over this model resemble normal curve peaked at 0
  # small VIF values indicates low correlation among variables under ideal conditions VIF<3. 
  # However it is acceptable if it is less than 10.

###############################################################################
##### 4. Build a prediction model to predict the selling price for a home #####
###############################################################################

# Values of the variables (from the instructions given)
inv <- 100
bath <- 2
a_c <- 1 
f_place <- 0
garsz_a <- 1
dw <- 1
dr <- 0
fr <- 0
age5 <- 1
stl10 <- 1
ltsz <- 0.25
hssz <- 1200
bsemt <- 0
bdrms <- 4

# house$size <- hssz / ltsz 
# -> 1200 * 0.25 = 300

# house$rooms <- bath * bdrms
# -> 2 * 4 = 8

# house$hvac <- a_c + f_place
# -> 1 + 0 = 1

# house$extra <- (garsz_a * stl10) + bsemt
# -> (1*1) + 0 = 1

# house$kitchen <- dw * (dr + fr)
# -> 1 * (0 + 0) = 0

# Provide a PREDICTION Interval for the "house of Interest"
predict.lm(final_model, newdata = data.frame(size = 300,
                                             rooms = 8,
                                             hvac = 1,
                                             extra = 1,
                                            kitchen = 0,
                                             inv = 100,
                                             age5 = 1))

# Prediction Interval (95 percent confidence)
predict.lm(final_model, newdata = data.frame(size = 300,
                                             rooms = 8,
                                             hvac = 1,
                                             extra = 1,
                                             kitchen = 0,
                                             inv = 100,
                                             age5 = 1), interval = 'prediction')
                                         