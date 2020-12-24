#################### Session 1 #######################
######################################################

  # Import the file in the session 1 folder
  file <- read.csv(file = 'C:\\Users\\seanj\\Desktop\\MSBA Bootcamp\\Business Stats\\Module 2\\GBS_MBA2.csv')

  # summary statistics
  summary(file)

  # install Psych Package
  install.packages("psych")

  # describe provides more robust statistics
  describe(file)

  # Create histogram over weight variable
  hist(file$Weight)

  # specify nClass, which creates a number of specified bins
  hist(file$Weight, nclass=20)
  
  # Calculate the average value of weight variable and assign that to wm
  wm <- mean(file$Weight)

#################### Session 2 #######################
##################### Part 1 #########################
  
  # Calculate the probability that x is smaller than equal to 90 
  pnorm(90, mean=100, sd=15) #0.25

  # Calculate the probability that x is bigger than 90
  pnorm(90, mean=100, sd=15, lower.tail=FALSE) #0.74
  
  # Calculate the probability that x is between 80 and 90
  pnorm(90, mean=100, sd=15)-pnorm(80, mean=100, sd=15) #0.16
  
  # Calculate the specific points that include the certain probaility
    # Most common convention is to find a middle 95 percent
    # 1-0.95=0.05 -> 0.05/2 = 0.025 
  qnorm(0.025, mean=100, sd=15) #70.6
  qnorm(0.975, mean=100, sd=15) #129.4
  
  # returns z value of -2
  qnorm(0.025)  #-1.96
  # returns z value of +2
  qnorm(0.975)  #+1.96

  
#################### Session 2 #######################
##################### Part 2 #########################
  
  GBS_MBA2 <- read.csv(file = 'C:\\Users\\seanj\\Desktop\\MSBA Bootcamp\\Business Stats\\Module 2\\GBS_MBA2.csv')
  
  # Install psych package
  install.packages("psych")
  library(psych)
  
  # Show summary stats for file
  summary(GBS_MBA2)
  
  # Find average weight
  meanw <- mean(GBS_MBA2$Weight)
  print(meanw)

  # Histogram with 30 bins
  hist(GBS_MBA2$Weight, nclass = 30)
  
  # Describe by Gender
  describeBy(GBS_MBA2, group = GBS_MBA2$Gender)
  
  # Split the previous list
  mylist <- split(GBS_MSBA2, GBS_MBA2$Gender)
  
  # Describe the splitted list
  describe(mylist$`1`)
  
  # Histogram of split file
  hist(mylist$`1`$weight)
  hist(mylist$`0`$weight)
  
  # Draw a scatterplot of Height vs. Weight
  plot(GBS_MBA2$Height,GBS_MBA2$Weight)
  
  # Female as red dot, Male as blue dot
  points(GBS_MBA2$Height[GBS_MBA2$Gender==1],GBS_MBA2$Weight[GBS_MBA2$Gender==1],col="red")
  points(GBS_MBA2$Height[GBS_MBA2$Gender==0],GBS_MBA2$Weight[GBS_MBA2$Gender==0],col="blue")
  
  # Describe the Weight group by Gender
  describeBy(GBS_MBA2$Weight,group=GBS_MBA2$Gender)
  
  # Generate multiple scatter plots
  pairs(~Weight + Height + Gender + Age, 
        data=GBS_MBA2,  
        main="Simple Scatterplot Matrix")
  
  
  # Check the correlation 
  cor(GBS_MBA2) # gives an error message
  newdata <- GBS_MBA2[1:220,c(4,2,3,5)]
  View(newdata)
  cor(newdata)
  pairs(newdata)
  
  # Linear model
  fitlinear_1 <- lm(newdata$Weight ~ newdata$Height)
  summary(fitlinear_1)  # Weight = (6.16 * Height) - 261.05
  
  # More complicated linear model
  fitlinear_2 <- lm(newdata$Weight ~ newdata$Height + newdata$Gender)
  summary(fitlinear_2)

  
  
#################### Session 2 #######################
##################### Homework #######################
  
  # The TV Show Magnum PI had Tom Selleck driving a Ferrari 308 GTS and he always wore a hat with the top off, 
  # In reality he was too tall to fit in the car with the top on.  The local Ferrari dealer was interested in how much of the population would not fit.
  # SO: Given:  Normal population with mean mu = 67", standard deviation sigma = 4.2".
  
  # 1. He knows that if someone is taller than six foot (72 inches), what percent of the population would be too tall?
  pnorm(72, mean=67, sd=4.2, lower.tail = FALSE) 
    
  # 2. He figures that someone up to 6 foot 6 could scrunch down or lean, how unusual is a person who is this tall?
  pnorm(78, mean=67, sd=4.2)  
    
  # 3. What portion of the population lies between 72" and 74"?
  pnorm(74, mean=67, sd=4.2)-pnorm(72, mean=67, sd=4.2)
    
  # 4. He want to tell the designers to make sure all but 4% of the people will fit, how tall is that?
  qnorm(0.96, mean=67, sd=4.2) 
    
  # 5. There is an additional problem that people who are too short cant see over the steering wheel,  
  # if they need to be 5 foot 5 (65 inches) what prercent of the people are too short?
   
    # 5a. too short?
    pnorm(65, mean = 67, sd=4.2)
  
    # 5b. If they could scoot up to see an additional 4 inches what percentage do we gain?  
    pnorm(61, mean = 67, sd=4.2)-pnorm(56, mean = 67, sd=4.2)
    
    # What percent of the population lies between 61" and 65"?
    pnorm(65, mean=67, sd=4.2)-pnorm(61, mean=67, sd=4.2)
  
    # 5c. Like the design issue above, What height is so small that only 1% are shorter?
    qnorm(.01, mean=67, sd=4.2)
    
#################### Session 3 #######################
################ Group Assignment ####################
#################### Part 1 ##########################
    
    install.packages("readxl")
    library(readxl)
    Fernbank_data_set <- read_excel('C:\\Users\\seanj\\Desktop\\MSBA Bootcamp\\Business Stats\\Module 3\\Fernbank_data.xlsx')
    
    # View the file
    View(Fernbank_data_set)
    
    # Summary
    summary(Fernbank_data_set)
    
    # Histogram 
    hist(Fernbank_data_set$attendance,nclass=50)
    snow_day<-Fernbank_data_set[c(Fernbank_data_set$"snow/ice"==1),]
    nonsnow_day<-Fernbank_data_set[c(Fernbank_data_set$"snow/ice"!=1),]
    
    # Split our data by Christmas Week
    split_christmas<-split(nonsnow_day,nonsnow_day$christmas)
    print(split_christmas)
    
    # Load psych package
    library("psych")
    
    # See the details of Christmas Week
    describe(split_christmas$'1')
    
    # See the histogram of non-Christams week and non-snow days 
    hist(split_christmas$'0'$attendance, nclass=10)
    plot(Fernbank_data_set$attendance,Fernbank_data_set$school, type = "n", las=1, xlab = "attendance",ylab="school days in session")
    points(Fernbank_data_set$attendance[Fernbank_data_set$christmas==1],Fernbank_data_set$school[Fernbank_data_set$christmas==1],col="red")
    points(Fernbank_data_set$attendance[Fernbank_data_set$christmas==0],Fernbank_data_set$school[Fernbank_data_set$christmas==0],col="blue")
    
    # Pairs scatter plot of multiple variables
    pairs(~attendance + adult + temp + advertising, data = Fernbank_data_set,  main="Simple Scatterplot Matrix")
    
    # Find the correlation between attendance and temp
    cor(Fernbank_data_set$attendance, Fernbank_data_set$temp) #0.71
    
    # Fit a linear model
    model1<-lm(attendance~temp, data = Fernbank_data_set)
    summary(model1)
    
    # Correlation 
    cor(Fernbank_data_set[,c(2:4,6,7,9)])
    
    # Fit a linear model for variables that we see some correlation
    model2 <-lm(attendance~temp+school+adult+sunshine+advertising+`snow/ice`+christmas, data = Fernbank_data_set)
    summary(model2) #t value of advertising is not greater than 2 - not sig
    
    # Drop the advertising and christmas variables
    Fernbank_data_set_new <- fernbank_data_set[,c(2:6)]
    summary(Fernbank_data_set_new)
    cor(Fernbank_data_set_new)
    
    # Fit a model based on the correlation charts from previous code
    model3<-lm(attendance~temp+school+adult+`snow/ice`,data = Fernbank_data_set_new)
    summary(model3)
    
    # Find the relationship between attendance and adult
    cor(Fernbank_data_set_new$attendance, Fernbank_data_set_new$adult) #-0.40
    plot(Fernbank_data_set_new$attendance, Fernbank_data_set_new$adult, 
         las=1, xlab = "Attendance", ylab="Adult")
    
#################### Session 4 #######################
##################### Part 2 #########################

    
    # Import the file in the session 1 folder
    file <- read.csv(file = 'C:\\Users\\seanj\\Desktop\\MSBA Bootcamp\\Business Stats\\Module 2\\GBS_MBA2.csv')
    
    # Summary statistics
    summary(file)
    
    # View file
    View(file)
    
    # Exclude NA Values
    file <- file[c(1:220),]
    View(file)
    
    # Install Psych Package
    install.packages("psych")
    library(psych)
    
    # Describe provides more robust statistics
    describe(file)
    
    # Correlation
    cor(file[,c(22, 4,2,3,5, 19)])
    pairs(file[,c(22, 4,2,3,5, 19)])
    
    # Plot against some variables
    plot(file$Age, file$Weight)
    abline(lm(file$Weight~file$Age), col="red")
    
    # Run linear regression
    fitlinear1 <- lm(file$Weight ~ file$Height)
    summary(fitlinear1) #t value equals slope divided by Std. Error.
    
    # T statistics
    qt(0.025, 218)
    qt(0.975, 218)
    
    # Standard Deviation
    std <- sd(file$Weight)
    
    # Variance
    var <- sd(file$Weight)^2
    var

    # Calculate Adjusted R-squared
    # It measures the proportion of variation in the dependent variable that can be attributed to the independent variable.
    # Calculated by Squared Residual Standard Error divided by Variance
    20.12^2
    1-(20.12^2)/(sd(file$Weight)^2) #0.58 of the percent variation explain 
    # Add prediction and err column to the dataset
    file$predict <- predict.lm(fitlinear1)
    file$err <- file$Weight-file$predict
    View(file)
    
    # Attach
    attach(file)
    
    # Linear Model
    fitlinear <- lm(Weight ~ Height)
    summary(fitlinear)
    
    # Create model prediction interval
    predict.lm(fitlinear, newdata=data.frame(Height=70), interval="prediction", level=0.95)
    
    # Or you can do these two codes in one command using resid
    resid <- resid(fitlinear1)
    
    -261+(6.16*70)
    170.2 +- 2*20.12
    
#################### Session 5 #######################
##################### Part 1 #########################
    
  bigsmall <- read_excel('C:\\Users\\seanj\\Desktop\\MSBA Bootcamp\\Business Stats\\Module 5\\bigsmall.xlsx')
  summary(bigsmall)
  sd(bigsmall$y)
  hist(bigsmall$y, nClass=20)
  pairs(bigsmall)
  cor(bigsmall)  
  attach(bigsmall)    
  model1 <- lm(y~ x1)    
  summary(model1)    
    # Notice the RSME of 118.7
    # mean/(2* RSME) = standard deviation
    # 95 percent confidence that slope is 95 +- 2(8.794)
  qt(0.025, 17)

  # Residual (errors)
  bigsmall$error <- resid(model1)
  View(bigsmall) 
  
  # See the stats
  summary(bigsmall$error) 
  
  # Correlation
  cor(bigsmall) # x2 is a pretty good predictor of model
  
  # Rerun the model
  model2 <- lm(y~ x1 + x2) 
  summary(model2) # notice how RSME has been decrased
  # Also intercept having high p value and low t value is fine in the regression model
  # Adjusted R squared value of 0.89 explains that this new model explains about 90 percent of variance.
  # It is a great idea to have both adjusted R squared and multiple R squared to have similar value
    
#################### Session 5 #######################
##################### Part 2 #########################  
  
  # Import SeaWatch_C Data
  library(readxl)
  SeaWatch_C_data <- read_excel("Module 4/SeaWatch C data.xls", 
                                col_types = c("text", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text"))
  
  # View the SeaWatch_C Data
  View(SeaWatch_C_data)
  
  # Summary Stats
  summary(SeaWatch_C_data)  
  
  # We notice that there is missing value in our dataset, now we delete them
  SeaWatchC_clean<-na.omit(SeaWatch_C_data)
  
  # Let's check again if there is any missing value 
  sum(is.na(SeaWatchC_clean))
  
  # View SeaWatch_C_Clean
  View(SeaWatchC_clean)
  
  # We also need to see the mean of GROSS and its histogram
  mean(SeaWatchC_clean$GROSS)
  hist(SeaWatchC_clean$GROSS, nClass = 100)
  
  # See the correlation between our variables 
  cor(SeaWatchC_clean[,c(3,4,5:15,18,19)])
  
  # Calculate the standard deviation before fitting the model
  sd(SeaWatchC_clean$GROSS) #4157.74

  # Attach
  attach(SeaWatchC_clean)
  model_SW <- lm(GROSS ~ CNVHRS)
  summary(model_SW) # RSME = 1007
  
  # Predict 
  SeaWatchC_clean$predict <- predict(model_SW, newdata=SeaWatchC_clean)
  summary(SeaWatchC_clean$predict)
  
  # Error
  SeaWatchC_clean$error <- resid(model_SW)
  View(SeaWatchC_clean)
  
  summary(SeaWatchC_clean$error)
  
  # Residual
  SeaWatchC_clean$resid <- SeaWatchC_clean$GROSS-SeaWatchC_clean$predict
  View(SeaWatchC_clean)
  
  # Correlation between Residual and COLLPR
  SeaWatchC_clean$COLLPR <- as.numeric(SeaWatchC_clean$COLLPR)
  cor(SeaWatchC_clean$resid, SeaWatchC_clean$COLLPR, use="complete.obs")
  
  # Look at the correlation again
  cor(SeaWatchC_clean[,c(3,4,5:15,18,19,22)])
  # We found no correlation between error and other variables
  
  # Add more variables to get lower standard errors
  model_SW_2 <- lm(GROSS ~ CNVHRS + CART + REAG + COLLPR/POP80 + factor(VISIT), data=SeaWatchC_clean)
  summary(model_SW_2)  # RSME = 777
  # ^^Explanation of each variables
  # Gross - Total gross receipts, in dollars
  # CNVHRS - Total canvasser-hours used
  # CART - number of votes for Jimmy Caters
  # REAG - number of votes for Ronald Reagan
  # COLLPR - % of population with at least 4 years of college education 
  # POP80 - total population
  # Visit - Which visit to this town does this observation represent
  
  # Draw histogram
  hist(resid(model_SW_2),nclass=100)
  
#################### Session 5 #######################
##################### Homework #######################
  
  # Generate a model that predicts CNVHRS  using (number of College Graduates)
  # Comment on the regression output
  # Significance?
  # Generate a prediction for a town with 10,000 college grads
  View(SeaWatchC_clean)
  SeaWatchC_clean$colnum <- (SeaWatchC_clean$POP80*SeaWatchC_clean$COLLPR)/100
  View(SeaWatchC_clean)
  attach(SeaWatchC_clean)
  FitCol <- lm(CNVHRS~colnum)
  summary(FitCol) # RSME = 77.51
  
  predict.lm(FitCol, newdata=data.frame(colnum=10000))

#################### Session 6 #######################
##################### Part 1 #########################
  
  # Transform allows one to predict the value of the response variable 
  # for varying inputs of the predictor variable given 
  # the slope and intercept coefficients of the line of best fit
  install.packages("regclass")
  library(regclass)
  VIF(model_SW)
  
#################### Session 7 #######################
############### Afternoon Session 1 ##################
  
  # Import MBA Success data
  library(readxl)
  MBA_success_data <- read_excel("Module 6/MBA success data.xlsx")
  View(MBA_success_data)
  
  # Attach the data
  attach(MBA_success_data)
  
  # Observe the Success column 
  hist(Success)
  summary(Success)
  
  # Pairs
  pairs(MBA_success_data) 
  #^^ there could be relationship between GMAT and success
  
  # Correlation
  cor(MBA_success_data)
  
  # Build a linear model
  linear <- lm(Success ~ GMAT)
  summary(linear)  
  #^^ Though P and T values are good, RSME is not ideal
  #^^ Therefore, it is not considered a good model
  
  # Build a logistic linear model
  model <- glm(Success ~ GMAT, family = binomial(link = 'logit'))
  summary(model)  
  #^^ For every 1pt increase in GMAT, log of odd ratio increase by 0.09  
  
  # Predict
  predict(model)
  predict(model, type = 'response')
  
  # Graph
  MBA_success_data$prob <- predict(model, type = 'response')
  plot(MBA_success_data$GMAT, MBA_success_data$prob)
  

  # Error
  MBA_success_data$error <- MBA_success_data$Success - MBA_success_data$prob
  plot(GMAT, MBA_success_data$error)
  
  
############### SeaWatch D Prediction #################
##################### Project #########################
  
  # Import SeaWatch_C Data
  library(readxl)
  SeaWatch_C_data <- read_excel("Module 4/SeaWatch C data.xls", 
                                col_types = c("text", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text"))
  
  # Import SeaWatch D data
  library(readxl)
  SeaWatch_D_data <- read_excel("Module 4/SeaWatch D data.xls", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
  
  # Generate pairs
  pairs(SeaWatch_D_data[,c(6:15)])
  
  # Attach SeaWatch D Data
  attach(SeaWatch_D_data)
  

  # Change the linear regression model from SeaWatch C accordingly
  model_SW_D <- lm(SeaWatchC_clean$GROSS ~ SeaWatch_D_data$CART + SeaWatch_D_data$REAG + SeaWatch_D_data$COLLPR + SeaWatch_D_data$POP80)
  
  summary(model_SW_D)
  
  # Tranforming SeaWatch_C  
  SeaWatch_C_data$pop<-SeaWatch_C_data$POP80*SeaWatch_C_data$POVPR/100
  
  SeaWatch_C_data$man<-SeaWatch_C_data$POP80*SeaWatch_C_data$MFGPR/100
  
  SeaWatch_C_data$student<-SeaWatch_C_data$POP80*SeaWatch_C_data$COLLPR/100
  
  SeaWatch_C_data$income<-SeaWatch_C_data$POP80*SeaWatch_C_data$PERCAPI/100

  lm_SWC <-lm(GROSS~student+PERCAPI+pop+factor(VISIT),data=SeaWatch_C_data)
  
  summary(lm_SWC)
  
  # Correlation among GROSSS, student, POVPR, PERCAPI and pop
  cor(SeaWatch_C_data[,c(3,24,14,13,22)],use = 'complete.obs')
  
  # Transforming SeaWatch_D as well
  SeaWatch_D_data$pop<-SeaWatch_D_data$POP80*SeaWatch_D_data$POVPR/100
  
  SeaWatch_D_data$man<-SeaWatch_D_data$POP80*SeaWatch_D_data$MFGPR/100
  
  SeaWatch_D_data$student<-SeaWatch_D_data$POP80*SeaWatch_D_data$COLLPR/100
  
  SeaWatch_D_data$income<-SeaWatch_D_data$POP80*SeaWatch_D_data$PERCAPI/100
  
  SeaWatch_D_data$VISIT <- 1 
  
  detach()
  
  attach(SeaWatch_C_data)
  
  View(SeaWatch_D_data)
  
  # Build model using C
  lm_d <-lm(GROSS ~ student + pop + factor(VISIT), data=SeaWatch_C_data, na.action=na.exclude)
  
  predict.lm(lm_d, newdata=data.frame(student = 10000, pop=10000, VISIT = 1))
  
  # Predict.lm function takes formula above and apply 
  SeaWatch_D_data$prediction <- round(predict.lm(lm_d, newdata=SeaWatch_D_data))
  
  as.numeric(SeaWatch_D_data$prediction)
  
  max_gross <- round(max(SeaWatch_D_data$prediction, na.rm = TRUE))
  
  # We've found that Hempstead is the city with max gross revenue
  SeaWatch_D_data[SeaWatch_D_data$prediction==max_gross, ]
  
  # What is the actual amount of revenue?
  predict.lm(lm_d, newdata=data.frame(student = 154345.8730, pop=38401.8440, VISIT = 1))