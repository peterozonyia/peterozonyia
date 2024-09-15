# Assignment_Predictive Modelling using R

# 1. Import House Price Data. Check the structure of the data.
hprice_data <- read.csv(file.choose(), header = T)
str(hprice_data)
head(hprice_data)

# 2. Split the data into Training (80%) and Testing (20%) data sets
#First load "caret" packages and create new vector 'index' for the data splitting
library(caret)

index <- createDataPartition(hprice_data$Price, p=0.8, list=FALSE)
traindata <- hprice_data[index,]
testdata <- hprice_data[-index,]
dim(traindata)
dim(testdata)

# 3. Build a regression model on training data to estimate selling price of a House.
hprice_data1 <- lm(Price~Area+Distance+Schools, data=traindata)
summary(hprice_data1)
#Comment: The test output produced very high statistically significant values
# for all the three variables (Schools, Area, Distance) at *** Confidence 
# Interval, which implies that all the independent variables are very 
# strong predictors for the selling prices of house.

# 4. List down significant variables and interpret their regression coefficients.
# Comments: The model summary outputs show that all the three predictor 
# variables (Area, Distance and Schools) have p_values less than 0.05 
# (p<0.05) and are statistically significant at *** Confidence Interval.
# The estimated regression coefficients of all the variables are: -9.542258 
# for the Selling Prices (intercept); +0.034612 for Area; -1.870419 for 
# Distance and +1.318738 for Schools. It means that when all the predictors 
# are at zero coefficient, the coefficient for the 'selling prices' = 
# -Rs9.542258. And so when controlling for all the other variables in the 
# model, each additional unit increase in the predictors means that 
# 'selling prices' is expected to increase by Rs1.32 for the schools and 
# by Rs0.03 for the Area and decreases for the Distance by -Rs1.87 when 
# holding other variables constant.

# 5. What is the R2 and adjusted R2 of the model? Give interpretation.
# Comment: The pooled variance (R2) of the selling prices as predicted by the 
# three predictors in the regression model is 78.83% and the adjusted R2 value
# is 78.42%.This implies that the model is predicting well above the standard 70% cutoff
# and likewise the adjusted R2 value at 78.42% is also very good, which 
# means we can proceed to training our model.

# 6. Is there a multicollinearity problem? If yes, do the necessary steps to remove it.
# Load the car library
library(car)
vif(hprice_data1)
# Comment:  VIF for Area = 1.713328; Distance = 1.024116; Schools = 1.734791
# All the three variables have VIF less than 5 (< 5), which implies that 
# there is no multicollinearity problem in the model.

# 7.Are there any influential observations in the data?
# Check influence in our model data using Cook's Distance Method and DFBETAs
influ <- influence.measures(hprice_data1)
influ

# Influential Plot using plot function of the car packages
influencePlot(hprice_data1,
              id.method="identify",
              main="Influential Plot",
              sub="Circle size is proportional to Cook's Distance")
# Comment: Plot shows some influential observations that are proportional
# to Cook's Distances.Data points detected in the bubbles are 32, 98, 17 & 134.

# 8. Can we assume that errors follow ‘Normal’ distribution?
# Check for normal distribution of errors using Q-Q Plot & Shapiro Test
qqnorm(traindata$resi)
qqline(traindata$resi)

shapiro.test(traindata$resi)

library(nortest)
lillie.test(traindata$resi)
# Comment: The Lilliefors test, Shapiro test & Q-Q Plot show non normal 
# distribution of errors

# 9. Is there a Heteroscedasticity problem? Check using residual vs. predictor plots.
# Get the predicted and residual values and then conduct the plot
traindata$pred <- fitted(hprice_data1)
traindata$resi <- residuals(hprice_data1)
plot(traindata$pred, traindata$resi, col='blue')
# Comment: Our plot shows heteroskedasticity is present as the randomness of 
# the errors tends to resemble that of a cone-shape from left to right.

# Checking normality distribution of the errors also with Q-Q Plot & Shapiro 
qqnorm(traindata$resi)
qqline(traindata$resi, col='red')
# Comment: Q-Q Plot also shows non-normality as the line did not perfectly 
# pass through the first and third quartiles. 

shapiro.test(traindata$resi)
# Comment: Shapiro test also rejects normality as p-value is <0.05 with an
# output p-value of 0.001089

# 10. Calculate the RMSE for the Training and Testing data.
# Finding the RSME of the Training Data
RMSE__train <- sqrt(mean(traindata$resi**2))
RMSE__train
# Comment:Traindata's RSME = 2.244735

# Model Validation using Holdout Method to find the RSME of the Test Data
testdata$pred <- predict(hprice_data1,testdata)
testdata$res <- (testdata$Price-testdata$pred)
RMSEtest <- sqrt(mean(testdata$res**2))
RMSEtest
# cOMMENT: Testdata's RSME =  2.01666
