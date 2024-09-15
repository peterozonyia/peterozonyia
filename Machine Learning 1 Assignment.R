# Importing the Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(ROCR)
library(e1071)
library(car)

# Q1: Import Email Campaign DATA
mcdata <- read.csv(file.choose(), header = T)
head(mcdata)
str(mcdata)

# Converting Categorical Variables to Factors using as.factor()
mcdata$Success <- as.factor(mcdata$Success)
mcdata$Gender <- as.factor(mcdata$Gender)

# MODEL 1: BINARY LOGISTIC REGRESSION (BLR) 
# Building Logistic Regression Model using glm()
model_blr <- glm(Success ~ Gender+AGE+Recency_Service+Recency_Product+Bill_Service+Bill_Product, data = mcdata,family = binomial)
summary(model_blr)
# Interpretation: The summary of BLR model shows that Gender and Age are highly insignificant except 
# Age <=55 which is moderately significant at 10% with p_value of 0.065986. This means that Gender 
# (both male and female) does not affect (negatively or positively) the campaign outcome. Likewise the
# Age variable does not has much impact except those in the range >45<=55 with moderate significance. 
# The other predictors:Recency_Service, Recency_Product, Bill_Service and Bill_Product are all highly 
# significant at p_value < 0.001 with both negative and positive coefficients. Both Recency_Service 
# and Recency_Product are highly significant with negative coefficients, which indicate strongly that
# with unit increases campaign 'success' decrease. Whereas the Bill_Service and Bill_Product have 
# positive coefficients, which indicate a higher probability of success as these variables increase in
# unit amount. Hence, the campaign can be rated and analysed as a huge success (class = 1). 
# This interpretation can help the business understand key factors affecting its campaign sucess and 
# then prioritise their investments and talor their strategies for maximum successful outcome.

# Checking model Performance using only significant variables
# Re-Running the BLR model by dropping insignificant variables: Gender and Age
model_blr2 <- glm(Success ~ Recency_Service+Recency_Product+Bill_Service+Bill_Product, data = mcdata,family = binomial)
summary(model_blr2)

# Get the estimate of predicted probability 
mcdata$predprob <- round(fitted(model_blr2),2)
head(mcdata)

# Setting threshold
counts <- data.frame(table(mcdata$Success))
colnames(counts)[1] <- "Success"
counts$Percent <- counts$Freq/sum(counts$Freq)
counts # Percentage of Success at 0 = 74% and at 1 = 26%

threshold <- 0.26

# Checking Model Performance using ROC Curve and AUC
mcdata$predprob <- predict(model_blr2, mcdata, type = 'response')
pred <- prediction(mcdata$predprob, mcdata$Success)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

auc <- performance(pred,"auc")
auc@y.values # auc = 85% (0.8528275)

# Getting Confusion Matrix
mcdata$Estimated_Success <- as.factor(ifelse(mcdata$predprob > threshold,1,0))
confusionMatrix(mcdata$Estimated_Success,mcdata$Success,positive="1")

# Comment: Overall BLR model accuracy = 76% (0.757); and the area under the ROC curve = 85.28 


# Q2: MODEL 2: NAIVE BAYES (NB)
# Building Naive Bayes model
model_nb <- naiveBayes(Success ~ Gender+AGE+Recency_Service+Recency_Product+Bill_Service+Bill_Product, data = mcdata)
model_nb

# ROC Curve and AUC
prednb <- predict(model_nb, mcdata, type='raw')

#Computing AUC value for ROC curve
pred <- prediction(prednb[,2], mcdata$Success)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)

auc <- performance(pred, "auc")
auc@y.values # auc = 0.811818

# Confusion Matrix for test data
mcdata$Estimated_Success2 <- as.factor(ifelse(prednb[,2] > threshold,1,0))
confusionMatrix(mcdata$Estimated_Success2, mcdata$Success, positive="1")

# Comment: The overall NB model accuracy = 79% (0.7862); and the Area under ROC Curve = 81.18

# Conclusion:
# The Binary Logistic Regression method gave highest AUC = 85.28 compared to Naive Bayes method with 
# AUC = 81.18, which suggests that BLR model performs better than NB model in predicting binary outcomes
# for the dependent variable 'Success' (1: campaign 'email is opened' and 0: 'email not opened'). 


# Q 3: COMPARING BINARY LOGISTIC REGRESSION (BLR) vs SUPPORT VECTOR MACHINE (SVM)
# NOTE: No need to re-import data. Still using previously imported campaign data (mcdata)
# Also the Categorical Variables: Gender and Success have already been converted to factors

# Combine the service and product variables
mcdata$Total_Recency <- mcdata$Recency_Service + mcdata$Recency_Product
mcdata$Total_Bill <- mcdata$Bill_Service + mcdata$Bill_Product

# MODEL 3; BINARY LOGISTIC REGRESSION (BLR)
# Fit the logistic regression model
model_lr <- glm(Success ~ AGE + Total_Recency + Total_Bill + Gender, 
                      data = mcdata, family = binomial)

# Summary of the logistic model
summary(model_lr)
# Observation: Gender and Age are insignificant and will be dropped in the re-run of the model

# Re-run the model with only the significant variables
model_lr <- glm(Success ~ Total_Recency + Total_Bill, 
                data = mcdata, family = binomial)
summary(model_lr)

vif(model_lr) # No multicollinearity

# Get the estimate of predicted probability 
mcdata$predprob3 <- round(fitted(model_lr),2)
head(mcdata)

# Checking Model Performance using ROC Curve and AUC
mcdata$predprob3 <- predict(model_lr, mcdata, type = 'response')
pred <- prediction(mcdata$predprob3, mcdata$Success)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

auc <- performance(pred,"auc")
auc@y.values # auc = 0.8262536

# Getting Confusion Matrix
mcdata$Estimated_Success3 <- as.factor(ifelse(mcdata$predprob3 > threshold,1,0))
confusionMatrix(mcdata$Estimated_Success3, mcdata$Success, positive="1") # Accuracy = 74% (0.7365)

# Observation: The overall accuracy for BLR = 74%; and the area under ROC curve = 82.63 (0.8262536)


# MODEL 4: SUPPORT VECTOR MACHINE (SVM)
# Fit the SVM model
svm_model <- svm(Success ~ AGE + Total_Recency + Total_Bill + Gender, 
                 data = mcdata, type = "C", probability = TRUE, kernel = "linear")
svm_model

pred3 <- predict(svm_model, mcdata, probability = TRUE)
pred4 <- attr(pred3, "probabilities")[,1]

# Computing AUC value for ROC curve
pred <- prediction(pred4, mcdata$Success)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

auc <- performance(pred, "auc")
auc@y.values # 0.1732273

# Confusion Matrix for Test Data
mcdata$Estimated_Success4 <- as.factor(ifelse(pred4 > threshold,1,0))
confusionMatrix(mcdata$Estimated_Success4, mcdata$Success, positive="1") # Accuracy = 23% (0.2328)

# Comment: The overall accuracy for SVM model is 23%; and the area under ROC curve is 17.32

# Conclusion: The BLR model gives the highest AUC of 82.63 whereas the SVM model gives AUC of 17.32, 
# which implies that BLR model performs far more better than SVM model when both service and product
# variables of the campaign data are combined together.