# APM_BINARY_LOGISTIC_REGRESSION_ASSIGNMENT
# Load libraries
library(gmodels)
library(dplyr)
library(caret)
library(car)
library(ROCR)
library(pROC)

# 1. Import BIRTH WEIGHT dataset.
birth_data <- read.csv(file.choose(), header = T)
head(birth_data)
str(birth_data)

# Pre-processing and Feature Engineering the Dataset before modeling
# Check for Missing Values (NA)
missing_values <- colSums(is.na(birth_data))
missing_values # No missing values

# Convert numerically coded categorical variables into 'factors'
birth_data$LOW <- as.factor(birth_data$LOW)
birth_data$RACE <- as.factor(birth_data$RACE)
birth_data$SMOKE <- as.factor(birth_data$SMOKE)
birth_data$PTL <- as.factor(birth_data$PTL)
birth_data$HT <- as.factor(birth_data$HT)
birth_data$UI <- as.factor(birth_data$UI)
birth_data$FTV <- as.factor(birth_data$FTV)
str(birth_data)

# Check for Data (Im)balance using table() or crossTable()
low_counts <- table(birth_data$LOW)
low_counts 
# Comment: It appears that the target variable 'LOW' (0='Not Low' & 1='Low') is imbalanced 
# given that class 0 = 130 and class 1 = 59. This has potential to bias the model and 
# affect its accuracy and reliability. 

# Checking Percentage of 'Low' birth weight vs 'Not Low' birth weight using crossTable()
CrossTable(birth_data$LOW)
# Comment: 69% of Women have 'Not Low' birth weight and 31% have 'Low' birth weight

# 2. Cross tabulate dependent variable with each independent variable
library(gmodels)

# cross Tabulate LOW by AGE
CrossTable(birth_data$AGE, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by LWT
CrossTable(birth_data$LWT, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by RACE
CrossTable(birth_data$RACE, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by SMOKE
CrossTable(birth_data$SMOKE, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by PTL
CrossTable(birth_data$PTL, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by HT
CrossTable(birth_data$HT, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by UI
CrossTable(birth_data$UI, birth_data$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by FTV
CrossTable(birth_data$FTV, birth_data$LOW, prop.r = TRUE, prop.c = F)

# 3. Develop a model to predict if birth weight is low or not using the given variables.
# Split Data into Training and Testing subsets
set.seed(123)

index <- createDataPartition(birth_data$LOW, p=0.7, list = FALSE)
train_data <- birth_data[index,]
test_data <- birth_data[-index,]
dim(train_data)
dim(test_data)

# Modeling the Binary Logistic Regression on the Traindata using glm()
train_model <- glm(LOW~AGE+LWT+RACE+SMOKE+PTL+HT+UI+FTV,data=train_data,family="binomial")
summary(train_model)
# Comment: Some variables (ex: Age, FTV, PTL) that are very insiginsignificant will be 
# excluded in the model re-run. E.g., FTV is 'statistically' insignificant, despite 
# being a crucial 'clinically important' variable. The reason may be because of the very 
# low number of 'adequate' visits up to max of 6 visits (eg,3 visits at FTV3=7 only, 
# FTV4=4, FTV5=1 only) compared to no visit at FTV0=100, one visit at FTV1=47 and 
# for two visits at FTV2=30. According to literature, more visits will most likely lead to
# 'Low' birth-weight; but looking critically at those in the 'none' visits (FTV0), majority 
# n=64 were 'Not Low' while n=36 recorded 'Low' birth weights. This nonconformity suggests
# that other factors, besides FTV have strong significant influence on low birth-weight. 
# Similarly, history of premature lobour (PTL) suggests low birth-weight, but most of those
# reported at PTL3 & PTL2 did not have low birth weight, which is contrary to domain 
# knowledge. Given above explanations, there is no advantage to include FTV & PTL in our 
# model despite their 'clinical importance' as is well documented in the literature. Yet, 
# due to domain knowledge & extant findings other insignificant variables will be retained.

# Model re-run with FTV, PTL and Age variables excluded completely
train_model <- glm(LOW ~ LWT + RACE + SMOKE + HT + UI, data = train_data, family = "binomial")
summary(train_model) # All the retained variables in the model are now significant 

# Check for multicollinearity using vif()
library(car)
vif(train_model) 
# Comment: There are no multicollinearities as all the VIF scores are < 5

# 4. Generate three classification tables with cut-off values 0.4, 0.3 and 0.55
library(ROCR)
# Predicting Probabilities
train_data$predprob <- round(fitted(train_model),2)
predtrain <- prediction(train_data$predprob, train_data$LOW)

test_data$predprob <- predict(train_model, newdata=test_data, type='response')
predtest <- prediction(test_data$predprob, test_data$LOW)

# Classification Table with cut-off values of 0.3, 0.4, 0.55
classificationtable1 <- table(train_data$LOW, train_data$predprob > 0.3)
classificationtable1 # (63+32)/133 = 71%

classificationtable2 <- table(train_data$LOW, train_data$predprob > 0.4)
classificationtable2 # (75+22)/133 = 73%

classificationtable3 <- table(train_data$LOW, train_data$predprob > 0.55)
classificationtable3 # (85+15)/133 = 75%

#5. Calculate sensitivity,specificity and misclassification rate for all three tables above. 
sensitivity1 <- (classificationtable1[2,2]/(classificationtable1[2,2] + classificationtable1[2,1]))*100
sensitivity1 # 76.19048

sensitivity2 <- (classificationtable2[2,2]/(classificationtable2[2,2] + classificationtable2[2,1]))*100
sensitivity2 # 52.38095

sensitivity3 <- (classificationtable3[2,2]/(classificationtable3[2,2] + classificationtable3[2,1]))*100
sensitivity3 # 35.71429

specificity1 <- (classificationtable1[1,1]/(classificationtable1[1,1] + classificationtable1[1,2]))*100
specificity1 # 69.23077

specificity2 <- (classificationtable2[1,1]/(classificationtable2[1,1] + classificationtable2[1,2]))*100
specificity2 # 82.41758

specificity3 <- (classificationtable3[1,1]/(classificationtable3[1,1] + classificationtable3[1,2]))*100
specificity3 # 93.40659

# Model Validation: Holdout Method using confusionMatrix
# What is the recommended cut-off value?
# Finding the Best Threshold
sstrain <- performance(predtrain, "sens", "spec")
best_threshold <- sstrain@alpha.values[[1]][which.max(sstrain@x.values[[1]]+sstrain@y.values[[1]])]
paste("Best Threshold is :",round(best_threshold,2)) 
# Comment: The model's "Best Threshold" is "0.37"

# Classification Report based on the Best Threshold: 0.37
classificationtable4 <- table(train_data$LOW, train_data$predprob > 0.37)
classificationtable4 # (72+24)/133 = 0.72180 (72%)

sensitivity4 <- (classificationtable1[2,2]/(classificationtable1[2,2] + classificationtable1[2,1]))*100
sensitivity4 # 57.14286 (57%)

specificity4 <- (classificationtable1[1,1]/(classificationtable1[1,1] + classificationtable1[1,2]))*100
specificity4 # 79.12088 (79%)

# Using Confusion Matrix
train_data$predtrain<-as.factor(ifelse(train_data$predprob>best_threshold,1,0))
confusionMatrix(train_data$predtrain,train_data$LOW,positive="1")
# Comment: Based on the best threshold, Sensitivity = 57%, Specificity = 79% and 
# Accuracy = 72%, which indicates a relatively good model performance.

# 6. Obtain ROC curve and report area under curve.
perftrain <- performance(predtrain, 'tpr', 'fpr')
plot(perftrain)
abline(0,1)

perftest <- performance(predtest, 'tpr', 'fpr')
plot(perftest)
abline(0,1)

auctrain <- performance(predtrain, "auc")
auctrain@y.values 
# Comment: Output = 0.7484301. Thus, the model's AUC is 74.84% which shows strong 
# discriminative ability, meaning our training model has a good predictive performance.
