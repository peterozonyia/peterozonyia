# 1. Import and Load BIRTH WEIGHT Data
birthdata <- read.csv(file.choose(), header = T)
head(birthdata)
str(birthdata)

# 2. Cross tabulate dependent variable with each independent variable
library(gmodels)

# Cross tabulate 'Low' birth weight and 'Not Low' birth weight in the data
CrossTable(birthdata$LOW) 
# Comment: 69% of Women have 'Not Low' birth weight and 31% have 'Low' birth weight

# cross Tabulate LOW by AGE
CrossTable(birthdata$AGE, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by LWT
CrossTable(birthdata$LWT, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by RACE
CrossTable(birthdata$RACE, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by SMOKE
CrossTable(birthdata$SMOKE, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by PTL
CrossTable(birthdata$PTL, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by HT
CrossTable(birthdata$HT, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by UI
CrossTable(birthdata$UI, birthdata$LOW, prop.r = TRUE, prop.c = F)

# cross Tabulate LOW by FTV
CrossTable(birthdata$FTV, birthdata$LOW, prop.r = TRUE, prop.c = F)

# 3. Develop a model to predict if birth weight is low or not using the given variables.
# First convert all numerically coded categorical variables into 'factors'
birthdata$LOW <- as.factor(birthdata$LOW)
birthdata$RACE <- as.factor(birthdata$RACE)
birthdata$SMOKE <- as.factor(birthdata$SMOKE)
birthdata$PTL <- as.factor(birthdata$PTL)
birthdata$HT <- as.factor(birthdata$HT)
birthdata$UI <- as.factor(birthdata$UI)
str(birthdata)

# Splitting Data into Training and Testing subsets
library(caret)
index <- createDataPartition(birthdata$LOW, p=0.8, list=FALSE)
traindata <- birthdata[index, ]
testdata <- birthdata[-index, ]
dim(traindata)
dim(testdata)
str(birthdata)
head(birthdata)

# Model the Binary Logistic Regression on the Traindata using glm()
birth_model <- glm(LOW ~. -SR.NO-ID, data=traindata, family="binomial")
summary(birth_model)



# Comment: AGE, FTV, PTL2, PTL3 and UI are highly insignificant but LWT is moderately 
# insignificant at p_value = 0.12945 and will be retained in the model for re-run.

# Recode levels in RACE and PTL using factor() and droplevels() to address insignificance
birthdata$RACE <- factor(birthdata$RACE, levels = c("1","2"))
birthdata$PTL <- factor(birthdata$PTL, levels = c("1","2"))
str(birthdata)

# Re-Run Binary Logistic Regression on the Traindata using glm()
birth_model <- glm(LOW ~. -SR.NO-ID, data = traindata, family = 'binomial')
summary(birth_model)

# Exclude insignificant variables (AGE, FTV) from the re-run
birth_model2 <- glm(LOW ~ LWT + RACE + SMOKE + PTL + HT + UI, data = traindata, family = 'binomial')
summary(birth_model2) # As expected all the remaining variables are now significant

# Check for multicollinearity using vif()
library(car)
vif(birth_model2) # There is no multicollinearity as all the variables are < 5

# 4. Generate three classification tables with cut-off values 0.4, 0.3 and 0.55
# Predicting probabilities
library(ROCR)

traindata$predprob <- round(fitted(birth_model2),2)
predtrain <- prediction(traindata$predprob, traindata$LOW)
perftrain <- performance(pred, 'tpr', 'fpr')
plot(perftrain)
abline(0,1)

testdata$predprob <- predict(birth_model2, newdata=testdata, type='response')
predtest <- prediction(testdata$predprob, testdata$LOW)
perftest <- performance(predtest, 'tpr', 'fpr')
plot(perftest)
abline(0,1)

head(traindata)

# Classification Table with cut-off values of 0.4, 0.3, 0.55
classificationtable <- table(traindata$LOW, traindata$predprob > 0.4)
classificationtable

classificationtable <- table(traindata$LOW, traindata$predprob > 0.3)
classificationtable

classificationtable <- table(traindata$LOW, traindata$predprob > 0.55)
classificationtable

#5. Calculate sensitivity,specificity and misclassification rate for all three tables above. 
# What is the recommended cut-off value?
sensitivity <- (classificationtable[2,2]/(classificationtable[2,2] + classificationtable[2,1]))*100
sensitivity

specificity <- (classificationtable[1,1]/(classificationtable[1,1] + classificationtable[1,2]))*100
specificity





# Fixing issue with the display of levels in the PTL variable
levels(train_data$PTL)

# Find rows where PTL is equal to 0
ptl_0_rows <- birth_data[birth_data$PTL == "0", ]
ptl_0_rows

# Find rows where PTL is equal to 1
ptl_1_rows <- birth_data[birth_data$PTL == "1", ]
ptl_1_rows 

# Find rows where PTL is equal to 2
ptl_2_rows <- birth_data[birth_data$PTL == "2", ]
ptl_2_rows # Only five (5) observations in this level

# Find rows where PTL is equal to 3
ptl_3_rows <- birth_data[birth_data$PTL == "3", ]
ptl_3_rows # Only one (1) observation in this level

# Combine levels 1, 2, and 3 of PTL variable into a new category called "1"
# Create a sample dataframe
df <- data.frame(PTL = factor(c("0", "1", "2", "3")))

# Combine levels 1, 2, and 3 into a new level "1"
df_modified <- df %>%
  mutate(PTL = ifelse(PTL %in% c("1", "2", "3"), "1", as.character(PTL)))
df_modified

str(train_data)
