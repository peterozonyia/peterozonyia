# Assignment_3 Descriptive Statistics

# Iport Premiums data
data2 <- read.csv(file.choose(), header = TRUE)
summary(data2)
str(data2)

# Obtain the Mode for the count of policies available across each Zone.
mode_by_zone <- table(data2$ZONE_NAME)
mode_zone <- as.numeric(names(mode_by_zone)[which.max(mode_by_zone)])
print(paste("Mode for the count of policies across each zone: ", mode_zone))

# Alternatively using Frequency Table function
freq <- table(data2$ZONE_NAME)
freq

# Obtain box-whisker plots for Vintage period
boxplot(data2$Vintage_Period, data = data2, main = "Boxplot
        (Vintage_Period)", ylab = "Vintage_Period", col = "darkorange")

# Detect outliers if present. Hint: use Boxplot() function of ‘car’ Package
# Find skewness and kurtosis of Premium amount by Zone

# using car packages to test for outliers
install.packages("car")
library(car)

outlierTest(lm(Vintage_Period ~ 1, data = data2))

# using package e1071 to find skewness and kurtosis
library(e1071)

skewness(data2$Vintage_Period, type = 2)

kurtosis(data2$Vintage_Period, type = 2)

# Skewness and Kurtosis by Zone using the Function and Aggregate functions
f_skew_kurt <- function(x)c(skew = skewness(x,type = 2),kurt = kurtosis(x,type = 2))
aggregate(Vintage_Period~ZONE_NAME,data = data2,FUN = f)

# Draw a scatter plot of Premium and Vintage period
plot(data2$Premium, data2$Vintage_Period, col = "blue")

# Find the correlation coefficient between Premium and Vintage period and interpret the value
cor(data2$Premium, data2$Vintage_Period) # Moderately Positive correlation at 0.3641487 as r > 0 
