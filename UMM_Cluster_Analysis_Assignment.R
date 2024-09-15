# Import Libraries
library(dplyr)
library(purrr)
library(ggplot2)
library(factoextra)

# Import Customer Dataset 
custdata <- read.csv(file.choose(), header = T)
head(custdata)
str(custdata)
dim(custdata) #  80081 observations of 7 variables 

# Omit the Rows with Missing Values (NA)
custdata <- na.omit(custdata)

# Subset Data and Delete Cust_id and City
custdata_cl <- subset(custdata, select = c(-Cust_Id,-City))
head(custdata_cl)

# Scale the Variables
custdata_cl <- scale(custdata_cl) %>% as.data.frame()
head(custdata_cl)

# Run kmeans() with 3 clusters and Set.seed() to ensure reproducibility
set.seed(123)
CL <- kmeans(custdata_cl, 3)

# Get 'Cluster Size' (i.e., Number of Customers in Each Cluster)
CL$size # 27016, 26013, 27052

# Adding Cluster Membership to the Original Data
custdata$segment <- CL$cluster
custdata = data.frame(custdata)
head(custdata)

# Getting the 'Means' of Each Variables Clusterwise to 2 Decimal Places
allmean <- aggregate(cbind(age, MonthlyIncome, MinBal, MaxBal, Age.with.Bank)~segment, data = custdata, FUN = mean)
allmean=allmean %>% map(round, 2) %>% as.data.frame()
allmean
# Comment: Segment 2 are the 'platinum' customers with avarage age of 54 years,
# which represents the older customer bracket that have been with the Bank for
# over six and half (6.5) years. These platinum customers have greater monthly 
# income, minimum balance and maximum balance. Whereas Segment 3 represents the 
# Bank's youngest customers who have been with the Bank for just one (1.0) year; 
# and have the least monthly balance, min. balance and max. balance when compared 
# to Segments 1 and 2.

# Using Bar Plot to summarise each variables (avarage value) within each clusters
# Age
ggplot(allmean, aes(x = segment, y = age)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = age), vjust = -0.5, color = "black", size = 3) + 
  labs(x = "Cluster", y = "Age")

# MonthlyIncome 
ggplot(allmean, aes(x = segment, y = MonthlyIncome)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = MonthlyIncome), vjust = -0.5, color = "black", size = 3) + 
  labs(x = "Cluster", y = "MonthlyIncome")

# MinBal
ggplot(allmean, aes(x = segment, y = MinBal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = MinBal), vjust = -0.5, color = "black", size = 3) + 
  labs(x = "Cluster", y = "MinBal")

# MaxBal
ggplot(allmean, aes(x = segment, y = MaxBal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = MaxBal), vjust = -0.5, color = "black", size = 3) + 
  labs(x = "Cluster", y = "MaxBal")

# Age.With.Bank
ggplot(allmean, aes(x = segment, y = Age.with.Bank)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Age.with.Bank), vjust = -0.5, color = "black", size = 3) + 
  labs(x = "Cluster", y = "Age.With.Bank")

# Obtain plot of WSS (Within Sum of Squares) to check number of clusters
# Create plot for the 'Elbow Method' to decide Optimum Number of Clusters
wss <- function(k) {
  kmeans(custdata_cl, k,iter.max = 1000)$tot.withinss
}

k.values <- 1:15

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Comment: Using the Elbow Method, the plot indicates K = 3 clusters are finalised.
