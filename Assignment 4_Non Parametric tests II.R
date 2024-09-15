# Assignment 4_Non Parametric tests II
# 1. Import EMPLOYEE SATISFACTION SURVEY data.
satsurvey <- read.csv(file.choose(), header = T)
head(satsurvey)
dim(satsurvey)
str(satsurvey)
View(satsurvey)

# Check for Normality of the data using Q-Q Plot and Shapiro Wilk test
library(ggplot2)
qqnorm(satsurvey$satlevel, col="blue", main = "Q-Q Plot for Sat Level")
qqline(satsurvey$satlevel, col=2) # Data is not normally distributed

shapiro.test(satsurvey$satlevel) 
# Inference: With p-value = 6.048e-05, p<0.05 and normality assumption is rejected

# 2. Find median satisfaction level for ‘IT’, ‘Sales’ and ‘Finance’.
median(satsurvey$satlevel[satsurvey$dept=="IT"]) # 3
median(satsurvey$satlevel[satsurvey$dept=="SALES"]) # 3
median(satsurvey$satlevel[satsurvey$dept=="FINANCE"]) # 4
med<-aggregate(satlevel~dept, data = satsurvey, FUN = median)
med # Finance = 4, IT = 3, Sales = 3

# Test whether the satisfaction level among three roles differ significantly
kruskal.test(formula = satlevel~dept, data=satsurvey)
# Inference: With p-value = 2.883e-06, p<0.05 and H0 is rejected, which
# implies that satlevel across the three departments differ significantly 

# 3. Is there any association between satisfaction level and experience level? 
# Experience level is defined as mid-level (greater than 2 years) and 
# Junior level (less than or equal to 2 years).
library(gmodels)

CrossTable(satsurvey$satlevel, satsurvey$exp, prop.c = FALSE, 
           prop.chisq = FALSE, chisq = TRUE)

chisq.test(table(satsurvey$satlevel, satsurvey$exp))
# Inference: Since p =  p =  0.167325, p>0.05 and H0 is accepted, which 
# implies that there is no association btw satlevel and experience level 

# 4. Find number of employees with satisfaction score greater than 3 in each department
satlevel_grt_3<-table(satsurvey$dept, satsurvey$satlevel>3)
satlevel_grt_3
# Inference: Number of employees with satlevel score greater than 3 across 
# all the departments are as follow: Finance = 16, IT = 4, Sales = 2
