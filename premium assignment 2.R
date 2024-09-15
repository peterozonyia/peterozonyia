# Import Premiums data and name it as Premium
Premium <- read.csv(file.choose(), header = TRUE)

# check structure of Premium data using str function
str(Premium)

# Check number of rows, columns in the data
dim(Premium)

# Display first 10 rows and last 5 rows 
head(Premium, 10)
tail(Premium, 5)

# Describe (summarize) all variables
summary(Premium)

# loading dplyr
library(dplyr)

Premium_sorted<-Premium %>% arrange(desc(Premium))
Premium_sorted

# Display top 5 and bottom 5 policies in terms of premium amount
top5 <- head(Premium_sorted, 5)
top5
bottom5 <- tail(Premium_sorted, 5)
bottom5

# Calculate the sum for variable ‘Sum_Assured’ by ‘Region’ variableS
aggregate(Premium_sorted$Sum_Assured, by=list(Premium_sorted$REGION), FUN=sum)

# Also using group_by function of dplyr
reg<-group_by(Premium_sorted, REGION)
dplyr_agg<-summarise(reg, sum=sum(Sum_Assured, na.rm = TRUE))
dplyr_agg

# Create a subset of policies of Asia Standard Plan with Sum_Assured < = 50,000.
## Keep variables Policy_No, Zone_name, Plan and Sum_Assured in the subset data
Premium2<-subset(Premium, Plan=="Asia Standard Plan" & Sum_Assured < 50,000,
                 select=c(POLICY_NO, ZONE_NAME, Plan, Sum_Assured))
Premium2

# Export the subsetted data into an xlsx file.
install.packages("xlsx")
library(xlsx)

write.xlsx(Premium2, file = "c:/Premium2.xlsx")

write.csv(Premium2, file = "Premium2.csv")
