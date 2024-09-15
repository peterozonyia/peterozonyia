# Assignment 2_One way AnOVA Test using Marketing Campaign Dataset

# Import New & Usual Campaigns dataset re-created as MARKET RESEARCH CAMPAIGN data
salesgrowth <- read.csv(file.choose(), header = TRUE)
head(salesgrowth, 12) # Data representing the variable New_Campaign
tail(salesgrowth, 12) # Data representing the variable Usual_Campaign
dim(salesgrowth)
str(salesgrowth)

# Is the new campaign more effective than usual campaign?
# Null Hypothesis H0: Ua = Ub (ie, means of various groups (new_campaign & usual_campaign) are equal), 
# where Ua is average mean for New_Campaign group & Ub is mean for Usual_Campaign group. 
# Alternative hypothesis H1 != H0. At least one sample means differs from the other. 
# H0 is rejected if output p-value is < 0.05.

# ONE WAY ANOVA Table function
anovatable <- aov(formula = Growth ~ Campaign, data = salesgrowth)
summary(anovatable)
# Inference: H0: Ua=Ub; alternatively, H1 != H0 
# Given our one factor variable 'Campaign' with two factorial levels:
# 'New_Campaign' and 'Usual_Campaign', the test output p-value = 0.0107. 
# As p-value is < 0.05, H0 is rejected, which implies that there is 
# significant difference in means. Hence, New_Campaign is more effective 
# than Usual_Campaign. Result also suggests some of the group means are 
# different and we don't know those pairs of groups in 'One Way ANOVA Test'. 
# This requires further testing for other covariables (eg, Zone) in the data.

# Testing the effect of Zonewise factor using TWO WAY ANOVA Table function
anovatable <- aov(formula = Growth ~ Campaign, data = salesgrowth)
summary(anovatable)
# Alternative formular
res.aov <- aov(Growth ~ Campaign*Zone, data = salesgrowth)
summary(res.aov)
# Inference: In testing 3 variables H0: Ua=Ub=Uc; alternatively, H1 !=H0
# With output p-value of 0.009955, 'Campaign' is statistically significant as p < 0.05.
# But p-value of 0.18759 for 'Zone' and p-value of 0.36537 for 'Campaign*Zone':
# H0 is not rejected, as p > 0.05.Thus, not statistically significant at the 
# 5% critical level but significant at 20% & 40% with zone > campaign*zone respectively.

# More further testing also using Multiple Tukey Pairwise Comparisons
install.packages("agricolae")
library(agricolae)

TukeyHSD(res.aov)
# Inference: The Multiple Tukey Pairs Comparisons output produced a p-value 
# of 0.0095539 for 'Campaign' factor, which proves that new-campaign is more
# effective than the usual-campaign on sales_growth.However, there is 
# no significance when campaign factor is compared with zone factor as p>0.05 
# So, H0 is not rejected. But within Zones: West-South is more significant 
# at 20% than South-East at 60% and West-East at 70% adjusted critical levels.
