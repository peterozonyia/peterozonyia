# Import adjusted NPRS Data
adjusted_nprsdata <- read.csv(file.choose(), header = T)
head(adjusted_nprsdata)

qqnorm(adjusted_nprsdata$NPRS_before)
shapiro.test(adjusted_nprsdata$NPRS_before) # NON NORMALITY IS CONFIRMED

# 2. Is post treatment NPRS score significantly less as compared to 
# ‘before treatment’ NPRS score for Group A?
# H0: Ua-Ub=D=0; i.e., there is no significant difference btw before & after treatment
# Alternative = "greater"; H1: !=0. Expected median of the difference is greater zero
wil_test_A<-wilcox.test(adjusted_nprsdata$NPRS_before[adjusted_nprsdata$Group=="A"],
                        adjusted_nprsdata$NPRS_after[adjusted_nprsdata$Group=="A"],
                        paired = TRUE, alternative = "greater")
wil_test_A
# Inference: With p-value = 0.0002984, H0 is rejected as p<0.05, which implies
# that pain level decreased after the 3 days of treatment. The post treatment 
# NPRS score was significantly less than before treatment score for Group A.

# 3. Is post treatment NPRS score significantly less as compared to 
# ‘before treatment’ NPRS score for Group B?
wil_test_B<-wilcox.test(adjusted_nprsdata$NPRS_before[adjusted_nprsdata$Group=="B"],
                        adjusted_nprsdata$NPRS_after[adjusted_nprsdata$Group=="B"],
                        paired = TRUE, alternative = "greater")
wil_test_B
# Inference: With p-value = 0.0003079, H0 is rejected as p<0.05, which implies
# that pain level decreased after the 3 days of placebo treatment. The post treatment 
# NPRS score was significantly less than before treatment score for Group B.

# Create new variable change_in_NPRS
adjusted_nprsdata$change_in_NPRS <- adjusted_nprsdata$NPRS_before - adjusted_nprsdata$NPRS_after
head(adjusted_nprsdata)

group_A<-adjusted_nprsdata$change_in_NPRS[adjusted_nprsdata$Group=="A"]
group_A
group_B<-adjusted_nprsdata$change_in_NPRS[adjusted_nprsdata$Group=="B"]
group_B

# Check for normality of the new variable change_in_NPRS
shapiro.test(adjusted_nprsdata$change_in_NPRS) # non normality confirmed
library(nortest)
lillie.test(adjusted_nprsdata$change_in_NPRS) # non normality also confirmed

# Given that change_in_ NPRS is non normally distributed, Mann Whitney test 
# is used. H0: no difference in change_in_NPRS between Group A and Group B
# Alternatively, H1 != H0, there is significant difference btw Group A and B
mann_test_AB <- wilcox.test(change_in_NPRS~Group, data=adjusted_nprsdata) 
mann_test_AB 

mann_test_AB <- wilcox.test(adjusted_nprsdata$change_in_NPRS[adjusted_nprsdata$Group == "A"],
                           adjusted_nprsdata$change_in_NPRS[adjusted_nprsdata$Group == "B"])
mann_test_AB
# Inference: With output  p-value = 0.5686, p>0.05 we accept the null hypothesis.
# Hence, change in pain level for Group A is not significantly more than Group B. 
# Conclusively, non significant change btw the two groups is not surprising 
# because the Wilcoxon test showed that the placebo also was effective post treatment.
# Also, there was not much difference in significant results btw A  & B.   

# Present the change in pain level for each group using box-whisker plot.
boxplot(adjusted_nprsdata$change_in_NPRS ~ adjusted_nprsdata$Group == "A", COL="blue",
        xlab = "GROUP", ylab = "Change in NPRS", main = "Boxplot for Group A")

boxplot(adjusted_nprsdata$change_in_NPRS ~ adjusted_nprsdata$Group == "B", col="red",
        xlab = "GROUP", ylab = "Change in NPRS", main = "Boxplot for Group B")

boxplot(group_A,group_B, names = c("Group A", "Group B"),
        col = c("blue", "red"),
        ylab = "Change in NPRS",
        main = "Boxplot of Change in NPRS for Group A and Group B")

