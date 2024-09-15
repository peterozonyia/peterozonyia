# Assignment 3_Non Parametric tests I
# 1. Import NPRS DATA and name it as pain_nprs.
pain_nprs <- read.csv(file.choose(), header = TRUE)
head(pain_nprs, 16)
tail(pain_nprs, 16)
str(pain_nprs)

# Find median NPRS before and after TREATMENT
median(pain_nprs$NPRS_before)
median(pain_nprs$NPRS_after)
med<-aggregate(cbind(NPRS_before,NPRS_after)~Group,data=pain_nprs,FUN=median)
med

# Check for normality of the DATA using both graphical and numerical tests
library(ggplot2)
qqnorm(pain_nprs$NPRS_before, col="blue", main = "Q-Q Plot for NPRS_Before")
qqline(pain_nprs$NPRS_before, col=2) # non linear; data is not normally distributed 

shapiro.test(pain_nprs$NPRS_before) 
# Inference: At p-value = 0.003973, H0 is rejected that data is not normally distributed

library(nortest)
lillie.test(pain_nprs$NPRS_before)
# Inference: Given confirmation of non normality, subsequent tests will be non-parametric tests.
# Eg., Wilcoxon Signed Rank Test for paired groups as in a treatment trial.

# 2. Is post treatment NPRS score significantly less as compared to 
# ‘before treatment’ NPRS score for Group A?
# H0: Ua-Ub=D=0; i.e., there is no significant difference btw before & after treatment
# Alternative = "greater"; H1: !=0. Expected median of the difference is greater zero
wil_test_A<-wilcox.test(pain_nprs$NPRS_before[pain_nprs$Group=="A"],
            pain_nprs$NPRS_after[pain_nprs$Group=="A"],
            paired = TRUE, alternative = "greater")
wil_test_A
# Inference: With p-value = 0.005199, H0 is rejected as p<0.05, which implies
# that pain level decreased after the 3 days of treatment. The post treatment 
# NPRS score was significantly less than before treatment score for Group A.

# 3. Is post treatment NPRS score significantly less as compared to ‘before treatment’ NPRS score for Group B?
wil_test_B<-wilcox.test(pain_nprs$NPRS_before[pain_nprs$Group=="B"],
            pain_nprs$NPRS_after[pain_nprs$Group=="B"],
            paired = TRUE, alternative = "greater")
wil_test_B
# Inference: With p-value = 0.0003079, H0 is rejected as p<0.05, which implies
# that pain level decreased after the 3 days of placebo treatment. The post treatment 
# NPRS score was significantly less than before treatment score for Group B.

# 4. Is the change in NPRS for group ‘A’ significantly different than group ‘B’?
# First create new variable named change_in_NPRS
pain_nprs$change_in_NPRS <- pain_nprs$NPRS_before - pain_nprs$NPRS_after
head(pain_nprs) 
group_A<-pain_nprs$change_in_NPRS[pain_nprs$Group=="A"]
group_A
group_B<-pain_nprs$change_in_NPRS[pain_nprs$Group=="B"]
group_B

# 2ndly check normality of the newly created change_in_NPRS for groups A & B
shapiro.test(pain_nprs$change_in_NPRS) # non normality confirmed, since p<0.05
lillie.test(pain_nprs$change_in_NPRS) # non normality also confirmed since p<0.05

# Given that change_in_ NPRS is non normally distributed, Mann Whitney test is used.
# H0: Ua-Ub=D=0, no difference in change_in_NPRS between Group A and Group B
# Alternatively, H1 != H0, there is significant difference btw Group A and B
man_test_AB <- wilcox.test(change_in_NPRS~Group, data=pain_nprs) 
man_test_AB

man_test_AB <- wilcox.test(pain_nprs$change_in_NPRS[pain_nprs$Group == "A"],
                           pain_nprs$change_in_NPRS[pain_nprs$Group == "B"])
man_test_AB
# Inference: With output p-value of  p-value = 0.378, p>0.05 we accept H0
# Average change in pain level for Group A is not significantly more than Group B. 
# Hence, there is no difference btw the drug and the placebo treatments

#  5. Present change in NPRS for each group using box-whisker plot.

boxplot(group_A,group_B, names = c("Group A", "Group B"),
        col = c("blue", "red"),
        ylab = "Change in NPRS",
        main = "Boxplot of Change in NPRS for Group A and Group B")

boxplot(pain_nprs$change_in_NPRS ~ pain_nprs$Group=="A", col="blue",
        xlab = "Group", ylab = "Change in NPRS", main = "Boxplot of Group A")

boxplot(pain_nprs$change_in_NPRS ~ pain_nprs$Group=="B", col="red",
        xlab = "Group", ylab = "Change in NPRS", main = "Boxplot of Group B")
