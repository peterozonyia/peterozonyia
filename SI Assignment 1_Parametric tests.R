# Assignment 1_Parametric tests

# Import VAS DATA and name it as pain_vas.
pain_vas <- read.csv(file.choose(), header = TRUE)
pain_vas
head(pain_vas, 16) # Group A observed values from receiving the test drug
tail(pain_vas, 16) # Group B observed values from receiving the placebo

# Check for normality of the DATA using both graphical and numerical tests
library(ggplot2)

# Q-Q Plot graphic
qqnorm(pain_vas$VAS_before,col="red", main = "Q-Q Plot for VAS_Before")
qqline(pain_vas$VAS_before, col=2) # Linearity confirms normal distribution 
qqnorm(pain_vas$VAS_after, col="blue", main = "Q-Q Plot for VAS_After")
qqline(pain_vas$VAS_after, col=2) # Lacks linearity and thus non normality

# Shapira-Wilk Test
shapiro.test(pain_vas$VAS_before) # At p-value = 0.7822, H0 is accepted for normality

# Is post treatment VAS score significantly less as compared to 
# ‘before treatment’ VAS score for Group A?
# Using Paired t-test to determine Change in Pain Level, 
# where H0: U1-U2=D=0 implies there is no difference btw Before & After VAS Scores
# Alternatively, H1: there is significant difference in VAS scores Before & After
# That is, there should be greater improvement post treatment 

t_test_A <- t.test(pain_vas$VAS_before[pain_vas$Group=="A"],
                   pain_vas$VAS_after[pain_vas$Group=="A"],
                   alternative="greater", paired=TRUE) 
t_test_A

# Inference: With output p-value = 2.111e-09, P<0.05, H0 is rejected
# There is significant difference btw VAS Scores Before and After
# Group A's "Change in Pain Level" decreased significantly after treatment
# Hence, treatment was effective.

# Is post treatment VAS score significantly less as compared to 
# ‘before treatment’ VAS score for Group B? 
t_test_B <- t.test(pain_vas$VAS_before[pain_vas$Group=="B"],
                   pain_vas$VAS_after[pain_vas$Group=="B"],
                   alternative="greater", paired=TRUE) 
t_test_B
# Inference: With output p-value = 0.01419, P<0.05, H0 is rejected
# There is significant difference btw VAS Scores Before and After in Group B.
# VAS Scores decreased after receiving the Placebo.

# Is the average change in pain level for group ‘A’ significantly 
# more than group ‘B’? # H0: Ua-Ub=D=0, no difference btw Group A and Group B
# Alternatively, H1 != H0, there is significant difference btw Group A and B
t_test_AB <- t.test(pain_vas$VAS_after[pain_vas$Group == "A"],
                    pain_vas$VAS_after[pain_vas$Group == "B"])
t_test_AB
# Inference: With output p-value of 7.946e-09, p<0.05 and so we reject H0
# Average change in pain level for Group A is significantly more than Group B. 

# Present change in pain level for each group using box-whisker plot.
boxplot(pain_vas$VAS_after ~ pain_vas$Group == "A", COL="darkorange",
xlab = "GROUP", ylab = "Change in Pain Level", main = "Boxplot for Group A")

boxplot(pain_vas$VAS_after ~ pain_vas$Group == "B", col="blue",
xlab = "GROUP", ylab = "Change in Pain Level", main = "Boxplot for Group B")
 