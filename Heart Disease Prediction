# Project Title: 
# Predicting the Risk of Heart Disease; A Comparison of Binary Logistic Regression, Naïve Bayes, Decision Tree, and Random Forest

# Importing Libraries
library(dplyr)  # for data manipulation
library(caret)  # for one-hot encoding
library(tidyr)
library(knitr)
library(ggplot2)
library(kableExtra)
library(car)
library(e1071)
library(stats)
library(imbalance)
library(corrplot)
library(goftest)
library(tidyverse)
library(conflicted)
library(purrr)
library(rlang)
library(reshape2)
library(fastDummies)
library(pROC)
library(ROCR)
library(shiny)
library(party)
library(randomForest)
library(DT)


# Import Project's Four Datasets
behaviour_data <- read.csv(file.choose(), header = T)
demographics_data <- read.csv(file.choose(), header = T)
heart_disease_data <- read.csv(file.choose(), header = T)
medical_history_data <- read.csv(file.choose(), header = T)

# Exploring Data Structures; Columns Variables and Types; Summary Statistics
# Also Check Column names (variable names) to ensure proper naming and no spacing

# EDA for Behaviour Dataset
head(behaviour_data) # Column names look good
str(behaviour_data) # Contains 319795 obs. and  6 variables (chr & Int)
summary(behaviour_data) # Only SleepTime (integer) produced descriptive stats
any_missing_values <- anyNA(behaviour_data)
any_missing_values # No missing values
any_duplicates <- duplicated(behaviour_data)
head(any_duplicates)
summary(any_duplicates) # No duplicates

# EDA for Demographics Dataset
head(demographics_data) # Column names look good
str(demographics_data) # Contains 319795 obs. and  5 variables (chr & num)
summary(demographics_data) # Only BMI (numeric) produced descriptive stats
any_missing_values <- anyNA(demographics_data)
any_missing_values # No missing values
any_duplicates <- duplicated(demographics_data)
summary(any_duplicates)

# EDA for Heart_Disease Dataset
head(heart_disease_data) # Column names look good
str(heart_disease_data) # Contains 319795 obs. and  2 variables (chr)
summary(heart_disease_data)
any_missing_values <- anyNA(heart_disease_data)
any_missing_values # No missing values
any_duplicates <- duplicated(heart_disease_data)
summary(any_duplicates) # No duplicates

# EDA for Medical_History Dataset
head(medical_history_data) # Column names look good
str(medical_history_data) # Contains 319795 obs. and  9 variables (chr & num)
summary(medical_history_data) # Only MentalHealth & PhysicalHealth (on numeric scales) produced descriptive stats.
any_missing_values <- anyNA(medical_history_data)
any_missing_values # No missing values
any_duplicates <- duplicated(medical_history_data)
summary(any_duplicates)

# Comments: Given the above informations, categorical variables will be re-categorised and ecoded at 
# some stage to improve interpretability. The non-numeric variables will be converted (type_convert) and 
# dumied using 'One-Hot Encoded' for easy manipulation and analysis. Most importantly, the four datasets 
# will be merged by common Column name 'pid' (Patient ID) and then pre-proccessed thoroughly as required.

# Combining together the four (4) datasets based on common 'pid' column using the full_join() function
heartdata <- full_join(behaviour_data, demographics_data, by = "pid") %>%
  full_join(., medical_history_data, by = "pid") %>%
  full_join(., heart_disease_data, by = "pid")
head(heartdata)
str(heartdata) # Data Types: 15 variables = Charactrer; 3 variables = Integer; BMI = numeric 
dim(heartdata)# There are 319795 observations and 19 variables including the 'pid' (patient ID)
summary(heartdata) # Gives the mean/median of all the four numeric variables
duplicates <- duplicated(heartdata)
sum(duplicates) # There are no duplicates (0)
sum(is.na(heartdata)) # There are no Missing Values (0)


# EDA & PRE-PROCESSING OF THE COMBINED MASTER DATASET ('HEARTDATA')  

# Check the data types of all variables
data_types <- sapply(heartdata, typeof)
data_types

# Filter only numerical variables (integer and double)
numerical_vars <- names(data_types)[data_types %in% c("integer", "double")]
numerical_vars # Numeric Variables: "SleepTime", "BMI", "PhysicalHealth", "MentalHealth"

# Create summary statistics for numerical variables
summary_numerical_vars <- heartdata %>% 
  select(one_of(numerical_vars)) %>%
  summary()
summary_numerical_vars
# comment: This reveals important information about distribution ranges (min, median/mean, max) 
# for these numeric variables, which encourage recoding based on domain knowledge/source codebook.
# For 'SleepTime' average median/mean = 7, which is around 'normal sleep', suggesting also that 
# majority of the people are represented mostly in this group. This is confirmed in the recoded 
# SleepTimeCat levels & distributed as: 'VeryShort' (11081); 'Short' (85905); 'Optimal' (211394); 
# 'Long' (11415) - with more people in 'optimal' (i.e., 'normal sleep') level. 
# The BMI's average mean = 28 (or median = 27) is above recommended healthy BMI range (18.5-24.9),
# which suggests that majority of the people are overweight and/or obese with BMI >25 and BMI >30
# respectively. Given domain knowledge, this majority with high BMI are more at risk of having
# heart disease. And when BMI was recoded as BMICat variable, the overweight and obese are over-
# represented in the data as the distribution shows: Underweight (5114); Normal weight (95134);
# Overweight (114752); Obesity (104795). The mean for PhysicalHealth = 3.4 days of poor physical 
# health with the data skewed to the right and majority over-represented within 0 and < 7 days 
# & when recoded distribution shows: 'None' (226589), Few (51959), Some (10231) and Many (31016).
# The mean for MentalHealth = 4 days of poor mental health with the data skewed to the right and 
# majority are over-represented within 0 and < 7 days of poor mental health in a 30-day period.
# In other words, people having poor mental health for many more days are fewer in the dataset 
# compared to the majority over-represented within 'None' (205401), 'Few' (62818), Some (14808),
# & Many (36768). Interestingly, people experiencing several days of poor physical health and/or 
# poor mental health are more likely to suffer heart disease compared to those having zero and 
# fewer days of poor physical/mental health issues. The current dataset is highly imbalanced and 
# over-representated by healthy people without heart disease, which explains why the data is highly 
# skewed to the right with majority distribution within the zero scores and fewer days (< 5 days). 


# Visualise the numerical variables with Histogram plot
variables <- c("SleepTime", "BMI", "PhysicalHealth", "MentalHealth")

# Create and print histograms for each variable
ggplot(heartdata, aes(x = SleepTime)) + 
  geom_histogram(binwidth=1,fill="blue", color="black", alpha=0.7) + 
  labs(title = "Distribution of Sleep Time", x = "Sleep Time (hours)", y = "Frequency") +
  theme_bw() 
ggplot(heartdata, aes(x = BMI)) + 
  geom_histogram(binwidth=1,fill="blue", color="black", alpha=0.7) + 
  labs(title = "Distribution of BMI", x = "BMI", y = "Frequency") +
  theme_bw()
ggplot(heartdata, aes(x = PhysicalHealth)) + 
  geom_histogram(binwidth=1, fill="blue",color="black", alpha=0.7) + 
  labs(title = "Distribution of PhysicalHealth", x = "PhysicalHealth", y="Frequency") + 
  theme_bw()
ggplot(heartdata, aes(x = MentalHealth)) + 
  geom_histogram(binwidth=1, fill="blue",color="black", alpha=0.7) + 
  labs(title = "Distribution of MentalHealth", x = "MentalHealth", y="Frequency") + 
  theme_bw() 
# Comment: The histogram for the 'SleepTime variable' reveals normal distribution with high concentration 
# mostly around the 'normal' recommended sleeping times (7-8 hours); but skewed both to the 'shorter' 
# sleeping times (< 7 hours) and 'longer' sleeping times (> 8 hours), which have serious implications for 
# risk of having heart disease. The BMI (Body Mass Index) graph shows a high concentration beyond the 
# critical zone recommended as healthy BMI range (18-25 BMI) and thus highly skewed to the left, which 
# also suggests more people with very 'unhealthy' BMI scores. The physicalHealth shows that its data 
# distribution is skewed to the right with majority within < 5 days of poor physical health in 30 days 
# period. The distribution shows highest spike at None = 0 day of poor physical health, which is 
# expected given that the data is over-represented by healthy people with no heart disease. The 
# histogram for MentalHealth is also similar with distribution skewed to the right with the highest 
# spike at zero day of poor mental health and less likely to have heart disease. These observations 
# will be further analysed to understand their relationships with other variables.

# Filter columns with character data type
categorical_cols <- sapply(heartdata, is.character)
# Select the column names based on the logical vector
categorical_cols <- names(heartdata)[categorical_cols]
categorical_cols
# Data Type for 15 variables is 'Character' (categorical or label):
# "pid", "Smoking", "AlcoholDrinking", "DiffWalking", "PhysicalActivity", "Sex", "AgeCategory", 
# "Race", "GenHealth", "Asthma", "KidneyDisease", "SkinCancer", "Stroke", "Diabetic", "HeartDisease"  

# Define function to print frequency tables with 'counts' & 'percentages' using the 'knitr' & 'kableExtra' libraries
print_table <- function(heartdata, column_name) {
  cat("\nFrequency Table for", column_name, ":\n\n")
  table_data <- as.data.frame(table(heartdata[[column_name]]))
  colnames(table_data) <- c("Value", "Count")
  table_data$Percentage <- round(100 * table_data$Count / sum(table_data$Count), 2)
  
  table_data %>%
    kable(format = "html", align = 'c', col.names = c("Value", "Count", "Percentage (%)")) %>%
    kable_styling("striped", full_width = F) %>%
    column_spec(1, bold = T) %>%
    column_spec(2, width = "3em") %>%
    column_spec(3, width = "5em") %>%
    row_spec(0, bold = T, color = "white", background = "#D7261E") %>%
    print()
}

# List of categorical variables
categorical_vars <- c("Sex", "AgeCategory", "Race", "GenHealth", "Asthma", "KidneyDisease", 
                      "SkinCancer", "Stroke", "Diabetic", "HeartDisease", "Smoking", 
                      "AlcoholDrinking", "DiffWalking", "PhysicalActivity")

# Print frequency tables for all categorical variables
for (var in categorical_vars) {
  print_table(heartdata, var)
}
# Comment: The frequency tables for Sex is Female=167805	(52.47%), Male=151990	(47.53%). Though
# faily balanced data, yet more female than male. AgeCategory is 18-24=21064	(6.59%), 25-29=16955	
# (5.30%), 30-34=18753	(5.86%), 35-39=20550	(6.43%), 40-44=21006	(6.57%), 45-49=21791	(6.81%),
# 50-54=25382	(7.94%), 55-59=29757	(9.31%), 60-64=33686	(10.53%), 65-69=34151	(10.68%),
# 70-74=31065	(9.71%), 75-79=21482	(6.72%), 80 or older=24153	(7.55%); Race is American Indian/
# Alaskan Native=5202	(1.63%), Asian=8068	(2.52%), Black=22939	(7.17%), Hispanic=27446	(8.58%), 
# Other=10928	(3.42%) & White=245212	(76.68%); GenHealth is Excellent=66842	(20.90%), Fair=34677
# (10.84%), Good=93129	(29.12%), Poor=11289 (3.53%) & Very good=113858	(35.60%); Asthma shows 
# No=276923	(86.59%) & Yes=42872	(13.41%); KidneyDisease shows No=308016	(96.32%) & Yes=11779	
# (3.68%); SkinCancer is No=289976	(90.68%) & Yes=29819	(9.32%); Stroke is No=307726	(96.23%) 
# & Yes=12069	(3.77%); Diabetic is No=269653	(84.32%), No/borderline diabetes=6781	(2.12%), 
# Yes=40802	(12.76%) & Yes (during pregnancy)=2559	(0.80); HeartDisease is No=292422	(91.44%) & 
# Yes=27373	(8.56); Smoking is No=187887	(58.75%) & Yes=131908	(41.25%); AlcoholDrinking shows 
# No=298018	(93.19%) & Yes=21777	(6.81%); DiffWalking is No=275385	(86.11%) & Yes=44410	(13.89%); 
# PhysicalActivity is No=71838	(22.46%) & Yes=247957 (77.54).


# Visualise the Target Variable 'HeartDisease' with Pie Chart graph
# Count the occurrence of HeartDisease for the population
disease_counts <- table(heartdata$HeartDisease)

# Calculate percentages
percentages <- round(100 * prop.table(disease_counts), 1)

# Create the labels for both category_names and percentages
labels <- paste(names(disease_counts), "-", percentages, "%", sep = "")

# Create Pie Chart for HeartDisease (No, Yes) in Percentages
pie(disease_counts,
    labels = labels,
    col = c("blue", "red"),# Blue for the 'no HeartDisease' and Red for the 'yes HeartDisease
    main = "Pie Chart of Heart Disease Presence")
# Add legend
legend("topright", 
       legend = labels, 
       fill = c("blue", "red"))


# Computing the Chisq.test with original variables before any modification, transformation and recoding

# Check relationship between 'HeartDisease' (dependent variable) and all Independent variables
ncol(heartdata) # Total of 19 columns
column_indices <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
# Loop through each column index
for (index in column_indices) {
  test_result <- chisq.test(heartdata$HeartDisease, heartdata[, index], correct = FALSE)
  print(test_result)
}
# Comment
# With caution, all the p_values are less than 0.05 at a p_value < 2.2e-16 (i.e., almost 0). This means 
# that the Null Hypothesis of 'no association between variables' is strongly rejected. Rather there exist 
# very strong statistically significant association between the target variable (HeartDisease) and all 
# the 'feature variables' in the dataset.

# Alternatively, create function of Chi-square test for the categorical variables
chi_square_test <- function(heartdata, var) {
  heart_tbl <- table(heartdata[[var]], heartdata$HeartDisease)
  chisq.test(heart_tbl)
}

# Create also function of t-test for numerical variables
t_test <- function(heartdata, var) {
  t.test(heartdata[[var]] ~ heartdata$HeartDisease)
}

# Define the categorical variables
cat_vars <- c("Sex", "AgeCategory", "Race", "GenHealth", "Asthma", "KidneyDisease", "SkinCancer", 
              "Stroke", "Diabetic", "Smoking", "AlcoholDrinking", "DiffWalking", "PhysicalActivity")

# Define the numerical variables
num_vars <- c("SleepTime", "BMI", "PhysicalHealth", "MentalHealth")

# Generating Chi-square tests for categorical variables
chi_square_results <- lapply(cat_vars, function(var) {
  result <- chi_square_test(heartdata, var)
  data.frame(Variable = var, p_value = result$p.value)
})

# Combine results into a single data frame
chi_square_results <- bind_rows(chi_square_results)

# T-tests for numerical variables
t_test_results <- lapply(num_vars, function(var) {
  result <- t_test(heartdata, var)
  data.frame(Variable = var, p_value = result$p.value)
})

# Combine results into a single data frame
t_test_results <- bind_rows(t_test_results)

# Print the Chi-square test results for categorical variables
cat("Chi-square test results for categorical variables:\n")
print(chi_square_results)
# Interpretation: All categorical variables have highly significant association with the HeartDisease
# variable at p_values < 0.05, which make them very important predictors for heart disease risk.
# Some of these categorical variables like the 'AgeCategory variable' will be more useful in interpretatio
# when recoded into new category levels based on domain knowledge especially given the implications of 
# biological and chronological factors in determining risk of Cardiovascular Diseases (CVDs) in both men 
# and women with additional insight into the discriminant female-specific factors like sex hormones and 
# other gynaecological and reproductive conditions.
# Secondly, purposive recoding also aims to shorten number of observations in data, which can help 
# improve interpretability, model performance and testing accuracies.


# Print the t-test results for numerical variables
cat("\nT-test results for numerical variables:\n")
print(t_test_results)
# Interpretation: All the numerical variables also show a highly significant relationship with HeartDisease
# at p_values < 0.05, which make them very important predictors for heart disease risk.
# For better analysis, all the value labels of the numeric variables will be 're-adjusted' based
# on domain knowledge/original codebook. For example, 'SleepTime' will be readjusted as: 'VeryShort' 
# (< 4 hours); 'Short' (< 6 hours); 'Optimal' (7-8 hours); 'Long' (> 9). The 'BMI' as: 'Underweight' 
# (BMI < 18.5); 'Normal weight' (BMI 18.5-25); 'Overweight' (BMI > 25-30); 'Obesity' (BMI > 30). The 
# 'PhysicalHealth' as: 'None' (0 day), 'Few' (<= 7 days), 'Some' (<= 14) and 'Many' (30 days). The 
# 'MentalHealth' also as: 'None' (0 day), 'Few' (<= 7 days), 'Some' (<= 14), and 'Many' (30 days).


# Visualising relationships between HeartDisease and numerical variables using Boxplot
# Define and Concatenate the Numerical Variables
num_vars <- c("SleepTime", "BMI", "PhysicalHealth", "MentalHealth")

for (var in num_vars) {
  heart_box <- ggplot(heartdata, aes_string(x = "HeartDisease", y = var, fill = "HeartDisease")) +
    geom_boxplot() +
    scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
    labs(title = paste("Relationship between HeartDisease and", var)) +
    theme_minimal()
  print(heart_box)
}
# Interpretation: Both the 'no heartdisease' and 'yes haertdisease' show fairly normal distribution 
# around the median (middle line of the box), which indicates that half of the people in each side are 
# distributed above and below the median value with majority around the normal sleep time of 7-8 hours 
# whether they have heart disease or not. However, people that get enough sleep are less likely to have 
# heat disease compared to those who are not getting enough sleep daily. 
# The median of BMI appears to be higher for individuals with heart disease ("Yes") compared to those 
# without ("No"). The IQR is also wider for the "Yes" category, indicating a greater spread of BMI values 
# in that group. Outliers are higher for the "Yes" category, which suggest a possible concentration of 
# more individuals with higher BMI in that group.
# The boxplot for Physical Health shows the median is much higher & far wider spread in the 'Yes 
# Heartdisease' than in the 'No Heartdisease' box. The 'No' heart disease box shows a lower spread 
# with many in this group having very low scores and near zero compared to those having heart disease 
# (Yes). Generally, people who are physically active (i.e., lower days of poor physical health) is 
# less likely to have heart disease (the low scores) compared to those less physically active (i.e., 
# having many days of poor physical health), which are more likely to have heart disease.
# The MentalHealth boxplot shows higher median and wider spread for 'Yes' box compared to the 'No' box.
# Generally, people having many days of poor mental health are more likely to have heart disease 
# compared to those having fewer days of poor mental health issues.


# Visualising relationships between HeartDisease and categorical variables using Bar Plot
for (var in cat_vars) {
  heart_bar <- ggplot(heartdata, aes_string(x = var, fill = "HeartDisease")) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste("Relationship between HeartDisease and", var),
         x = var,
         y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-labels for better readability
  print(heart_bar)
}
# Interpretation: The PhysicalActivity bar plot shows that those who are less active are more at risk 
# of heart disease compared to those more physically active, which confirms that having more days of 
# physical activity monthly may have a protective effect against heart disease (low risk of heart 
# disease). The DiffWalking plot suggests that people having difficulty in walking ('Yes') have high 
# prevalence of heart disease (x4 times) compared to those with 'No' difficulty. The AlcoholDrinking 
# bars suggest that those consuming more alcohol are less likely to suffer heart disease compared to 
# those who are not. However this requires further investigation into the composition and distribution 
# within the data. The Smoking bars show that smoking can influence the risk of heart failure as the 
# 'Yes' smoking have more heart disease than those who do not smoke. The diabetic plot shows that 
# people with diabetes (Yes) have highest cases of heart disease followed by those in 'No borderline 
# diabetes' but lowest in 'diabetics during pregnancy'. The Stroke bar suggests that those without a 
# history of stroke are less likely to suffer heart failure compared to stroke patients. The skin 
# cancer shows that those with skin cancer are more likely to have heart disease compared to those 
# without skin cancer. Kidney disease also shows that those with kidney disease are more likely to 
# have heart disease compared to those without kidney disease.The Asthma plot shows that those with 
# asthma are more likely to experience heart failure compared to those without. In the general health,
# people with 'poor' general health are more likely to have heart disease while those at 'excellent' 
# with highest GenHealth less likely for heart disease. The race plot shows that American Indian / 
# Alaska Native have the highest heart disease compared to white, other, black, Hispanic and Asians 
# being the least likely to have hearth disease. The age bar shows a left skewed distribution with 
# older people aged 80 years and above being more likely to have heart disease compared to younger 
# age brackets with those less than 24 years having no heart disease. The sex bar shows that males 
# are doubly more likely to have heart disease compared to females in this data.


# PHASE ONE:
# # TRANSFORM AND RECODE SOME OF THE ORIGINAL VARIABLES BEFORE ENCODING FOR MODEL BUILDING

# Check the data structure for data types
str(heartdata) # Data Types: 15 variables = Charactrer; 3 variables = Integer; BMI = numeric 

# Save a copy of the merged masterdata 
write.csv(heartdata, "heart_data.csv", row.names = FALSE)

# Recode GenHealth (chr) using factor() to re-order its levels accordingly before ordinal encoding
# Ensuring GenHealth is a factor
heartdata$GenHealth <- as.factor(heartdata$GenHealth)
# Checking the current levels as stored in the dataset df
print(levels(heartdata$GenHealth)) # Stored as: "Excellent","Fair","Good","Poor","Very good"
# Re-order correctly in ascending order the levels of GenHealth variable using the factor()
heartdata$GenHealth <- factor(heartdata$GenHealth,
                              levels = c("Poor", "Fair", "Good", "Very good", "Excellent"))
# Verify reordering is successful by creating the table
genhealth_table <- table(heartdata$GenHealth)
print(genhealth_table) 
#  Poor      Fair      Good Very good Excellent 
# 11289     34677     93129    113858     66842 
# Comment: Reordering was successful with correct counts of each level as stored in the dataset.
# The levels were stored in the dataset as: "Excellent", "Fair", "Good", "Poor", "Very good", 
# which were not arranged in the correct descending order as per the main source codebook from 
# "Excellent", "Very Good", "Good", "Fair", "Poor". It is recoded/re-ordered here in ascending 
# order for better analysis and starts from "Poor", "Fair", "Good", "Very good", "Excellent".

# Recode all 'numerical' variables and create 'new categorical' variables with meaningful levels
heartdata$SleepTimeCat <- cut(heartdata$SleepTime, 
                              breaks = c(-Inf, 4, 6, 9, Inf), 
                              labels = c("VeryShort", "Short", "Optimal", "Long"))

heartdata$BMICat <- cut(heartdata$BMI, 
                        breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), 
                        labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))

heartdata$PhysicalHealthCat <- cut(heartdata$PhysicalHealth, 
                                   breaks = c(-Inf, 0, 7, 14, 30), 
                                   labels = c("None", "Few", "Some", "Many"))

heartdata$MentalHealthCat <- cut(heartdata$MentalHealth, 
                                 breaks = c(-Inf, 0, 7, 14, 30), 
                                 labels = c("None", "Few", "Some", "Many"))

# Counting number of occurrences for all the newly Recoded Numerical Variables
sleeptime_counts <- count(heartdata, SleepTimeCat)
print(sleeptime_counts)
# SleepTimeCat      n
# 1   Very Short  11081
# 2        Short  85905
# 3      Optimal 211394
# 4         Long  11415

bmi_counts <- count(heartdata, BMICat)
print(bmi_counts)
#         BMICat      n
# 1   Underweight   5114
# 2 Normal weight  95134
# 3    Overweight 114752
# 4       Obesity 104795

physicalhealth_counts <- count(heartdata, PhysicalHealthCat)
print(physicalhealth_counts)
# PhysicalHealthCat      n
# 1              None 226589
# 2               Few  51959
# 3              Some  10231
# 4              Many  31016

mentalhealth_counts <- count(heartdata, MentalHealthCat)
print(mentalhealth_counts)
#  MentalHealthCat      n
# 1            None 205401
# 2             Few  62818
# 3            Some  14808
# 4            Many  36768

# Visualising the newly created numerical categories using Bar Plot
# Defining the new categorical variables
new_cat_vars <- c("SleepTimeCat", "BMICat", "PhysicalHealthCat", "MentalHealthCat")

# Create bar plots for the recoded variables
library(ggplot2)

for (var in new_cat_vars) {
  heart_bar <- ggplot(heartdata, aes_string(x = var, fill = "HeartDisease")) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste("Relationship between HeartDisease and", var),
         x = var,
         y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(heart_bar)
}
# Interpretation:
# The new sleeptime bar plot shows that getting 'very short' or 'long' sleep may be associated with 
# a higher risk of heart disease compared to getting optimal sleep. The new BMI shows that as BMI 
# increases from "Underweight" to "Obesity," the proportion of individuals with heart disease also 
# increases. This suggests that individuals with higher BMI are more likely to have heart disease. 
# The new physical health categories show that as the number of days with poor physical health 
# increases from "None" to "Many," the proportion of individuals with heart disease also increases. 
# This suggests that individuals who experience more days of poor physical health are more likely
# to have heart disease. For MentalHealth, there is a positive relationship between number of days with 
# poor mental health and the prevalence of heart disease. As the number of days with poor mental health 
# increases from "None" to "Many," the proportion of individuals with heart disease also increases. This
# clearly suggests that individuals who experience more days of poor mental health are most likely to 
# have heart disease compared to those who experience lower number of days with poor mental health.

# Recode AgeCategory (13 levels) and create NewAgeCategory (6 levels) using factor()
age_levels <- list(
  `18-24` = "18-24",
  `25-29` = "25-34",
  `30-34` = "25-34",
  `35-39` = "35-44",
  `40-44` = "35-44",
  `45-49` = "45-54",
  `50-54` = "45-54",
  `55-59` = "55-64",
  `60-64` = "55-64",
  `65-69` = "65 or older",
  `70-74` = "65 or older",
  `75-79` = "65 or older",
  `80 or older` = "65 or older"
)
heartdata$AgeCategory <- unlist(lapply(heartdata$AgeCategory, function(x) age_levels[[x]]))

# Print the number of occurrences of each new age category
age_counts <- count(heartdata, AgeCategory)
print(age_counts) 
#  AgeCategory      n
# 1       18-24  21064
# 2       25-34  35708
# 3       35-44  41556
# 4       45-54  47173
# 5       55-64  63443
# 6 65 or older 110851

# Check the total columns after creating newly recoded variables
dim(heartdata) # There are now 23 variables

# Drop original numerical columns and rename recoded with original names
heartdata <- heartdata %>% select(-c(SleepTime, BMI, PhysicalHealth, MentalHealth))
# Rename recoded numerical columns to original names
heartdata <- heartdata %>% rename(SleepTime = SleepTimeCat, BMI = BMICat,  
                                  PhysicalHealth = PhysicalHealthCat, MentalHealth = MentalHealthCat)

# Save a copy of recoded 'heartdata' dataframe with the 'pid' for PowerBI Dashboarding 
#write.csv(heartdata, "recoded_heart_data.csv", row.names = FALSE)

dim(heartdata) # Back to the 19 variables and ready for Feature Engineering and encoding
# Comment: The heartdata now has the desired structure with 'all' recoded now renamed orinigal names


# STEP TWO OF FEATURES ENGINEERING:

#ENCODING THE HEARTDATA VARIABLES USING 'dplyr' LIBRARY

# List all variable types for ordinal encoding with their recoded categorical value labels
ordinal_vars <- c("BMI", "GenHealth", "AgeCategory") # Encode levels in ascending order
# Encode ordinal_vars
heartdata[ordinal_vars] <- lapply(heartdata[ordinal_vars], function(x) as.integer(factor(x)))

# List all the binary variables (n=8) and encode with their No == 0 and Yes == 1
binary_vars <- c("Smoking", "AlcoholDrinking", "Stroke", "DiffWalking", "PhysicalActivity", "Asthma", "KidneyDisease", "SkinCancer")
heartdata[binary_vars] <- lapply(heartdata[binary_vars], function(x) ifelse(x == "No",0,1))

# Nominally encode Diabetic using as.factor() with their stored value labels/ordering in DF
heartdata$Diabetic <- as.factor(heartdata$Diabetic)
levels(heartdata$Diabetic) # Stored levels: "No","No, borderline diabetes","Yes","Yes (during pregnancy)" 

# Nominally encode Race using as.factor() with their stored value labels/ordering in DF
heartdata$Race <- as.factor(heartdata$Race)
levels(heartdata$Race) # Stored Levels: "American Indian/Alaskan Native","Asian","Black","Hispanic","Other","White"

# Encode Sex variable with 'Female' value == 0 (originally stored as Female = 1 and Male = 2)
heartdata$Sex <- ifelse(heartdata$Sex == "Female",0,1) # Encode with 'Female' == 0, Male == 1

# Encode 'target variable' (HeartDisease) with "No" == 0 (originally stored "No"= 1, "Yes"= 2) 
heartdata$HeartDisease <- ifelse(heartdata$HeartDisease == "No", 0, 1) # No == 0, Yes == 1
heartdata$HeartDisease <- as.factor(heartdata$HeartDisease)
levels(heartdata$HeartDisease) # levels "0","1" is now encoded as intended

# View the transformed and encoded data
head(heartdata) 
str(heartdata) # The DF "heartdata" is now successfully encoded
dim(heartdata) # 319795 obs and 19 vars


# MODEL BUILDING

#Building Binary Logistic Regression (BLR) Model with the heartdata

# Check the proportion table for the Target Variable (HeartDisease) 
proportions <- prop.table(table(heartdata$HeartDisease))
proportions 
# Comment: The 'Yes HeartDisease' = 8.6% (0.08559) whereas the 'No HeartDisease' = 91% (0.91441).
# This shows that the data is highly imbalanced with serious implications for modelling performance.

# Load necessary libraries
library(data.table)
library(ROSE)
library(caret)

# Spliting 'heartdata' into Training (80%) and Testing Sets (20%)
set.seed(134679)  # For reproducibility
index <- createDataPartition(heartdata$HeartDisease, p = 0.8, list = FALSE)
traindata <- heartdata[index, ]
testdata <- heartdata[-index, ]

# Setting the threshold
threshold <- 0.50

# Proportions of Traindata and Testdata Subsets
dim(traindata) # 80% (255837)
dim(testdata) # 20% (63958)


# 1. BALANCED_DATA WITH UNDER-SAMPLING METHOD

# Convert traindata to data.table for efficient processing
traindata_dt <- as.data.table(traindata)

# Check class distribution in traindata (80% portion only)
class_counts_train <- table(traindata_dt$HeartDisease)
print("Class distribution in traindata before under-sampling:")
print(class_counts_train) # 0 = 233938 and 1 = 21899 

# Get the size of the minority class
minority_class_size_train <- min(class_counts_train)
print(paste("Minority class size:", minority_class_size_train)) # "Minority class size: 21899"

# Get the size of the majority class
majority_class_size_train <- max(class_counts_train)
print(paste("Majority class size:", majority_class_size_train)) # "Majority class size: 233938"

# Perform Under-sampling
set.seed(123)
balanced_data <- ovun.sample(
  HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + BMI + 
    Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
    SkinCancer + Stroke + Diabetic + Race, 
  data = traindata_dt, 
  method = "under", 
  N = 2 * minority_class_size_train
)$data

# Verifying the result
balanced_class_counts <- table(balanced_data$HeartDisease)
print("Class distribution after under-sampling:")
print(balanced_class_counts) 
# Comment: Because the Target Variable is significantly imbalanced with the 'No' class of 233938 
# being 9x the 'Yes' class of 21899, the 'Under-Sampling' method was prioritised over the 'Over-
# Sampling' approach. The arget variable is now balanced: No = 21899, Yes = 21899 but resulted to 
# a very small sample size of just 43798 obs. compare to the 319795 originally. Therefore, 'over-
# sampling' method and a combination of both will be implemented also to check which method is 
# better. And given the complexity/sensitivity of the healthcare data, over-sampling method helps
# prevent the loss of potential information that arises with the reduction of majority class.

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit the logistic regression model
set.seed(234)
blr_model <- glm(formula = model, family = binomial(link = "logit"), data = balanced_data)

# Display the summary of the model
summary(blr_model1)
# Interpretation: Almost all the variables are highly significant predictors of heart disease 
# except the 'PhysicalActivity' and some categories of other variables: PhysicalHealthMany, 
# MentalHealthSome, MentalHealthMany, DiabeticYes (during pregnancy), RaceOther are insignificant. 
# The categorical variables reveal the categories that are more significant or more insignificant 
# compared to their reference category. For instance, Sex variable confirms that the male sex is 
# at a higher risk compare to the female sex. In other words, men are more at a higher risk of 
# suffering heart disease, which is expected based on domain knowledge. Being a smoker (Smoking) 
# significantly increases likelihood of heart disease. But AlcoholDrinking suggests a decreased 
# likelihood of heart disease, which is surprising and may be due to other reasons like the way 
# data has been gathered in the first place. The difficulty in walking (DiffWalking) is highly 
# significantly associated with increasing risk of heart disease. Likewise, poor physical health 
# also increases the risk of heart disease. The SleepTime shows that 'Optimal' and 'short' sleep 
# hours are associated with a lower likelihood of heart disease compared to reference category. 
# The BMI is borderline significant at p < 0.1, suggesting that a higher BMI score slightly 
# increases the risk of heart disease. Interestingly, older age cohorts/categories show a higher 
# likelihood of heart disease, which is also expected. Having good general health (GenHealth) 
# significantly reduces the risk of heart disease. The Asthma, kidney disease, skin cancer, and 
# history of stroke are all highly significant risk factors for heart disease. The DiabeticNo,
# borderline diabetes is also significant at p < 0.05, except the insignificant DiabeticYes 
# (during pregnancy). Race variable has varying impacts: Asian ***, Black***, RaceHispanic ** 
# and RaceWhite * are significant at p < 0.5 while RaceOther is insignificant.

vif(blr_model) # There is no multicollinearity in any of the variables. So all will be retained.

# calculate the odd ratios with the model's coefficient output for useful interpretation
# Extract coefficients and standard errors
library(epitools)
library(logistf)

# Calculating the odds ratios
odds_ratios <- exp(coef(blr_model))
# Print odds ratios
print(odds_ratios)

# Extract the coefficients from the model
coefficients <- coef(blr_model)

# Exponentiate the coefficients to get the odds ratios
odds_ratios <- exp(coefficients)

# Combine coefficients and odds ratios into a data frame for easier viewing
odds_ratios_df <- data.frame(
  Coefficient = coefficients,
  OddsRatio = odds_ratios
)

# Print the odds ratios
print(odds_ratios_df)
# Interpretation:
# The 'Odds ratio' (OR) for Smoking at 1.3605946 (OR ≈ 1.36) with positive coefficient suggests 
# smokers have approximately 36% higher odds of having heart disease compared to the non-smokers. 
# AlcoholDrinking (OR ≈ 0.77) with negative coefficient shows that alcohol drinkers have about 
# 25% lower odds of having heart disease compared to non-drinkers. The DiffWalking (OR ≈ 1.37) 
# shows that those who have difficulty walking have 37% higher odds of having heart disease 
# compared to those who don't. The Sleepers in SleepTimeShort (OR ≈ 0.76), SleepTimeOptimal 
# (OR ≈ 0.73), and SleepTimeLong (OR ≈ 0.83) have lower odds of having heart disease at 24%, 27% 
# and 17% respectively compared to those who are SleepTimeVeryShort, which is the reference group. 
# As expected those in the SleepTimeOptimal have the lowest risk. The GenHealth (OR ≈ 0.59) 
# suggests that one-unit increase in the general health score is associated with a 41% reduction 
# in the odds of heart disease. For the Sex variable, males have approximately twice the odds 
# (OR: 2.02) of having heart disease compared to females. Also as age increases by one category, 
# the odds of having heart disease increase by approximately 91% (OR: 1.91). The stroke variable 
# with (OR: 3.34) implies that individuals who have previously had a stroke have about 3.3 times 
# the odds of having heart disease compared to those who have not had a stroke incidence. As also 
# expected, DiabeticYes individuals have about 69% higher odds of having heart disease compared 
# to non-diabetics; and those who had diabetes during pregnancy have 7% (OR: 1.07) higher odds 
# of having heart disease compared to non-diabetics pregnancy. The RaceWhite have about 17% 
# (OR: 0.83), RaceAsian about 44% (OR: 0.56), RaceBlack about 34% (OR: 0.66), Hispanic about 29% 
# (OR: 0.71) lower odds of having heart disease compared to the reference race group the 
# American_Indian_Alaskan_Native. Comparing all the races, the Asian race has lowest odds (44%) 
# and the Whites having the worst odds (17%) of having heart disease. The BMI shows that higher 
# body mass index slightly increases the odds of heart disease. For each unit increase in BMI, 
# the odds of having heart disease increases by approximately 3% (OR: 1.03). So, those in the
# higher BMI categories ('Overweight' and 'Obese' groups) have the highest risk while those in the 
# recommended healthy BMI category ('Normal Weight' group) have the lowest risk.

# EVALUTE PERFORMANCE OF BLR_MODEL ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE AND AUC SCORES
# Generate Predictions on Training Data named 'balanced_data'
train_predictions <- predict(blr_model, newdata = balanced_data, type = "response")
train_predictions_binary <- ifelse(train_predictions > 0.5, 1, 0)

balanced_data$predprob <- round(fitted(blr_model),2)
predtrain <- prediction(balanced_data$predprob, balanced_data$HeartDisease)
perftrain <- performance(predtrain,"tpr","fpr")
plot(perftrain, , col = "red", main = "ROC Curve for Test Data")
abline(0,1)

# AUC for traindata
auc<-performance(predtrain,"auc")
auc@y.values # The AUC for BLR is 83.86

# Generate Confusion Matrix for the Traindata
balanced_data$predY <- as.factor(ifelse(balanced_data$predprob > threshold,1,0))
confusionMatrix(balanced_data$predY, balanced_data$HeartDisease, positive = "1")
# Comment: The BLR model accuracy is 76% (7629)

# EVALUTE PERFORMANCE OF BLR_MODEL ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Generate Predictions on the Test Data
test_predictions <- predict(blr_model, newdata = testdata, type = "response")
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

# Plot the ROC Curve and Calculate the AUC Score
testdata$predprob <- predict(blr_model, testdata, type = 'response')
predtest <- prediction(testdata$predprob, testdata$HeartDisease)
perftest <- performance(predtest,"tpr","fpr")
plot(perftest, col = "blue", main = "ROC Curve for Test Data")
abline(0,1)

# AUC for Test data
auc<-performance(predtest,"auc")
auc@y.values # The AUC for Test data is 83.84

# Generate Confusion Matrix for the Test data
testdata$predY <- as.factor(ifelse(testdata$predprob > threshold,1,0))
confusionMatrix(testdata$predY, testdata$HeartDisease, positive = "1")
# Comment: The BLR overall performance accuracy is 74% (0.7439)


# 2. BALANCED_DATA2 WITH OVER-SAMPLING METHOD
library(ROSE)

# Convert traindata to data.table for efficient processing
traindata_dt2 <- as.data.table(traindata)

# Check class distribution in traindata (80% portion only) 
class_counts_train <- table(traindata_dt2$HeartDisease)
print("Class distribution in traindata before Over-sampling:")
print(class_counts_train) # A total of 255837 observations: No = 233938 and Yes = 21899

# Get the size of the minority class
minority_class_size_train <- min(class_counts_train)
print(paste("Minority class size:", minority_class_size_train)) # "Minority class size: 21899"

# Get the size of the majority class
majority_class_size_train <- max(class_counts_train)
print(paste("Majority class size:", majority_class_size_train)) # "Majority class size: 233938"

# Calculate the desired sample size (adjust N as needed)
N <- nrow(traindata_dt2) * 1.82  # Increasing sample size by 82%

# Perform Oversampling
set.seed(456)
balanced_data2 <- ovun.sample(
  HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + BMI + 
    Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
    SkinCancer + Stroke + Diabetic + Race,  
  data = traindata_dt2, 
  method = "over", 
  N = N
)$data

# Verify the result
balanced_class_counts <- table(balanced_data2$HeartDisease)
print("Class distribution after Over-Sampling:")
print(balanced_class_counts)
# Comment: Because the Target Variable is significantly imbalanced with the 'No' class of 233938
# almost 9x the 'Yes' class of 21899 in the training data, the 'Over-Sampling' method is being 
# prioritised over the 'Under-Sampling' approach. After balancing the minority class by 80% size 
# increase, the target variable has become fairly balanced with the No = 233938 and Yes = 231685, 
# which totals 465623 observations. Such increase is highly expected for both classes to balance.

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit the logistic regression model with the correct formula
set.seed(345)
blr_model2 <- glm(formula = model, family = binomial(link = "logit"), data = balanced_data2)

# Display the summary of the model
summary(blr_model2)
# Observation: Compared to under-sampled blr_model (model1) result, the over-sampled blr_model2 
# (model2) result shows PhysicalActivity which was insignificant in both model1 is insignificant
# also in model2. The Race categories (RaceOther) is insignificant both in model1 & model2. Again,
# DiabeticYes (during pregnancy) was insignificant in moidel1 but very significant** in model2. 
# MentalHealthSome* and MentalHealthMany*** are significant in model2, but were insignificant in
# model1. Given the above inferences, both under-sampling and over-sampling methods have improved
# performance outcome, but over-Sampling method out performed under-Sampling method.

vif(blr_model2) # No multicollinearity

# Calculating the odds ratios
odds_ratios <- exp(coef(blr_model2)) #No multicollinearity
# Print odds ratios
print(odds_ratios)

# Extract the coefficients from the model
coefficients <- coef(blr_model2)

# Exponentiate the coefficients to get the odds ratios
odds_ratios <- exp(coefficients)

# Combine coefficients and odds ratios into a data frame for easier viewing
odds_ratios_df2 <- data.frame(
  Coefficient = coefficients,
  OddsRatio = odds_ratios
)

# Print the odds ratios
print(odds_ratios_df2) # The output also shows better improved odds ratios

# EVALUTE PERFORMANCE OF BLR_MODEL2 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Generate Predictions on Training Data named 'balanced_data2'
train_predictions2 <- predict(blr_model2, newdata = balanced_data2, type = "response")

balanced_data2$predprob <- round(fitted(blr_model2),2)
predtrain2 <- prediction(balanced_data2$predprob, balanced_data2$HeartDisease)
perftrain2 <- performance(predtrain2,"tpr","fpr")
plot(perftrain2, , col = "red", main = "ROC Curve for Test Data2")
abline(0,1)

# AUC for the traindata
auc <- performance(predtrain2,"auc")
auc@y.values # The AUC2 for BLR is 83.72

# Generate Confusion Matrix for the Traindata
balanced_data2$predY <- as.factor(ifelse(balanced_data2$predprob > threshold,1,0))
confusionMatrix(balanced_data2$predY, balanced_data2$HeartDisease, positive = "1")
# Comment: The BLR model accuracy is 76% (7603)

# EVALUTE PERFORMANCE OF BLR_MODEL2 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Generate Predictions on the Test Data
test_predictions2 <- predict(blr_model2, newdata = testdata, type = "response")
test_predictions2_binary <- ifelse(test_predictions2 > 0.5, 1, 0)

# Plot the ROC Curve and Calculate the AUC Score
testdata$predprob2 <- predict(blr_model2, testdata, type = 'response')
predtest2 <- prediction(testdata$predprob2, testdata$HeartDisease)
perftest2 <- performance(predtest2,"tpr","fpr")
plot(perftest2, col = "blue", main = "ROC Curve for Test Data2")
abline(0,1)

# AUC for Test data
auc<-performance(predtest2,"auc")
auc@y.values # The AUC2 for Test data is 83.85

# Generate Confusion Matrix for the Test data
testdata$predY2 <- as.factor(ifelse(testdata$predprob2 > threshold,1,0))
confusionMatrix(testdata$predY2, testdata$HeartDisease, positive = "1")
# Comment: The BLR_Model2 overall performance accuracy is 75% (0.7476)


# 3. BALANCED_DATA3 WITH COMBINATION OF BOTH UNDER-SAMPLING AND OVER-SAMPLING METHODS

# Convert traindata to data.table for efficient processing
traindata_dt3 <- as.data.table(traindata)

# Check class distribution in the training data (80% portion only)
class_distribution <- table(traindata_dt3$HeartDisease)
print("Class distribution in training data before Under- and Over-Sampling:")
print(class_distribution) # No = 233938, Yes = 21899; Total = 255837

# Calculate the desired total number of samples (adjust as needed)
desired_total_samples <- sum(class_distribution)

# Perform combination of Under- and Oversampling Method
set.seed(789)
balanced_data3 <- ovun.sample(
  HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + BMI + 
    Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
    SkinCancer + Stroke + Diabetic + Race, 
  data = traindata_dt3, 
  method = "both", p = 0.5, 
  N = desired_total_samples
)$data

# Verify the result
balanced_class_counts <- table(balanced_data3$HeartDisease)
print("Class distribution after Combination of Under- and Over-Sampling:")
print(balanced_class_counts) # No = 127374, Yes = 128463; total = 255837
# Comment: Because the Target Variable is significantly imbalanced with the 'No' class of 233938
# being 9x the 'Yes' class of 21899 in the training data, the 'Under-Sampling' method has been 
# prioritised over the 'Over-Sampling' method. After balancing 'traindata' of 255837 observations,
# the data is now fairly balanced with the No = 127374 and the Yes = 128463.

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit the logistic regression model with the correct formula
set.seed(456)
blr_model3 <- glm(formula = model, family = binomial(link = "logit"), data = balanced_data3)

# Display the summary of the model
summary(blr_model3)
# Observation: Compared to model1 and model2 results, the Under- and over-sampled 
# blr_model3 (model2) result shows only slight improvement. For eg., PhysicalActivity & RaceOther 
# remain insignificant in all the models (model1, model2 and model3). By and large, model3 seems
# to have a more better performance than model1 and model2: as model1 reduced the training data 
# drastically and model2 substantially increased the training data size. However, a combination 
# of Under- and Over-Sampling method did not increase or decrease the training data, which makes 
# this approach more reliable and robust in improving model performance output.

vif(blr_model3) # No multicollinearity

# Calculating the odds ratios
odds_ratios <- exp(coef(blr_model3))
# Print odds ratios
print(odds_ratios)

# Extract the coefficients from the model
coefficients <- coef(blr_model3)

# Exponentiate the coefficients to get the odds ratios
odds_ratios <- exp(coefficients)

# Combine coefficients and odds ratios into a data frame for easier viewing
odds_ratios_df3 <- data.frame(
  Coefficient = coefficients,
  OddsRatio = odds_ratios
)

# Print the odds ratios
print(odds_ratios_df3) # The output also shows better improved odds ratio reading

# EVALUTE PERFORMANCE OF BLR_MODEL3 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Generate Predictions on Training Data named 'balanced_data3'
train_predictions3 <- predict(blr_model3, newdata = balanced_data3, type = "response")

balanced_data3$predprob <- round(fitted(blr_model3),2)
predtrain3 <- prediction(balanced_data3$predprob, balanced_data3$HeartDisease)
perftrain3 <- performance(predtrain3,"tpr","fpr")
plot(perftrain3, , col = "red", main = "ROC Curve for Test Data3")
abline(0,1)

# AUC for taindata
auc <- performance(predtrain3,"auc")
auc@y.values # The AUC3 for the Training data is 83.78

# Generate Confusion Matrix for the Traindata
balanced_data3$predY <- as.factor(ifelse(balanced_data3$predprob > threshold,1,0))
confusionMatrix(balanced_data3$predY, balanced_data3$HeartDisease, positive = "1")
# Comment: The BLR performance accuracy is 76% (7610)

# EVALUTE PERFORMANCE OF BLR_MODEL3 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Generate Predictions on the Test Data
test_predictions3 <- predict(blr_model3, newdata = testdata, type = "response")
test_predictions3_binary <- ifelse(test_predictions3 > 0.5, 1, 0)

# Plot the ROC Curve and Calculate the AUC Score
testdata$predprob3 <- predict(blr_model3, testdata, type = 'response')
predtest3 <- prediction(testdata$predprob3, testdata$HeartDisease)
perftest3 <- performance(predtest3,"tpr","fpr")
plot(perftest3, col = "blue", main = "ROC Curve for Test Data3")
abline(0,1)

# AUC for Test data
auc<-performance(predtest3,"auc")
auc@y.values # The AUC3 for Test data is 83.85

# Generate Confusion Matrix for the Test data
testdata$predY3 <- as.factor(ifelse(testdata$predprob3 > threshold,1,0))
confusionMatrix(testdata$predY3, testdata$HeartDisease, positive = "1")
# Comment: The BLR Model has an overall performance accuracy of 74% (0.7425)

# Conclusion:
# With the under-sampling method, the BLR Model1 has an overall accuracy of 74% (0.7439) and AUC 
# of 83.84. The BLR Model2 with over-sampling method has an overall performance accuracy of 75% 
# (0.7476) and AUC2 of 83.85. The BLR Model3 with the combination of under- and over-sampling 
# method has an overall accuracy of 74% (0.7425) and AUC3 of 83.85.


# MODEL TWO: NAIVE BAYES (NB)
library(naivebayes)
library(e1071)

# 1. BALANCED_DATA WITH UNDER-SAMPLING METHOD

# Using the Training Balanced_Data 
table(balanced_data$HeartDisease) # Data already balanced with Under-Sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit Naive Bayes model
set.seed(34567)
nb_model <- naiveBayes(formula = model, data = balanced_data)
nb_model

# Generate Predictions on Training Data
train_predictions <- predict(nb_model, newdata = balanced_data, type = "raw")[,2]
balanced_data$predprob <- train_predictions

# Plot ROC Curve for training data
predtrain <- prediction(balanced_data$predprob, balanced_data$HeartDisease)
perftrain <- performance(predtrain, "tpr", "fpr")
plot(perftrain, col = "red", main = "ROC Curve for Train Data (Naive Bayes)")
abline(0, 1)

# AUC for traindata
auc_train <- performance(predtrain, "auc")
auc_train@y.values
# Comment: The AUC for NB is 81.17

# Confusion Matrix for Training Data
threshold <- 0.5
balanced_data$predY <- as.factor(ifelse(balanced_data$predprob > threshold, 1, 0))
confusion_matrix_train <- confusionMatrix(balanced_data$predY, balanced_data$HeartDisease, positive = "1")
confusion_matrix_train
# Comment: The NB performance accuracy is 72% (0.7152)

# Generate Predictions on Test Data
test_predictions <- predict(nb_model, newdata = testdata, type = "raw")[,2]
testdata$predprob <- test_predictions

# Plot ROC Curve
predtest <- prediction(testdata$predprob, testdata$HeartDisease)
perftest <- performance(predtest, "tpr", "fpr")
plot(perftest, col = "blue", main = "ROC Curve for Test Data (Naive Bayes)")
abline(0, 1)

# Calculate AUC for Test Data
auc_test <- performance(predtest, "auc")
auc_test@y.values
# Comment: The AUC for NB is 81.13

# Confusion Matrix for Test Data
testdata$predY <- as.factor(ifelse(testdata$predprob > threshold, 1, 0))
confusion_matrix_test <- confusionMatrix(testdata$predY, testdata$HeartDisease, positive = "1")
confusion_matrix_test
# Comment: The NB performance accuracy is 80% (0.7963)


# 2. BALANCED_DATA2 WITH OVER-SAMPLING METHOD

# Using the Training Balanced_Data2 
table(balanced_data2$HeartDisease) # Data already balanced with Under-Sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit Naive Bayes model
set.seed(34567)
nb_model2 <- naiveBayes(formula = model, data = balanced_data2)
nb_model2

# Generate Predictions on Training Data
train_predictions2 <- predict(nb_model2, newdata = balanced_data2, type = "raw")[,2]
balanced_data2$predprob <- train_predictions2

# Plot ROC Curve for training data
predtrain2 <- prediction(balanced_data2$predprob, balanced_data2$HeartDisease)
perftrain2 <- performance(predtrain2, "tpr", "fpr")
plot(perftrain2, col = "red", main = "ROC Curve for Train Data (Naive Bayes)")
abline(0, 1)

# AUC for traindata
auc_train2 <- performance(predtrain2, "auc")
auc_train2@y.values
# Comment: The AUC for NB is 81.01

# Confusion Matrix for Training Data
threshold <- 0.5
balanced_data2$predY <- as.factor(ifelse(balanced_data2$predprob > threshold, 1, 0))
confusion_matrix_train <- confusionMatrix(balanced_data2$predY, balanced_data2$HeartDisease, positive = "1")
confusion_matrix_train
# Comment: The NB performance accuracy is 71% (0.7133)

# Generate Predictions on Test Data
test_predictions2 <- predict(nb_model2, newdata = testdata, type = "raw")[,2]
testdata$predprob <- test_predictions2

# Plot ROC Curve
predtest2 <- prediction(testdata$predprob, testdata$HeartDisease)
perftest2 <- performance(predtest2, "tpr", "fpr")
plot(perftest2, col = "blue", main = "ROC Curve for Test Data (Naive Bayes)")
abline(0, 1)

# Calculate AUC for Test Data
auc_test2 <- performance(predtest2, "auc")
auc_test2@y.values
# Comment: The AUC for NB is 81.14

# Confusion Matrix for Test Data
testdata$predY <- as.factor(ifelse(testdata$predprob > threshold, 1, 0))
confusion_matrix_test <- confusionMatrix(testdata$predY, testdata$HeartDisease, positive = "1")
confusion_matrix_test
# Comment: The NB performance accuracy is 80% (0.7973)


# 3. BALANCED_DATA3 WITH THE COMBINATION OF UNDER- AND OVER-SAMPLING METHOD

# Using the Training Balanced_Data3 
table(balanced_data3$HeartDisease) # Data balanced with Combined Under- and Over-sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit Naive Bayes model
set.seed(34567)
nb_model3 <- naiveBayes(formula = model, data = balanced_data3)
nb_model3

# Generate Predictions on Training Data
train_predictions3 <- predict(nb_model3, newdata = balanced_data3, type = "raw")[,2]
balanced_data3$predprob <- train_predictions3

# Plot ROC Curve for training data
predtrain3 <- prediction(balanced_data3$predprob, balanced_data3$HeartDisease)
perftrain3 <- performance(predtrain3, "tpr", "fpr")
plot(perftrain3, col = "red", main = "ROC Curve for Train Data (Naive Bayes)")
abline(0, 1)

# AUC for traindata
auc_train3 <- performance(predtrain3, "auc")
auc_train3@y.values
# Comment: The AUC for NB is 81.05

# Confusion Matrix for Training Data
threshold <- 0.5
balanced_data3$predY <- as.factor(ifelse(balanced_data3$predprob > threshold, 1, 0))
confusion_matrix_train <- confusionMatrix(balanced_data3$predY, balanced_data3$HeartDisease, positive = "1")
confusion_matrix_train
# Comment: The NB performance accuracy is 71% (0.7127)

# Generate Predictions on Test Data
test_predictions3 <- predict(nb_model3, newdata = testdata, type = "raw")[,2]
testdata$predprob <- test_predictions3

# Plot ROC Curve
predtest3 <- prediction(testdata$predprob, testdata$HeartDisease)
perftest3 <- performance(predtest3, "tpr", "fpr")
plot(perftest3, col = "blue", main = "ROC Curve for Test Data (Naive Bayes)")
abline(0, 1)

# Calculate AUC for Test Data
auc_test3 <- performance(predtest3, "auc")
auc_test3@y.values
# Comment: The AUC for NB is 81.17

# Confusion Matrix for Test Data
testdata$predY <- as.factor(ifelse(testdata$predprob > threshold, 1, 0))
confusion_matrix_test <- confusionMatrix(testdata$predY, testdata$HeartDisease, positive = "1")
confusion_matrix_test
# Comment: The NB performance accuracy is 80% (0.7975)


# MODEL THREE: DECISION TREE (DT)
library(partykit) # ctree function

# 1. BALANCED_DATA WITH UNDER-SAMPLING METHOD
table(balanced_data$HeartDisease) # Traindata already balanced with Under-Sampling method

# Train Decision Tree model
set.seed (123456) # For reproducibility

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit Decision Tree model on traindata (balanced_data) with ctree function from Partykit library
set.seed(23456)
dt_model <- partykit::ctree(formula = model, data = balanced_data)

# Plot the tree
plot(dt_model, type="simple", gp=gpar(cex=0.4)) # The 'gp' setting helps readability
# Comment: The tree plot was too dense due to large number of predictors to train 

# EVALUTE PERFORMANCE OF DT_MODEL ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on training data named 'balanced_data' and plot ROC Curve
predtree_train <- predict(dt_model, balanced_data, type = "prob")
# Plot ROC curve for training data
pred_dt_train <- prediction(predtree_train[,2], balanced_data$HeartDisease)
perf_dt_train <- performance(pred_dt_train, "tpr", "fpr")
plot(perf_dt_train, main = "ROC Curve (Training Data)")
abline(0, 1, col = "red")

# Calculate AUC for training data
auc_train <- performance(pred_dt_train, "auc")
auc_train@y.values # The AUC for DT is 83.94

# Confusion Matrix for traindata
balanced_data$predY<- predict(dt_model, balanced_data, type="response")
confusionMatrix(balanced_data$predY, balanced_data$HeartDisease, positive="1") 
# Comment: The performance accuracy for DT is 77% (0.7656)

# EVALUTE PERFORMANCE OF DT_MODEL ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on testing data
predtree_test <- predict(dt_model, testdata, type = "prob")
# Plot ROC Curve for testdata
pred_dt_test <- prediction(predtree_test[,2], testdata$HeartDisease)
perf_dt_test <- performance(pred_dt_test, "tpr", "fpr")
plot(perf_dt_test, main = "ROC Curve (Testing Data)")
abline(0, 1, col = "red")

# Calculate AUC for testing data
auc_test <- performance(pred_dt_test, "auc")
auc_test@y.values # The AUC for the DT testdata is 83.01

# Confusion Matrix for testdata
testdata$predY<- predict(dt_model, testdata, type="response")
confusionMatrix(testdata$predY, testdata$HeartDisease, positive="1") 
# Comment: The DT testdata overall accuracy is 72% (0.7209)


# 2. BALANCED_DATA2 WITH OVER-SAMPLING METHOD
table(balanced_data2$HeartDisease) # Traindata already balanced with Over-Sampling method

# Train Decision Tree model
set.seed (123456) #or reproducibility

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit the Decision Tree model using ctree function from the Partykit library
set.seed(23456)
dt_model2 <- partykit::ctree(formula = model, data = balanced_data2)

# Plot the tree
plot(dt_model2, type="simple", gp=gpar(cex=0.4))
# Comment: Plotting of the tree was very slowed due to large number of predictors and doubling
# of traindata size as the minority class size got increased by 83% to balance with majority.

# EVALUTE PERFORMANCE OF DT_MODEL2 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on training data named 'balanced_data' and plot ROC Curve
predtree_train2 <- predict(dt_model2, balanced_data2, type = "prob")
# Plot ROC curve for training data
pred_dt_train2 <- prediction(predtree_train2[,2], balanced_data2$HeartDisease)
perf_dt_train2 <- performance(pred_dt_train2, "tpr", "fpr")
plot(perf_dt_train2, main = "ROC Curve (Training Data)")
abline(0, 1, col = "red")

# Calculate AUC for training data
auc_train2 <- performance(pred_dt_train2, "auc")
auc_train2@y.values # The AUC for DT is 88.87

# Confusion Matrix for traindata2
balanced_data2$predY<- predict(dt_model2, balanced_data2, type="response")
confusionMatrix(balanced_data2$predY, balanced_data2$HeartDisease, positive="1") 
# Comment: The accuracy for DT is 81% (0.8097)

# EVALUTE PERFORMANCE OF DT_MODEL2 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on testing data
predtree_test2 <- predict(dt_model2, testdata, type = "prob")
# Plot ROC Curve for testdata
pred_dt_test2 <- prediction(predtree_test2[,2], testdata$HeartDisease)
perf_dt_test2 <- performance(pred_dt_test2, "tpr", "fpr")
plot(perf_dt_test2, main = "ROC Curve (Testing Data)")
abline(0, 1, col = "red")

# Calculate AUC for testing data
auc_test2 <- performance(pred_dt_test2, "auc")
auc_test2@y.values # The AUC for the testdata is 77.52

# Confusion Matrix for testdata
testdata$predY2 <- predict(dt_model2, testdata, type="response")
confusionMatrix(testdata$predY2, testdata$HeartDisease, positive="1") 
# Comment: The overall DT performance accuracy is 73% (0.7290)


# 3. BALANCED_DATA3 WITH COMBINATION OF UNDER- AND OVER-SAMPLING METHOD
table(balanced_data3$HeartDisease) # Traindata already balanced with Over-Sampling method

# Train Decision Tree model
set.seed (123456) # For reproducibility

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Fit the Decision Tree model using ctree function from the Partykit library
set.seed(23456)
dt_model3 <- partykit::ctree(formula = model, data = balanced_data3)

# Plot the tree
plot(dt_model3, type="simple", gp=gpar(cex=0.4))
# Comment: Plotting of the tree was very slowed due to large number of predictors and despite no
# maintaining the traindata sample size number without being reduced or increased.

# EVALUTE PERFORMANCE OF DT_MODEL3 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on training data named 'balanced_data' and plot ROC Curve
predtree_train3 <- predict(dt_model3, balanced_data3, type = "prob")
# Plot ROC curve for training data
pred_dt_train3 <- prediction(predtree_train3[,2], balanced_data3$HeartDisease)
perf_dt_train3 <- performance(pred_dt_train3, "tpr", "fpr")
plot(perf_dt_train3, main = "ROC Curve (Training Data)")
abline(0, 1, col = "red")

# Calculate AUC for training data
auc_train3 <- performance(pred_dt_train3, "auc")
auc_train3@y.values # The AUC for DT is 87.35

# Confusion Matrix for traindata
balanced_data3$predY<- predict(dt_model3, balanced_data3, type="response")
confusionMatrix(balanced_data3$predY, balanced_data3$HeartDisease, positive="1") 
# Comment: The performnce accuracy for DT is 79% (0.7920)

# EVALUTE PERFORMANCE OF DT_MODEL3 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predict probabilities on testing data
predtree_test3 <- predict(dt_model3, testdata, type = "prob")
# Plot ROC Curve for testdata
pred_dt_test3 <- prediction(predtree_test3[,2], testdata$HeartDisease)
perf_dt_test3 <- performance(pred_dt_test3, "tpr", "fpr")
plot(perf_dt_test3, main = "ROC Curve (Testing Data)")
abline(0, 1, col = "red")

# Calculate AUC for testing data
auc_test3 <- performance(pred_dt_test3, "auc")
auc_test3@y.values # The AUC for the testdata is 80.29

# Confusion Matrix for testdata
testdata$predY3 <- predict(dt_model3, testdata, type="response")
confusionMatrix(testdata$predY3, testdata$HeartDisease, positive="1")
# Comment: The overall DT accuracy is 74% (0.7400)


# MODEL THREE: RANDOM FOREST (RF)
library(randomForest)

# 1. BALANCED_DATA WITH UNDER-SAMPLING METHOD

# Use the Balanced Trained Data 
table(balanced_data$HeartDisease) # Data already balanced with Under-Sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Train Random Forest model
set.seed (123456) # For reproducibility

rf_model <- randomForest(formula = model, data = balanced_data, importance = TRUE, ntree = 100)
rf_model
# Comment: The model has an estimated error rate of 24.34% based on out-of-bag data. The confusion 
# matrix shows that approximately 27.72% of instances labeled as class 0 were actually class 1; 
# and approximately 20.96% of instances labeled as class 1 were actually class 0. This suggests 
# that the model has a higher error rate for class 0 compared to class 1.

# Variable Importance Plot
rf_model$importance
# Comment: As expected AgeCategory has the highest 'MeanDecreaseAccuracy' and 'MeanDecreaseGini' 
# values among all the features, which indicates that it strongly influences the model's ability 
# to correctly classify instances and reduce impurity in decision trees. Generally, different age 
# groups have distinct risk profiles for heart disease, and the model has learned to effectively 
# capture these patterns. Also among the most important predictors based on the MeanDecreaseAccuray 
# are GenHealth, Diabetic, Stroke, DiffWalking, PhysicalHealth, etc; while some variables like 
# AlcoholDrinking and smoking are among features that seem to have relatively lower important.

varImpPlot(rf_model,col="blue") # Plots both the MeanDecreaseAccuracy and MeanDecreaseGini

# Visualising the Features Importance using ggplot2 library
importance <- data.frame(rf_model$importance)
importance$Feature <- rownames(importance)
library(ggplot2)
ggplot(importance, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Feature", y = "Mean Decrease Accuracy") +
  ggtitle("Feature Importance")
# Comment: The plot displays hierachically the features of the highest importance on top and the
# lowest importance at the bottom.

# EVALUTE PERFORMANCE OF RF_MODEL ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Training Data (balanced_data)
rf_predictions <- predict(rf_model, balanced_data, type = "prob")[,2]
rf_predictions_binary <- ifelse(rf_predictions > 0.5, 1, 0)

# Compute ROC curve for training data
rf_roc <- roc(balanced_data$HeartDisease, rf_predictions)
plot(rf_roc, col = "red", main = "ROC Curves for Models")

# Get AUC for traindata
rf_auc_value <- auc(rf_roc)
print(paste("Random Forest AUC value:", rf_auc_value))
# Comment: The AUC for Random Forest is 90.42

# Get Confusion Matrix for training data
rf_confusion_matrix <- confusionMatrix(as.factor(rf_predictions_binary), as.factor(balanced_data$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix)
# Comment: The Random Forest accuracy is 85% (0.8523)

# EVALUTE PERFORMANCE OF RF_MODEL ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Test Data
rf_predictions <- predict(rf_model, testdata, type = "prob")[,2]
rf_predictions_binary <- ifelse(rf_predictions > 0.5, 1, 0)

# Compute ROC Curve for testdata
rf_roc <- roc(testdata$HeartDisease, rf_predictions)
plot(rf_roc, col = "red", main = "ROC Curves for Model")

# Get AUC for testdata
rf_auc_value <- auc(rf_roc)
print(paste("Random Forest AUC value:", rf_auc_value))
# Comment: The AUC for Random Forest is 81.13

# Get Confusion Matrix for testing data
rf_confusion_matrix <- confusionMatrix(as.factor(rf_predictions_binary), as.factor(testdata$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix)
# Comment: The Random Forest accuracy is 73% (0.7273)


# 2. BALANCED_DATA2 WITH OVER-SAMPLING METHOD

# Use the Balanced Trained Data 
table(balanced_data2$HeartDisease) # Data already balanced with Over-Sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Train Random Forest model
set.seed (123456) # For reproducibility

rf_model2 <- randomForest(formula = model, data = balanced_data2, importance = TRUE, ntree = 100)
rf_model2
# Comment: The model has an estimated error rate of 16.50% based on out-of-bag data. The confusion 
# matrix shows that approximately 21.90% of instances labeled as class 0 were actually class 1; 
# and approximately 11.06% of instances labeled as class 1 were actually class 0. Compared to the 
# previous rf_model1, the rf_model2 shows significant improvements with a lower OOB error rate:
# The out-of-bag error rate dropped from 24.34% to 16.5%, indicating better overall performance.
# The confusion matrix also improved as both error rates for classes 0 and 1 have decreased, 
# suggesting better classification accuracy for both classes.

# Variable Importance Plot
rf_model2$importance
# Comment: As expected AgeCategory has the highest 'MeanDecreaseAccuracy' and 'MeanDecreaseGini' 
# values among all the features, which indicates that it strongly influences the model's ability 
# to correctly classify instances and reduce impurity in decision trees. Generally, different age 
# groups have distinct risk profiles for heart disease, and the model has learned to effectively 
# capture these patterns. Also among the most important predictors are GenHealth, DiffWalking, 
# Diabetic, PhysicalHealth as well as Stroke, etc.

varImpPlot(rf_model2,col="blue")

# Visualising the Features Importance using ggplot2 library
importance <- data.frame(rf_model2$importance)
importance$Feature <- rownames(importance)
library(ggplot2)
ggplot(importance, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Feature", y = "Mean Decrease Accuracy") +
  ggtitle("Feature Importance")
# Comment: While AgeCategory remains the most important predictor, there is improved performance 
# in model2 with some variables that were previously of lower importance proving to be among the 
# strong predictors for heart disease. 

# EVALUTE PERFORMANCE OF RF_MODEL2 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Training Data (balanced_data2)
rf_predictions2 <- predict(rf_model2, balanced_data2, type = "prob")[,2]
rf_predictions_binary2 <- ifelse(rf_predictions2 > 0.5, 1, 0)

# Compute ROC curve for training data
rf_roc2 <- roc(balanced_data2$HeartDisease, rf_predictions2)
plot(rf_roc2, col = "red", main = "ROC Curves for Models")

# Get AUC for traindata
rf_auc_value2 <- auc(rf_roc2)
print(paste("Random Forest AUC value:", rf_auc_value2))
# Comment: The AUC for Random Forest (model2) is 90.84

# Get Confusion Matrix for training data
rf_confusion_matrix2 <- confusionMatrix(as.factor(rf_predictions_binary2), as.factor(balanced_data2$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix2)
# Comment: The Random Forest accuracy is 85% (0.8514)

# EVALUTE PERFORMANCE OF RF_MODEL2 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Test Data
rf_predictions2 <- predict(rf_model2, testdata, type = "prob")[,2]
rf_predictions_binary2 <- ifelse(rf_predictions2 > 0.5, 1, 0)

# Compute ROC Curve for testdata
rf_roc2 <- roc(testdata$HeartDisease, rf_predictions2)
plot(rf_roc2, col = "red", main = "ROC Curves for Models")

# Get AUC for testdata
rf_auc_value2 <- auc(rf_roc2)
print(paste("Random Forest AUC value:", rf_auc_value2))
# Comment: The AUC for Random Forest is 79.83

# Get Confusion Matrix for testing data
rf_confusion_matrix2 <- confusionMatrix(as.factor(rf_predictions_binary2), as.factor(testdata$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix2)
# Comment: The Random Forest overall accuracy is 77% (0.7743)


# 3. BALANCED_DATA3 WITH THE COMBINATION OF UNDER- AND OVER-SAMPLING METHOD

# Use the Balanced Trained Data 
table(balanced_data3$HeartDisease) # Data already balanced with Under-Sampling method

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + 
  BMI + Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Train Random Forest model
set.seed (123456) # For reproducibility

rf_model3 <- randomForest(formula = model, data = balanced_data3, importance = TRUE, ntree = 100)
rf_model3
# Comment: The model has an estimated error rate of 16.63% based on out-of-bag data. The confusion 
# matrix shows that approximately 22.33% of instances labeled as class 0 were actually class 1; 
# and approximately 10.97% of instances labeled as class 1 were actually class 0.

# Variable Importance Plot
rf_model3$importance
# Comment: As expected AgeCategory has highest 'MeanDecreaseAccuracy' and 'MeanDecreaseGini' 
# values among all the features, which indicates that it strongly influences the model's ability 
# to correctly classify instances and reduce impurity in decision trees. Other features that were
# previously of lower importance in model1, for example, 'Smoking' and 'MentalHealth' are now 
# high in importance in model3, which is not surprising that these variables are among the strong 
# prectors for heart disease given domain knowledge.

varImpPlot(rf_model3,col="blue")

# Visualising the Features Importance using ggplot2 library
importance <- data.frame(rf_model3$importance)
importance$Feature <- rownames(importance)
library(ggplot2)
ggplot(importance, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Feature", y = "Mean Decrease Accuracy") +
  ggtitle("Feature Importance")
# Comment: As expected AgeCategory is the most important predictor among all the features, which 
# indicates that it strongly influences the model's ability to correctly classify instances and 
# reduce impurity in decision trees. Interestingly, in the rf_model3 other features that were 
# previously of lower importance in model1, for example, 'Smoking' and 'MentalHealth' are now 
# high in importance in model3, which is not surprising.

# EVALUTE PERFORMANCE OF RF_MODEL3 ON TRAINDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Training Data (balanced_data)
rf_predictions3 <- predict(rf_model3, balanced_data3, type = "prob")[,2]
rf_predictions_binary3 <- ifelse(rf_predictions3 > 0.5, 1, 0)

# Compute ROC curve for training data
rf_roc3 <- roc(balanced_data3$HeartDisease, rf_predictions3)
plot(rf_roc3, col = "red", main = "ROC Curves for Models")

# Get AUC for traindata
rf_auc_value3 <- auc(rf_roc3)
print(paste("Random Forest AUC value:", rf_auc_value3))
# Comment: The AUC for Random Forest is 90.94

# Get Confusion Matrix for training data
rf_confusion_matrix3 <- confusionMatrix(as.factor(rf_predictions_binary3), as.factor(balanced_data3$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix3)
# Comment: The Random Forest accuracy is 85% (0.8549)

# EVALUTE PERFORMANCE OF RF_MODEL3 ON TESTDATA USING CONFUSION MATRIX, ROC CURVE & AUC SCORES
# Predicting on the Test Data
rf_predictions3 <- predict(rf_model3, testdata, type = "prob")[,2]
rf_predictions_binary3 <- ifelse(rf_predictions3 > 0.5, 1, 0)

# Compute ROC Curve for testdata
rf_roc3 <- roc(testdata$HeartDisease, rf_predictions3)
plot(rf_roc3, col = "red", main = "ROC Curves for Models")

# Get AUC for testdata
rf_auc_value3 <- auc(rf_roc3)
print(paste("Random Forest AUC value:", rf_auc_value3)) # The overall AUC score for RF is 80.29

# Get Confusion Matrix for testing data
rf_confusion_matrix3 <- confusionMatrix(as.factor(rf_predictions_binary3), as.factor(testdata$HeartDisease))
print("Random Forest Confusion Matrix")
print(rf_confusion_matrix3) # The overall performance accuracy for the RF is 75% (0.7550)

# General Observations / Overall Conclusion: 
# Comparing the performance of the four models: Binary Logistic Regression (BLR) with Naive Bayes 
# (NB), Decision Tree(DT), and Random Forests (RF) based on the three balancing methods (Under-Sampling, 
# Over-Sampling, and Combination of Under- and Over-Sampling) to identify 
# the best performing model and approach. 
# Based the accuracy and AUC output results, Random Forest (RF) consistently outperforms all the other 
# models regardless of the balancing method. 
# Among the balancing methods, Over-sampling generally improves performance for all the models, 
# especially for Decision Trees (DT). Under-sampling and Combination methods showed mixed results, 
# with some models benefiting and others deteriorating.
# BLR: While over-sampling and combination methods slightly improved the model's AUC scores, 
# however, there is no significant difference in the accuracy results.
# NB: Over-sampling and combination methods led to a slight increase in accuracy but tended to 
# have minimal impact on the AUC scores.
# DT: Over-sampling significantly improves both accuracy and AUC results of the model.
# RF: All the three balancing methods in the RF model yielded high accuracy and AUC scores, with 
# the over-sampling method and the combination method showing a slight edge. 
# Given the output results, Random Forest performs the best overall due to its high performance 
# across all metrics, while Decision Tree performs the least well. Naive Bayes and Binary Logistic 
# Regression fall in between, with Naive Bayes slightly outperforming Logistic Regression in this 
# context.


# Visual Representation of the Comparative Model Performance Across the Three Balancing Methods  
# Plotting overall model performance on the testing data
performance_testing <- data.frame(
  Model = rep(c("BLR", "NB", "DT", "RF"), each = 3),
  BalancingMethod = c("Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination"),
  Accuracy = c(0.7439, 0.7476, 0.7425, 0.7963, 0.7973, 0.7975, 0.7209, 0.7290, 0.7400, 0.7273, 0.7743, 0.7550),
  Sensitivity = c(0.78389, 0.77859, 0.78809, 0.62276, 0.62094, 0.62039, 0.79905, 0.72963, 0.74881, 0.7214, 0.7837, 0.7567),
  Specificity = c(0.74015, 0.74467, 0.73829, 0.81256, 0.81378, 0.81410, 0.71361, 0.72892, 0.73914, 0.7901, 0.6741, 0.7375),
  AUC = c(83.84, 83.85, 83.85, 81.13, 81.14, 81.17, 83.01, 77.52, 80.29, 81.13, 79.83, 80.29)
)

# Convert data to long format for ggplot
performance_long <- performance_testing %>%
  pivot_longer(cols = Accuracy:AUC, names_to = "Metric", values_to = "Value")

# Plotting the Bar Chart
ggplot(performance_long, aes(x = Model, y = Value, fill = BalancingMethod)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  facet_wrap(~ Metric, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Model Performance on the Testdata",
       x = "Model",
       y = "Performance Metric",
       fill = "Balancing Method") +
  scale_fill_brewer(palette = "Set1")

# Listing Model Accuracy, Sensitivity, & Specificity scores for Bar Chart
performance_data <- data.frame(
  Model = rep(c("BLR", "NB", "DT", "RF"), each = 3),
  BalancingMethod = rep(c("Under-Sampling", "Over-Sampling", "Combination"), times = 4),
  Accuracy = c(0.7629, 0.7603, 0.7610, 0.7152, 0.7133, 0.7127, 0.7656, 0.8097, 0.7920, 0.8523, 0.8514, 0.8549),
  Sensitivity = c(0.7789, 0.7715, 0.7771, 0.6110, 0.6104, 0.6107, 0.8076, 0.8757, 0.8278, 0.8362, 0.8094, 0.8118),
  Specificity = c(0.7468, 0.7492, 0.7447, 0.8194, 0.8152, 0.8155, 0.7236, 0.7443, 0.7559, 0.8684, 0.8938, 0.8977)
)

# Listing Model AUC scores for line chart plot
auc_data <- data.frame(
  Model = rep(c("BLR", "NB", "DT", "RF"), each = 3),
  BalancingMethod = rep(c("Under-Sampling", "Over-Sampling", "Combination"), times = 4),
  AUC = c(83.86, 83.72, 83.78, 81.17, 81.01, 81.05, 83.94, 88.87, 87.35, 90.42, 90.84, 90.94)
)

# Reshaping data for plotting
performance_data_long <- reshape2::melt(performance_data, id.vars = c("Model", "BalancingMethod"))

# Create bar chart
ggplot(performance_data_long, aes(x = interaction(Model, BalancingMethod), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Model & Balancing Method", y = "Value", fill = "Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Model Performance Across Different Balancing Methods")

# Create Line Chart for the Models' AUC Scores
ggplot(auc_data, aes(x = BalancingMethod, y = AUC, group = Model, color = Model)) +
  geom_line() +
  geom_point() +
  labs(x = "Balancing Method", y = "AUC", color = "Model") +
  ggtitle("Model AUC Across Balancing Methods")

# Load necessary libraries
library(knitr)
library(kableExtra)

# Define the data
performance_table <- data.frame(
  Model = c("BLR", "BLR", "BLR", "NB", "NB", "NB", "DT", "DT", "DT", "RF", "RF", "RF"),
  BalancingMethod = c("Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination",
                      "Under-Sampling", "Over-Sampling", "Combination"),
  Accuracy = c(0.7629, 0.7603, 0.7610, 0.7152, 0.7133, 0.7127, 0.7656, 0.8097, 0.7920, 0.8523, 0.8514, 0.8549),
  Sensitivity = c(0.7789, 0.7715, 0.7771, 0.6110, 0.6104, 0.6107, 0.8076, 0.8757, 0.8278, 0.8362, 0.8094, 0.8118),
  Specificity = c(0.7468, 0.7492, 0.7447, 0.8194, 0.8152, 0.8155, 0.7236, 0.7443, 0.7559, 0.8684, 0.8938, 0.8977),
  AUC = c(83.86, 83.72, 83.78, 81.17, 81.01, 81.05, 83.94, 88.87, 87.35, 90.42, 90.84, 90.94)
)

# Customize and print the table
kable(performance_table, format = "html", caption = "Model Performance Across Different Balancing Methods", 
      col.names = c("Model", "Balancing Method", "Accuracy", "Sensitivity", "Specificity", "AUC")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, italic = TRUE) %>%
  column_spec(3:6, background = "lightblue", color = "black", width = "8em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") %>%
  add_header_above(c(" " = 2, "Performance Metrics" = 4))

# Declare preference for dplyr::filter to avoid conflict
conflicts_prefer(dplyr::filter)

# Read the data from the CSV file
data <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Split the single column into multiple columns
data <- data %>%
  separate(Model.BalancingMethod.Metric.Value, into = c("Model", "BalancingMethod", "Metric", "Value"), sep = ",")

# Convert Value column to numeric
data$Value <- as.numeric(data$Value)

# Print the first few rows and column names to verify
print(head(data))
print(colnames(data))

# Filter data for Accuracy and AUC using dplyr::filter
data_filtered <- data %>%
  dplyr::filter(Metric %in% c("Accuracy", "AUC"))

# Rank the models based on Accuracy and AUC
ranked_data <- data_filtered %>%
  group_by(Metric) %>%
  mutate(Rank = rank(-Value, ties.method = "min")) %>%  # Rank in descending order
  ungroup()

# Calculate the average rank for each Model-BalancingMethod combination
average_rank <- ranked_data %>%
  group_by(Model, BalancingMethod) %>%
  summarise(AverageRank = mean(Rank, na.rm = TRUE), .groups = 'drop') %>%
  arrange(AverageRank)

# Create the final table
final_table <- average_rank %>%
  select(Model, BalancingMethod, AverageRank)

# Print the final table
print(final_table)
# Conclusion: 
# The best performed model is the NB (Average Rank: 3) with Combination Method while the Worst 
# performed model is the DT (Average Rank: 11) with Over-Sampling. These ranks suggest that the 
# NB model performs best when using the Combination balancing method, while the DT model performs 
# worst with the Over-Sampling balancing method.



# PHASE TWO:
# ALTERNATIVELY ENCODE ORIGINAL MASTER_DATA DIRECTLY WITHOUT TRANSFORMING/RECODING THE VARIABLES

# Combined four (4) datasets
heartdata <- full_join(behaviour_data, demographics_data, by = "pid") %>%
  full_join(., medical_history_data, by = "pid") %>%
  full_join(., heart_disease_data, by = "pid")

# Save a copy of 'heartdata' as masterdata 
#write.csv(heartdata, "masterdata.csv", row.names = FALSE) # Merged masterdata saved as csv file

# Load the master_data dataset
master_data <- read.csv(file.choose(), stringsAsFactors = FALSE)
head(master_data)
str(master_data) # Data Types: 15 variables = Charactrer; 3 variables = Integer; BMI = numeric 
dim(master_data)# There are 319795 observations and 19 variables including the 'pid' (patient ID)

# Convert the relevant columns to factors
binary_vars <- c("Smoking", "AlcoholDrinking", "Stroke", "DiffWalking", 
                 "PhysicalActivity", "Asthma", "KidneyDisease", "SkinCancer")
master_data[binary_vars] <- lapply(master_data[binary_vars], function(x) as.numeric(factor(x, levels = c("No", "Yes"))) - 1)

# Convert Sex to numeric
master_data$Sex <- as.numeric(factor(master_data$Sex, levels = c("Male", "Female"))) - 1

# Convert Race to numeric
race_levels <- c("White", "Black", "Asian", "American Indian/Alaskan Native", "Hispanic", "Other")
master_data$Race <- as.numeric(factor(master_data$Race, levels = race_levels)) - 1

# Convert Diabetic to numeric
diabetic_levels <- c("No", "No, borderline diabetes", "Yes", "Yes (during pregnancy)")
master_data$Diabetic <- as.numeric(factor(master_data$Diabetic, levels = diabetic_levels)) - 1

# Ordinal encode GenHealth
genhealth_levels <- c("Excellent", "Very good", "Good", "Fair", "Poor")
master_data$GenHealth <- as.numeric(factor(master_data$GenHealth, levels = genhealth_levels)) - 1

# Standardize SleepTime, PhysicalHealth, MentalHealth, and BMI
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
master_data$SleepTime <- standardize(master_data$SleepTime)
master_data$PhysicalHealth <- standardize(master_data$PhysicalHealth)
master_data$MentalHealth <- standardize(master_data$MentalHealth)
master_data$BMI <- standardize(master_data$BMI)

# Ordinal encode AgeCategory
agecategory_levels <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older")
master_data$AgeCategory <- factor(master_data$AgeCategory, levels = agecategory_levels, ordered = TRUE)
master_data$AgeCategory_encoded <- as.numeric(master_data$AgeCategory)

# Drop original AgeCategory column and rename AgeCategory_recoded as AgeCategory
master_data <- master_data %>% select(-AgeCategory) %>% rename(AgeCategory = AgeCategory_encoded)

# Encode 'target variable' (HeartDisease) with "No" == 0
master_data$HeartDisease <- ifelse(master_data$HeartDisease == "No", 0, 1) 
master_data$HeartDisease <- as.factor(master_data$HeartDisease)

# Check the structure of the modified data
str(master_data) # Data successfully transformed and encoded

# Split the Data into Training and Testing Sets (80/20 Split)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(master_data$HeartDisease, p = .8, list = FALSE)
trainData <- master_data[trainIndex, ]
testData <- master_data[-trainIndex, ]

# Perform combination of Under- and Oversampling Method using ovun.sample() for balancing
library(ROSE)
balanced_trainData <- ovun.sample(
  HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + BMI + 
    Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
    SkinCancer + Stroke + Diabetic + Race, 
  data = trainData, method = "both", p = 0.5, N = nrow(trainData))$data

# Ensure balanced_trainData is a data.frame
library(data.table)
balanced_trainData <- as.data.frame(balanced_trainData)

# Define the model formula with HeartDisease as dependent variable, other factors as independent
model <- HeartDisease ~ Smoking + AlcoholDrinking + DiffWalking + PhysicalActivity + SleepTime + BMI + 
  Sex + AgeCategory + PhysicalHealth + MentalHealth + GenHealth + Asthma + KidneyDisease + 
  SkinCancer + Stroke + Diabetic + Race

# Binary Logistic Regression:
logistic_model <- glm(formula = model, data = balanced_trainData, family = binomial)
logistic_preds <- predict(logistic_model, newdata = testData, type = "response")
logistic_class <- ifelse(logistic_preds > 0.5, 1, 0)


# Naive Bayes:
nb_model <- naiveBayes(formula = model, data = balanced_trainData)
nb_preds <- predict(nb_model, newdata = testData, type = "raw")
nb_class <- ifelse(nb_preds[,2] > 0.5, 1, 0)


# Decision Tree with ctree:
tree_model <- ctree(formula = model, data = balanced_trainData)
tree_preds <- predict(tree_model, newdata = testData, type = "prob")
# Check and handle the output of predict
if (is.list(tree_preds)) {
  tree_probs <- sapply(tree_preds, function(x) x[2])
} else {
  tree_probs <- tree_preds[, 2]
}
tree_class <- ifelse(tree_probs > 0.5, 1, 0)

# Random Forest:
rf_model <- randomForest(formula = model, data = balanced_trainData)
rf_preds <- predict(rf_model, newdata = testData, type = "prob")
rf_class <- ifelse(rf_preds[,2] > 0.5, 1, 0)

# Evaluate the models and Calculate Accuracy and AUC:
# Function to calculate Accuracy and AUC
evaluate_model <- function(true, predicted_prob, predicted_class) {
  accuracy <- mean(true == predicted_class)
  
  pred <- prediction(predicted_prob, true)
  perf <- performance(pred, measure = "auc")
  auc <- perf@y.values[[1]]
  
  return(list(accuracy = accuracy, auc = auc))
}

# Evaluate Logistic Regression
logistic_eval <- evaluate_model(testData$HeartDisease, logistic_preds, logistic_class)
print(logistic_eval) # The BLR overall accuracy is 75% (0.7545); and the AUC IS 84.04


# Evaluate Naive Bayes
nb_eval <- evaluate_model(testData$HeartDisease, nb_preds[,2], nb_class)
print(nb_eval) # The NB model overall accuracy is 80% (0.7992); and the AUC is 80.54

# Evaluate Decision Tree
tree_eval <- evaluate_model(testData$HeartDisease, tree_probs, tree_class)
print(tree_eval) # The DT model overall accuracy is 72% (0.7231); and the AUC is 81.15

# Evaluate Random Forest
rf_eval <- evaluate_model(testData$HeartDisease, rf_preds[,2], rf_class)
print(rf_eval) # The RF model overall accuracy is 80% (0.8043); and the AUC is 81.88
  
# Create a summary data frame for model performance
model_performance <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(logistic_eval$accuracy, nb_eval$accuracy, tree_eval$accuracy, rf_eval$accuracy),
  AUC = c(logistic_eval$auc, nb_eval$auc, tree_eval$auc, rf_eval$auc)
)

print(model_performance)
# Conclusion: 
# The overall Accuracy results show that the Best Accuracy is Random Forest at 80% (0.8043); the 
# Second Best Accuracy is Naive Bayes at  80% (0.7992); the Third Best Accuracy is Binary Logistic 
# Regression at 75% (0.7545); and the model with Least Accuracy is Decision Tree at 72% (0.7231).
# The overall AUC scores show that the model with the Best AUC is Binary Logistic Regression at 
# 84% (0.8404); the Second Best AUC is Random Forest model at 82% (0.8188), the Third Best AUC is 
# Decision Tree at 81% (0.8115); and the model with the Least AUC is Naive Bayes at 81% (0.8054).
# If accuracy is prioritised in the combination of Accuracy and AUC, the Random Forest may be 
# considered to have performed best with the highest accuracy, with the Binary Logistic Regression 
# coming Second Best model; the Naive Bayes Third Best Model; and the Decision Tree being the 
# Least Performing Model.
# However, given that the AUC score often provides a more comprehensive view of model performance, 
# especially in imbalanced datasets, the Binary Logistic Regression with the highest overall AUC 
# score of 84.04 proves to be the best model at distinguishing between the HeartDisease classes 
# (Yes/No) despite having lower accuracy at 75%. The Random Forest came second with AUC of 81.88, 
# the Naive Bayes came third with AUC of 80.54 and the Decision Tree has the least AUC of 81.15.


# Saving the generated 'model_performance' in Excel file for use in PowerBI Dashboarding Report
# Install and load necessary library
install.packages("writexl")
library(writexl)

# Rank according to AUC and Create the data frame
model_performance <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(0.7544795, 0.7991807, 0.7230526, 0.8042622),
  AUC = c(0.8404063, 0.8053635, 0.8115049, 0.8188498)
)

# Create the data frame with the revised ranking
model_performance <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Decision Tree", "Naive Bayes"),
  Accuracy = c(0.7544795, 0.8042622, 0.7230526, 0.7991807),
  AUC = c(0.8404063, 0.8188498, 0.8115049, 0.8053635)
)

# Balancing method used "Combination of Under- and Over-Sampling Method" (or Combination Method)
model_performance$BalancingMethod <- "Combination Method"

# Print the data frame to verify
print(model_performance)

# Write the data frame to an Excel file
write_xlsx(model_performance, "model_performance2.xlsx")

# Confirm the file is created
file.exists("model_performance2.xlsx") # Successfully created for use in PowerBI Dashboard Report.
