# Import Libraries
library(ggplot2)
library(GGally)
library(dplyr)

# Import Bird Data
bird_data <- read.csv(file.choose(), header = T)
head(bird_data)
str(bird_data)
dim(bird_data)

# Summarise bird type using Principal Component Analysis.
# Subset the Data by removing id and type
pc_bird_data <- subset(bird_data, select = -c(id, type))
head(pc_bird_data)

# Scale the Data and Standardise the features
pc_bird_data <- data.frame(scale(pc_bird_data))
head(pc_bird_data)

# Run PCA on the Data
pc <- princomp(formula = ~., data = pc_bird_data, cor = T)
summary(pc) 
# Interpretation: The first Principal Component (Comm.1) accounts for 85.43% of the 
# data's variance and explains most of the variation; whereas combining both the 
# 1st, 2nd & 3rd components capture approximately 96.24% of the total variance. This 
# clearly suggests that only a very few number of principal components represent 
# effectively the majority of the data's variability.

# Store the 'Principal Component' (PC) scores as a new variable.
# Component Loadings
pc$loadings

# Storing PC Scores in the original Data
bird_data$typecology <- pc$scores[,1] 
head(bird_data) # New column 'typecology' stores calculated values using Ist PC.

# Scree Plot
plot(pc, type = "lines") # First PC clearly explains most of the variation

# Plot of Bird's type wise typecology values
plot(bird_data$typecology)
text(bird_data$typecology, label = bird_data$type, col = 'red', cex = 0.4)

# Correlation Matrix of principal components
round(cor(pc$scores)) # Confirms that Principal Components are uncorrelated

# Find average values of the new variable for each bird type and interpret the results.
# # Group the data by 'type' and calculate the average score for each Bird's type
summary_pc_type <- bird_data %>%
  group_by(type) %>%
  summarise(typecology = round(mean(typecology),3)) %>% as.data.frame()

# Create a Bar Graph for Average Values by Bird Type
ggplot(summary_pc_type, aes(x = type, y = typecology, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Values by Bird Type", x = "Type", y = "Average Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
# Observation: Analysis of the graph shows notable typological disparities in the 
# calculated value metrics of the new variabe 'typecology' for each Bird type: with 
# 'Raptors' (R) type having the most positive values, followed by the 'Swimming 
# Birds' (SW); whereas the 'Singing Birds' (SO) type has the lowest negative values, 
# followed by the 'Scansorial Birds' (P).

# Calculating average values of 'typecology' for each bird type
average_typecology <- aggregate(typecology ~ type, data = bird_data, FUN = mean)

# Print the average values
print(average_typecology)
# Interpretation: The average values of the typecology variable for each Bird type
# shows P:'Scansorial Birds' is -1.6150454, R:'Raptors' is 2.4179824, SO:'Singing 
# Birds' is -2.3993690, SW:'Swimming Birds' is 1.8423457, T:'Terrestrial Birds' is  
# 0.1393812 and W:'Wading Birds' is 0.4048822. This result shows that the 'Raptors' 
# that have the most positive values (2.4179824) followed by 'Swimming Birds' 
# (1.8423457) are positively associated with the first principal component, which 
# tend to confirm also that these bird types have larger bone sizes relative to 
# their ecological habitats. In comparison, the 'Singing Birds' that are having 
# the lowest negative values (-2.3993690) followed by the 'Scansorial Birds' 
# (-1.6150454) are also negatively associated with the first principal component; 
# and that these bird types tend to have smaller bone sizes due to their ecology. 
# Hence, higher magnitude of the 'typecology' score is indicative of stronger 
# associations between the bird's bone measurements and the 1st principal component. 
