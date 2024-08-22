# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra) # Subplot pack
library(RColorBrewer) # Color palette pack

# Load the data
pima = read.csv("pima.txt", sep="\t")
# Here's a description of the data: https://rdrr.io/cran/faraway/man/pima.html
head(pima)
# Check for the data type of pima
str(pima)

## 1.1 Flag Irregularities Steps

# Design columns to convert
columns_to_convert = c("glucose", "diastolic", "triceps", "insulin")
pima_flag = pima #  # replicate pima as pima_flag

# Convert str into numeric
pima[columns_to_convert] = lapply(pima_flag[columns_to_convert], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))
  } else {
    as.numeric(x)
  }
})

# Check for the data type of converted columns
str(pima_flag[columns_to_convert])

# Replace nonsensical values of NA
# Negative pregnant shows a mistake.
pima_flag$pregnant[pima_flag$pregnant < 0] = NA
# Negative test result shows a mistake.
pima_flag$test[pima_flag$test < 0] = NA
# Blood glucose levels should normally be non-negative. 
pima_flag$glucose[pima_flag$glucose <= 0] = NA    
# Diastolic blood pressure should be non-negative. 
pima_flag$diastolic[pima_flag$diastolic <= 0] = NA    
# Triceps skinfold thickness should be non-negative. 
pima_flag$triceps[pima_flag$triceps <= 0] = NA    
# Insulin levels should be non-negative.
pima_flag$insulin[pima_flag$insulin <= 0] = NA     
# BMI should be non-negative.
pima_flag$bmi[pima_flag$bmi <= 0] = NA     
# Values of diabetes should be non-negative.
pima_flag$diabetes[pima_flag$diabetes <= 0] = NA      
# age should be non-negative.
pima_flag$age[pima_flag$age <= 0] = NA     

head(pima_flag)
#head(pima)

# Check for missing values in pima pima_flag
sapply(pima, function(x) sum(is.na(x)))
sapply(pima_flag, function(x) sum(is.na(x)))

# Numerical Summary
summary(pima_flag)

# Data Visulization

# Graphical summary
# Set color theme
set3_colors = brewer.pal(9, "Set3")

par(mfrow=c(3,3))

# Plot histograms to show weirdness
for (i in 1:9) {
    hist(pima[,i], col = set3_colors[i], main = names(pima)[i])
}
par(mfrow=c(1,1))

par(mfrow=c(3,3))

# Plot boxplot to show weirdness
for (i in 1:9) {
    boxplot(pima[,i], col = set3_colors[i], main = names(pima)[i])
    median_value = median(pima[,i], na.rm = TRUE)  # Add median value as text
    text(x = 1, y = median_value, labels = round(median_value, 1), 
         pos = 3, cex = 1, col = "Red")
}
# Reset plotting area to default
par(mfrow=c(1,1))

# Convert data from wide to long format
pima_longdata = pima_flag %>%
  pivot_longer(cols = c(pregnant, glucose, diastolic, triceps, insulin, bmi, diabetes, age), 
               names_to = "Variable", values_to = "Value")


# Plot violin graphs to view overall data outline
ggplot(pima_longdata, aes(x = factor(test), y = Value, fill = factor(test))) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3 ) +  # Split-plane display, one subplot per variable
  labs(x = "Diabetes (0 = No, 1 = Yes)", 
       y = "Value", 
       fill = "Diabetes Status") +  # Set labels
  scale_fill_manual(values = c("0" = "white", "1" = "grey"), 
                    labels = c("No", "Yes")) +  
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",  # Place legend on the right
        strip.text = element_text(hjust = 0.5, face = "bold"),  # Bold facet labels
        axis.text = element_text(size = 8),  
        panel.grid.minor = element_blank()) + 

# Add Q1 and Q3 as vertical lines
  stat_summary(fun.data = function(y) {
      Q1 = quantile(y, 0.25, na.rm = TRUE)
      Q3 = quantile(y, 0.75, na.rm = TRUE)
    return(data.frame(ymin = Q1, ymax = Q3)) # Compute Q1 and Q3
  }, geom = "errorbar", width = 0.2, color = "black") + # Plot vertical lines for Q1 and Q3 

# Add labels for Q1 and Q3
  stat_summary(fun.data = function(y) {
      Q1 = quantile(y, 0.25, na.rm = TRUE)
      Q3 = quantile(y, 0.75, na.rm = TRUE)
    return(data.frame(y = c(Q1, Q3), label = round(c(Q1, Q3), 1)))
  }, geom = "text", aes(label = ..label..), position = position_nudge(x = 0.3), size = 3, fontface = "bold")


## 1.3 Data Cleaning Steps

pima_clean = pima_flag  

# 1. Process `pregnant`, setting any values greater than 13 to 13
pima_clean$pregnant[pima_clean$pregnant > 13] = 13

# 2. Process `glucose`, replace 0 values with median
pima_clean$glucose[is.na(pima_clean$glucose)] = median(pima_clean$glucose, na.rm = TRUE)

# 3. Process `bmi`, replace 0 values with the medians.
pima_clean$bmi[is.na(pima_clean$bmi)] = median(pima_clean$bmi, na.rm = TRUE)

# 4. Process `diastolic`, using regression imputation method

set.seed(123) # Set seed for reproducibility
train_size = floor(0.8 * nrow(pima_clean))   # define the size of training set (80%)
train_indices = sample(seq_len(nrow(pima_clean)), size = train_size)  # Randomly pick up rows for sample

# Split the data into training and testing sets
train = pima_clean[train_indices, ]
test = pima_clean[-train_indices, ]

# Create linear Regression model
lm_model = lm(diastolic ~ pregnant + glucose + bmi + diabetes + age, #
               data = train)

# Predicted values in `diastolic`
predicted_diastolic = predict(lm_model, newdata = test)

# Replace 0 values with predicted values
pima_clean$diastolic[is.na(pima_clean$diastolic)] = predicted_diastolic[is.na(test$diastolic)]

# 5. Process `triceps` using random imputation method
# Step 1: Calculate Q1 and Q3 for non-NA values in triceps
triceps_non_na = pima_clean$triceps[!is.na(pima_clean$triceps)]
triceps_q1 = quantile(triceps_non_na, 0.25, na.rm = TRUE)
triceps_q3 = quantile(triceps_non_na, 0.75, na.rm = TRUE)

# # Step 2: Perform random imputation within the range [Q1, Q3]
set.seed(123)  # For reproducibility
pima_clean$triceps[is.na(pima_clean$triceps)] = sample(seq(triceps_q1, triceps_q3, by = 0.1), 
                                                      sum(is.na(pima_clean$triceps)), replace = TRUE)

# Further IQR check and process outliers
triceps_iqr = IQR(pima_clean$triceps, na.rm = TRUE)
triceps_upper = quantile(pima_clean$triceps, 0.75, na.rm = TRUE) + 1.5 * triceps_iqr
pima_clean$triceps[pima_clean$triceps > triceps_upper] = triceps_upper

# 6. Process Insulin using targeted random imputation
# Step 1: Calculate Q1 and Q3 for non-NA values in insulin
insulin_non_na = pima_clean$insulin[!is.na(pima_clean$insulin)]
insulin_q1 = quantile(insulin_non_na, 0.25, na.rm = TRUE)
insulin_q3 =quantile(insulin_non_na, 0.75, na.rm = TRUE)

# Step 2: Perform random imputation within the range [Q1, Q3]
set.seed(123)  # For reproducibility
pima_clean$insulin[is.na(pima_clean$insulin)] = sample(seq(insulin_q1, insulin_q3, by = 1), 
                                                        sum(is.na(pima_clean$insulin)), replace = TRUE)

# The cleaned data with imputed insulin values
head(pima_clean)


# The cleaned data is now stored in pima_clean
sapply(pima_clean, function(x) sum(is.na(x)))
 
       
# Plotting data
# Step 1: Add a 'dataset' column to both original and cleaned data
pima$dataset = "Raw"  
pima_clean$dataset = "DC"  # DC data set

# Step 2: List of variables to plot
variables = c("pregnant", "glucose", "bmi", "diastolic", "triceps", "insulin")

# Step 3: Create density plots for each variable in a loop
plot_list = lapply(variables, function(var) {
  combined_data = rbind(
    data.frame(value = pima[[var]], dataset = "Raw"),
    data.frame(value = pima_clean[[var]], dataset = "DC")
  )
  
  ggplot(combined_data, aes(x = value, fill = dataset)) +
    geom_density(alpha = 0.5) +
    labs(title = paste(var),
         x = paste(toupper(substring(var, 1, 1)), substring(var, 2)), 
         y = "Density") +
    theme_minimal(base_size = 10) +
    scale_fill_manual(values = c("Raw" = "grey", "DC" = "skyblue")) +
    theme(legend.position = "right")
})

# Step 4: Arrange all plots in a grid (3x2)
grid.arrange(grobs = plot_list, nrow = 3, ncol = 2)

# Split the data into training and testing sets       
set.seed(123) 
train_size = floor(0.8 * nrow(pima_clean))   
train_indices = sample(seq_len(nrow(pima_clean)), size = train_size)  
       
train = pima_clean[train_indices, ]
test = pima_clean[-train_indices, ]

# Fit the logistic regression model
GLMmodel = glm(test ~ pregnant+glucose+diastolic+triceps+insulin+bmi+diabetes+age, 
             data = train, 
             family = binomial)

# Summary of the model
summary(GLMmodel)

par(mfrow = c(2,2)); plot(GLMmodel)

# Get a summary of the model
lm_diastolic = lm(test ~ diastolic,data = train)
summary(lm_diastolic)

# Get a summary of the model
beta_glucose = coef(GLMmodel)["glucose"]
beta_glucose

# Make predictions on the test set
predicted_prob = predict(GLMmodel, newdata = test, type = "response")
predicted_class = ifelse(predicted_prob > 0.5, 1, 0)  # Convert probabilities to binary outcomes

# Construct the confusion matrix
actual_class = test$test
confusion_matrix = table(Predicted = predicted_class, Actual = actual_class)

# Display the confusion matrix
confusion_matrix

# Values from the confusion matrix
TP = 89  # True Positives
TN = 23  # True Negatives
FP = 13   # False Positives
FN = 29  # False Negatives

# Accuracy
accuracy = (TP + TN) / (TP + TN + FP + FN)

# Precision
precision = TP / (TP + FP)

# Recall
recall = TP / (TP + FN)

# F1 Score
f1_score = 2 * (precision * recall) / (precision + recall)

# Display the results
accuracy
precision
recall
f1_score

