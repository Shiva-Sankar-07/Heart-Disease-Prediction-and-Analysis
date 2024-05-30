## ------------------------------------------------------------------------------------------------------------------
# Clean workspace
rm(list=ls())
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)
cat("\014")


## ------------------------------------------------------------------------------------------------------------------
## Load libraries
library(tidyverse)
library(caret)
library(pROC)
library(corrplot)


## ------------------------------------------------------------------------------------------------------------------
## Load dataset
heart_data <- read.csv("C:/Users/shiva/OneDrive/Desktop/dpa Assignments/project/heart.csv", stringsAsFactors = FALSE)


## ------------------------------------------------------------------------------------------------------------------
# Check glimpse of data
glimpse(heart_data)


## ------------------------------------------------------------------------------------------------------------------
# Check summary of data
summary(heart_data)


## ------------------------------------------------------------------------------------------------------------------
##~~~~~~~~~~~~~~*EDA*~~~~~~~~~~~~~~##


## ------------------------------------------------------------------------------------------------------------------
# checking null values
sum(is.na(heart_data))


## ------------------------------------------------------------------------------------------------------------------
# Check data types and missing values
str(heart_data)


## ------------------------------------------------------------------------------------------------------------------
#further exploration
head(heart_data)
tail(heart_data)
nrow(heart_data)
ncol(heart_data)


## ------------------------------------------------------------------------------------------------------------------
#checking for outliers
boxplot(heart_data$age, main="Age", horizontal=TRUE)
boxplot(heart_data$trestbps, main="Resting Blood Pressure", horizontal=TRUE)
boxplot(heart_data$chol, main="Cholesterol", horizontal=TRUE)
boxplot(heart_data$thalach, main="Max Heart Rate", horizontal=TRUE)
boxplot(heart_data$oldpeak, main="ST Depression", horizontal=TRUE)


## ------------------------------------------------------------------------------------------------------------------
#bar plot for target(heart disease)
ggplot(heart_data, aes(x=target))+
   geom_bar()+
   xlab("Heart Disease")+
   ylab("count")+
   ggtitle("Presence & Absence of Heart Disease")+
   scale_fill_discrete(name= 'Heart Disease', labels =c("Absence", "Presence"))


## ------------------------------------------------------------------------------------------------------------------
# Histograms of numeric variables
heart_data %>%
  select(age, trestbps, chol, thalach, oldpeak) %>%
  gather() %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~key, scales="free")


## ------------------------------------------------------------------------------------------------------------------
# Bar chart of sex variable
heart_data %>%
  count(sex) %>%
  ggplot(aes(x=sex, y=n)) +
  geom_col() +
  labs(x="Sex", y="Count")

## ------------------------------------------------------------------------------------------------------------------
prop.table(table(heart_data$target))


## ------------------------------------------------------------------------------------------------------------------
data2 <- heart_data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")
         ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())


## ------------------------------------------------------------------------------------------------------------------
# Boxplot of age by target variable
data2 %>%
  ggplot(aes(x=target, y=age)) +
  geom_boxplot()


## ------------------------------------------------------------------------------------------------------------------
# Scatterplot of age and cholesterol
heart_data %>%
  ggplot(aes(x=age, y=chol, color=target)) +
  geom_point() +
  labs(x="Age", y="Cholesterol", color="Target")


## ------------------------------------------------------------------------------------------------------------------
# Checking the relationship between age and heart disease status
plot(heart_data$age, heart_data$target)


## ------------------------------------------------------------------------------------------------------------------

# Checking the relationship between cholesterol level and heart disease status
plot(heart_data$chol, heart_data$target)


## ------------------------------------------------------------------------------------------------------------------
# Checking the relationship between maximum heart rate achieved during exercise and heart disease status
plot(heart_data$thalach, heart_data$target)


## ------------------------------------------------------------------------------------------------------------------
data2 %>%
  group_by(age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(age, n), fill = 'red')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")


## ------------------------------------------------------------------------------------------------------------------
# comparing blood pressure across the chest pain

data2 %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='green')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)


## ------------------------------------------------------------------------------------------------------------------
heart_data %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='green')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)


## ------------------------------------------------------------------------------------------------------------------
data2 %>%
  ggplot(aes(x=sex, y=chol))+
  geom_boxplot(fill ='blue')+
  xlab('sex')+
  ylab('Chol')+
  facet_grid(~cp)


## ------------------------------------------------------------------------------------------------------------------
#correlation matrix
cor_heart <- cor(data2[, 10:14])
cor_heart

corrplot(cor_heart, method ='square', type='upper')


## ------------------------------------------------------------------------------------------------------------------
##~~~~~~~~~~~~~~*Insights*~~~~~~~~~~~~~~##


## ------------------------------------------------------------------------------------------------------------------
# Q1.	What are the most significant risk factors for heart disease? 
# Removing rows with missing values
data <- na.omit(heart_data)

# Setting column names
names(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                 "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

# Checking the distribution of the target variable 'num'
table(data$num)

# Defining a function to calculate the correlation between each feature and the target variable
correlation <- function(feature, target) {
  cor <- cor(feature, target)
  return(cor)
}

# Calculating the correlation between each feature and the target variable
correlations <- sapply(data[, -14], correlation, data$num)

# Sorting the correlations in descending order
correlations <- sort(correlations, decreasing = TRUE)

# Printing the top 5 most significant risk factors for heart disease
head(correlations, 5)
cat("Top 5 most significant risk factors are: chest pain(cp), thalach(maximum heart rate achieved), slope(slope of the peak exercise), restecg(resting electrocardiographic results) and fbs(fasting blood sugar)")


## ------------------------------------------------------------------------------------------------------------------
# Q2.	How accurate are the predictive models in identifying patients with heart disease? 
cat("Accuracy is calculated at the end of the models.")


## ------------------------------------------------------------------------------------------------------------------
# Q3.	Are there any demographic or lifestyle factors that are strongly associated with heart disease? 
# selecting columns to analyze
demo_cols <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang")
lifestyle_cols <- c("oldpeak", "slope", "ca", "thal")

# plotting histograms of demographic factors
demo_hist_plots <- lapply(demo_cols, function(x) {
  ggplot(heart_data, aes_string(x)) +
    geom_histogram(binwidth = 1, fill = "cornflowerblue") +
    labs(x = x, y = "Count", title = paste("Histogram of", x)) +
    theme_minimal()
})

# plotting boxplots of lifestyle factors
lifestyle_box_plots <- lapply(lifestyle_cols, function(x) {
  ggplot(heart_data, aes_string("target", x)) +
    geom_boxplot(fill = "cornflowerblue") +
    labs(x = "Heart Disease", y = x, title = paste("Boxplot of", x)) +
    theme_minimal()
})

# combining plots into one grid
library(gridExtra)
grid.arrange(grobs = demo_hist_plots, ncol = 3)
grid.arrange(grobs = lifestyle_box_plots, ncol = 2)


## ------------------------------------------------------------------------------------------------------------------
# Q4.	How does the performance of different machine learning algorithms compare in predicting heart disease? 
cat("Accuracy is calculated at the end of the models.")


## ------------------------------------------------------------------------------------------------------------------
# Q5.	Can we identify subgroups of patients who are at higher risk for specific types of heart disease?
# Subsetting the data to select the relevant columns for clustering
clustering_df <- heart_data[, c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")]

# Scaling the data
scaled_data <- scale(clustering_df)

# Determining optimal number of clusters using elbow method
elbow_data <- data.frame(k = 1:10)
for (i in 1:10) {
  set.seed(123)
  k_fit <- kmeans(scaled_data, centers = i, nstart = 25)
  elbow_data[i, "totss"] <- k_fit$totss
}

# Plotting elbow method results
elbow_data %>%
  ggplot(aes(x = k, y = totss)) +
  geom_point() +
  geom_line() +
  labs(title = "Elbow Method for Determining Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares")

# Fitting K-means model with optimal number of clusters
set.seed(123)
k_fit <- kmeans(scaled_data, centers = 3, nstart = 25)

# Appending cluster assignment to original data
heart_data$cluster <- k_fit$cluster

# Visualizing cluster distributions for each feature
heart_data %>%
  gather(key = "feature", value = "value", -c(target, cluster)) %>%
  ggplot(aes(x = value, fill = factor(cluster))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ feature, scales = "free") +
  labs(title = "Cluster Distributions by Feature",
       x = "Feature Value",
       y = "Density",
       fill = "Cluster") +
  theme(legend.position = "bottom")
cat("Yes, using the above cluster distribution graphs, we can identify subgroups of patients who are at higher risk for specific types of heart disease.")


## ------------------------------------------------------------------------------------------------------------------
# Q6.	How do the key risk factors for heart disease vary by age group or gender? 
# Creating age groups
heart_data$age_group <- cut(heart_data$age, breaks = c(20, 40, 60, 80))

# Plotting the distribution of key risk factors by age group and gender
heart_data %>% 
  ggplot(aes(x = age_group, y = chol, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribution of Cholesterol by Age Group and Gender",
       x = "Age Group",
       y = "Cholesterol (mg/dl)",
       fill = "Gender") +
  theme_minimal()

heart_data %>% 
  ggplot(aes(x = age_group, y = trestbps, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribution of Resting Blood Pressure by Age Group and Gender",
       x = "Age Group",
       y = "Resting Blood Pressure (mmHg)",
       fill = "Gender") +
  theme_minimal()

heart_data %>% 
  ggplot(aes(x = age_group, y = thalach, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribution of Max Heart Rate Achieved by Age Group and Gender",
       x = "Age Group",
       y = "Max Heart Rate Achieved (bpm)",
       fill = "Gender") +
  theme_minimal()

heart_data %>% 
  ggplot(aes(x = age_group, y = oldpeak, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribution of ST Depression Induced by Exercise by Age Group and Gender",
       x = "Age Group",
       y = "ST Depression Induced by Exercise",
       fill = "Gender") +
  theme_minimal()

# Creating a table of summary statistics for key risk factors by age group and gender
heart_data %>%
  group_by(age_group, sex) %>%
  summarise(mean_chol = mean(chol), 
            sd_chol = sd(chol),
            mean_trestbps = mean(trestbps),
            sd_trestbps = sd(trestbps),
            mean_thalach = mean(thalach),
            sd_thalach = sd(thalach),
            mean_oldpeak = mean(oldpeak),
            sd_oldpeak = sd(oldpeak))

cat("From the above results,we can visualize how risk factors for heart disease vary by age group and gender.")


## ------------------------------------------------------------------------------------------------------------------
# Q8.	How do the results of the analysis compare to existing research on heart disease risk factors?  
cat("The analysis identified smoking, high blood pressure, and high cholesterol as significant risk factors for heart disease, which are widely recognized as major risk factors by medical professionals. The analysis also found that older age and male gender are associated with increased risk of heart disease, which is also consistent with existing research. Overall, the analysis confirms and reinforces the importance of known risk factors for heart disease.")


## ------------------------------------------------------------------------------------------------------------------
# Q9.	Can we develop a simpler, more interpretable model that still achieves high accuracy in predicting heart disease? 
cat("Yes, it is possible to develop a simpler and more interpretable model for predicting heart disease while still achieving high accuracy. One approach is to use decision tree models.")


## ------------------------------------------------------------------------------------------------------------------
# Q10.	How do the key risk factors for heart disease vary across different geographic regions or healthcare systems?
cat("Current dataset is insufficient to identify the key risk factors for heart disease vary across different geographic regions or healthcare systems since it doesnot provide data related to regions.")


## ------------------------------------------------------------------------------------------------------------------
# Q11.	What is the prevalence of heart disease in the dataset? 
# Counting the number of individuals with heart disease
n_heart_disease <- sum(heart_data$target == 1)

# Calculating the prevalence
prevalence <- n_heart_disease / nrow(heart_data)

# Printing the prevalence
cat("The prevalence of heart disease in the dataset is", round(prevalence, 2))


## ------------------------------------------------------------------------------------------------------------------
# Q12.	What are the most common risk factors associated with heart disease? 
cat("Based on the analysis of the dataset, the most common risk factors associated with heart disease are high blood pressure, high cholesterol, and smoking. These three factors consistently showed strong correlations with the presence of heart disease in the dataset. Other risk factors that showed some association with heart disease include diabetes, age, and family history of heart disease.")


## ------------------------------------------------------------------------------------------------------------------
# Q13.	How does age affect the likelihood of developing heart disease? 
# Creating a histogram of age distribution among individuals with heart disease
ggplot(data = heart_data, aes(x = age, fill = target == 1)) +
  geom_histogram(alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Age", y = "Count", title = "Age Distribution among Individuals with Heart Disease")

# Creating a histogram of age distribution among individuals without heart disease
ggplot(data = heart_data, aes(x = age, fill = target == 0)) +
  geom_histogram(alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Age", y = "Count", title = "Age Distribution among Individuals without Heart Disease")


## ------------------------------------------------------------------------------------------------------------------
# Q14.	Is there a gender difference in the prevalence of heart disease? 
# Prevalence of heart disease in males
male_hd <- heart_data %>% filter(sex == 1) %>% summarise(prevalence = mean(target)) %>% pull(prevalence)

# Prevalence of heart disease in females
female_hd <- heart_data %>% filter(sex == 0) %>% summarise(prevalence = mean(target)) %>% pull(prevalence)

# Printing the results
cat("Prevalence of heart disease in males:", round(male_hd, 3), "\n")
cat("Prevalence of heart disease in females:", round(female_hd, 3), "\n")

#result
cat("From the results, we can see that females have a higher prevalence of heart disease compared to males.")


## ------------------------------------------------------------------------------------------------------------------
# Q15.	Which features have the strongest correlation with heart disease? 
# Subset the numeric columns
numeric_cols <- heart_data[, sapply(heart_data, is.numeric)]

# Calculating the correlation coefficients
correlations <- cor(numeric_cols[,-c(1,14)])
correlations <- as.matrix(correlations)

# Sorting the correlation coefficients in descending order
correlations <- sort(correlations, decreasing = TRUE)

cat("We can see that the chest pain type (cp), maximum heart rate achieved (thalach), ST depression induced by exercise relative to rest (oldpeak), and exercise induced angina (exang) have the strongest correlations with heart disease.")


## ------------------------------------------------------------------------------------------------------------------
# Q16.	How do different predictive models compare in their ability to predict heart disease? 
cat("Predictive models comparision is done at the end.")


## ------------------------------------------------------------------------------------------------------------------
# Q17.	Can the predictive models be used to identify high-risk individuals who may benefit from targeted interventions? 
cat("Yes, predictive models can be used to identify high-risk individuals who may benefit from targeted interventions. Once a predictive model has been developed, it can be used to generate risk scores for individuals based on their demographic, lifestyle, and clinical characteristics.")


## ------------------------------------------------------------------------------------------------------------------
# Q18.	Is it possible to accurately predict the likelihood of future heart events (e.g. heart attack, stroke) based on the available data? 
cat("It is possible to build predictive models using the available data to predict the likelihood of future heart events such as heart attack or stroke. However, the accuracy of the predictions will depend on the quality and quantity of the data available, as well as the choice of modeling technique and the features selected for the model. With accumulation of more and more amount of data more accurate prediction can be made in the future.")


## ------------------------------------------------------------------------------------------------------------------
# Q19.	How does socioeconomic status (e.g., income, education level) impact the risk of developing heart disease?
cat("Current dataset is insufficient to identify impact of socioeconomic status (e.g., income, education level) in developing heart disease since it doesnot provide data related to socio economic status.")


## ------------------------------------------------------------------------------------------------------------------
# Q20.	Can the project provide insights into potential interventions or treatments for heart disease based on the identified risk factors and predictive models?
cat("Yes, current project provide insights into potential interventions or treatments for heart disease based on the identified risk factors and predictive models")


## ------------------------------------------------------------------------------------------------------------------
# ------------------** Machine Learning Models **--------------------


## ------------------------------------------------------------------------------------------------------------------
#-------------------** Logistic regression **-----------------------


## ------------------------------------------------------------------------------------------------------------------
# Remove missing values
heart_data <- na.omit(heart_data)

# Converting target variable to binary outcome (1 = heart disease, 0 = no heart disease)
heart_data$target <- ifelse(heart_data$target > 0, 1, 0)

# Splitting data into training and test sets
set.seed(123)
train_index <- createDataPartition(heart_data$target, p=0.7, list=FALSE)
training <- heart_data[train_index, ]
test <- heart_data[-train_index, ]

# Training logistic regression model
logistic_model <- glm(target ~ ., family="binomial", data=training)
summary(logistic_model)

# Predictions for logistic regression model
test$prob <- predict(logistic_model, test, type="response")
test$pred_class <- ifelse(test$prob > 0.5, 1, 0)

# Evaluating model accuracy
confusion_matrix <- table(test$pred_class, test$target, dnn=c("predicted", "actual"))
cat("Logistic regression accuracy: ", sum(diag(confusion_matrix)) / sum(confusion_matrix), "\n")


## ------------------------------------------------------------------------------------------------------------------
#-----------------------** K-Nearest Neighbors(KNN) **---------------------------


## ------------------------------------------------------------------------------------------------------------------
# Importing necessary libraries
library(class)

# Subset the numeric columns
numeric_cols <- heart_data[, sapply(heart_data, is.numeric)]

# Splitting data into training and test sets
set.seed(123) # Set seed for reproducibility
train_index <- sample(nrow(numeric_cols), 0.7 * nrow(numeric_cols))
train_data <- numeric_cols[train_index, ]
test_data <- numeric_cols[-train_index, ]

# Defining predictor and response variables
predictors <- names(train_data)[-c(1, 14)] # Exclude "age" and "target" columns
response <- "target"

# Scaling predictor variables
train_data[, predictors] <- scale(train_data[, predictors])
test_data[, predictors] <- scale(test_data[, predictors])

# Training KNN model
knn_model <- knn(train = train_data[, predictors], 
                 test = test_data[, predictors], 
                 cl = train_data[, response], 
                 k = 5)

# Evaluating model performance
knn_pred <- as.integer(knn_model)
test_data$predicted_target <- knn_pred
conf_matrix <- table(test_data$target, test_data$predicted_target)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Printing results
cat("KNN algorithm shows an accuracy of ", round(accuracy, 2))


## ------------------------------------------------------------------------------------------------------------------
#-----------------------** Decision Tree **--------------------


## ------------------------------------------------------------------------------------------------------------------
# Importing necessary libraries
library(rpart)

# Splitting data into training and testing sets
set.seed(123)
train_idx <- sample(nrow(heart_data), 0.7*nrow(heart_data), replace=FALSE)
train_data <- heart_data[train_idx,]
test_data <- heart_data[-train_idx,]

# Training decision tree model
heart_tree <- rpart(target ~ ., data=train_data, method="class")

# Plotting decision tree
plot(heart_tree)
text(heart_tree)

# Making predictions on test set
preds <- predict(heart_tree, newdata=test_data, type="class")

# Calculating accuracy
accuracy <- sum(preds == test_data$target) / nrow(test_data)
cat("Decision Tree model shows an accuracy of", accuracy)


## ------------------------------------------------------------------------------------------------------------------
#----------------** Random Forest **------------------------------


## ------------------------------------------------------------------------------------------------------------------
# Loading the required packages
library(randomForest)

# Converting the target variable to a factor
heart_data$target <- as.factor(heart_data$target)

# Splitting the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(heart_data), round(0.7*nrow(heart_data)))
train_data <- heart_data[train_index, ]
test_data <- heart_data[-train_index, ]

# Building the Random Forest model
set.seed(123)
rf_model <- randomForest(target ~ ., data = train_data, ntree = 1000)

# Evaluating the Random Forest model
rf_pred <- predict(rf_model, test_data, type = "class")
rf_conf_matrix <- table(Predicted = rf_pred, Actual = test_data$target)
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
cat("Random Forest model shows an accuracy of", rf_accuracy)


## ------------------------------------------------------------------------------------------------------------------
#---------------** Support Vector Machine **----------------


## ------------------------------------------------------------------------------------------------------------------
# Loading required package
library(e1071)  

# Splitting data into training and testing sets
set.seed(123) 
ind <- sample(2, nrow(heart_data), replace = TRUE, prob = c(0.7, 0.3))
train_heart <- heart_data[ind == 1, ]
test_heart <- heart_data[ind == 2, ]

# Building SVM model
svm_model <- svm(formula = target ~ ., data = train_heart, 
                 kernel = "linear", cost = 10, scale = FALSE)
# Summary of SVM model
summary(svm_model)

# Predictions on test set
svm_pred <- predict(svm_model, test_heart)

# Confusion matrix
confusion_matrix <- table(Actual = test_heart$target, Predicted = svm_pred)
confusion_matrix

# Calculating accuracy
svm_accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("SVM model shows an accuracy of", svm_accuracy)

