#####################################################
#title: "Heart Disease Report"
#author: "Humberto Garza"
#date: "21/NOV/2022"
#####################################################

## Data Acquisition

if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

library(dslabs)
library(tidyverse)
library(caret)
library(data.table)
library(gridExtra)
library(matrixStats)
library(xgboost)


# The heart disease data set is available for download at the following link in csv format:
# https://drive.google.com/file/d/1-UJK8-oj9jrJh1eT5GQfUc1tYKP094fl/view?usp=sharing

# Of from Github project webpage:
# https://github.com/EDXGarza/HeartDisease_PH125.9x/blob/main/heart_disease.csv

# Or from the original creator of the dataset in zip format:
# https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset/download?datasetVersionNumber=3

# Once downloaded and extracted the data set can be loaded into R with:

hd <- read.csv("/FILE_LOCATION/heart_disease.csv")


# Analysis
# Exploratory Data Analysis
# We will start to explore the relation that exist, between each of the variables of the data set and the presence of disease.

# Check if there are any missing values in our data set.


check_na <- hd %>%  summarize(across(.cols = everything(), 
.fns = ~ any(is.na(.x)), .names = "{col}"))


check_na[1:8] %>% knitr::kable()
check_na[9:14] %>% knitr::kable()
check_na[15:21] %>% knitr::kable()


# The first thing we notice is a huge imbalance in the amount of healthy cases vs disease.

tab <- table(hd$HeartDiseaseorAttack)
hd %>% summarize(Disease_cases = sum(HeartDiseaseorAttack == 1), 
Healthy_cases = sum(HeartDiseaseorAttack == 0)) %>% knitr::kable()


# There are:
round(tab[1] / tab[2], 2)
# times more healthy cases than disease cases


# We will balance the data set using random samples and plot the different variables against the disease cases, to examine the relation between them.


# Create index of the disease cases
disease_index <- which(hd$HeartDiseaseorAttack == 1)
# Create index of the healthy cases
health_index <- which(hd$HeartDiseaseorAttack == 0)
# Extract a sample of healthy cases the same length as the disease cases
set.seed(9, sample.kind = "Rounding")
ind <- sample(health_index, length(disease_index), replace = FALSE)
# Join the two index variables
balanced_ind <- union(disease_index, ind)
# Make dataset with balanced outcomes
balanced <- hd[balanced_ind,]
# Rename the first column to a shorter name
balanced <- balanced %>% rename(Disease = HeartDiseaseorAttack)




# The following variables in our data set consist of binary variables:  
# "Disease", "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "DiffWalk", "Sex".



# While the rest are numerical discrete:
# "BMI", "Diabetes", "GenHlth", "MentHlth", "PhysHlth", "Age", "Education", "Income".


# Charts assesment

# Make bar plots for binary variables:


# HighBP
p1 <- balanced %>% group_by(Disease) %>% select(Disease, HighBP) %>%
table() %>% data.frame() %>% ggplot(aes(HighBP, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# HighChol
p2 <- balanced %>% group_by(Disease) %>% select(Disease, HighChol) %>%
table() %>% data.frame() %>% ggplot(aes(HighChol, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

#Stroke
p3 <- balanced %>% group_by(Disease) %>% select(Disease, Stroke) %>%
table() %>% data.frame() %>% ggplot(aes(Stroke, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# PhysActivity
p4 <- balanced %>% group_by(Disease) %>% select(Disease, PhysActivity) %>%
table() %>% data.frame() %>% ggplot(aes(PhysActivity, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

grid.arrange(p1, p2, p3, p4, ncol = 2)




bpprop <- balanced %>% select(Disease, HighBP) %>% group_by(Disease) %>% 
table() %>% data.frame() %>% filter(Disease == 1) %>% 
summarize(pr = Freq[HighBP == 1]/sum(Freq) * 100 ) %>% pull(pr)

cholprop <- balanced %>% select(Disease, HighChol) %>% group_by(Disease) %>% 
table() %>% data.frame() %>% filter(Disease == 1) %>% 
summarize(pr = Freq[HighChol == 1]/sum(Freq) * 100 ) %>% pull(pr)

smokeprop <- balanced %>% select(Disease, Smoker) %>% group_by(Disease) %>% 
table() %>% data.frame() %>% filter(Disease == 1) %>% 
summarize(pr = Freq[Smoker == 1]/sum(Freq) * 100 ) %>% pull(pr)

sexprop <- balanced %>% select(Disease, Sex) %>% group_by(Disease) %>% 
table() %>% data.frame() %>% filter(Disease == 1) %>% 
summarize(pr = Freq[Sex == 1]/sum(Freq) * 100 ) %>% pull(pr)



# We get some interesting insights from the bar plots, for an instance, 
round(bpprop, 2) # percent of the disease cases present high blood Pressure, similar situation with 
round(cholprop, 2) # percent of the disease cases showing high cholesterol


# Smoker
p5 <- balanced %>% group_by(Disease) %>% select(Disease, Smoker) %>% 
table() %>% data.frame() %>% ggplot(aes(Smoker, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# DiffWalk
p6 <- balanced %>% group_by(Disease) %>% select(Disease, DiffWalk) %>% 
table() %>% data.frame() %>% ggplot(aes(DiffWalk, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# Sex
p7 <- balanced %>% group_by(Disease) %>% 
select(Disease, Sex) %>% table() %>% 
data.frame() %>% ggplot(aes(Sex, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

grid.arrange(p5, p6, p7, ncol = 2)



# Here we observe the proportion of disease cases that reported being smokers is:
round(smokeprop, 2) # percent, also heart disease is more prevalent in one gender than the other 
round(sexprop, 2) # percent


# HvyAlcoholConsump
p8 <- balanced %>% group_by(Disease) %>% select(Disease, HvyAlcoholConsump) %>%
table() %>% data.frame() %>% 
ggplot(aes(HvyAlcoholConsump, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# NoDocbcCost
p9 <- balanced %>% group_by(Disease) %>% select(Disease, NoDocbcCost) %>% 
table() %>% data.frame() %>% ggplot(aes(NoDocbcCost, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# CholCheck
p10 <- balanced %>% group_by(Disease) %>% select(Disease, CholCheck) %>% 
table() %>% data.frame() %>% ggplot(aes(CholCheck, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# Fruits
p11 <- balanced %>% group_by(Disease) %>% 
select(Disease, Fruits) %>% table() %>%
data.frame() %>% ggplot(aes(Fruits, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# Veggies
p12 <- balanced %>% group_by(Disease) %>% select(Disease, Veggies) %>% 
table() %>% data.frame() %>% ggplot(aes(Veggies, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

# AnyHealthcare
p13 <- balanced %>% group_by(Disease) %>% 
select(Disease, AnyHealthcare) %>% table() %>% 
data.frame() %>% ggplot(aes(AnyHealthcare, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)



grid.arrange(p8, p9, p10, p11, p12, p13, ncol = 3)


# In general we observe a very similar distribution of cases for the majority of the variables with minor variations.
# Next we will examine the numerical discrete variables.


# NUMERICAL DISCRETE
# "Diabetes", "GenHlth", "MentHlth", "PhysHlth", "Age", "Education", "Income"

p14 <- balanced %>% group_by(Diabetes) %>% select(Diabetes, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(Diabetes, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

p15 <- balanced %>% group_by(GenHlth) %>% select(GenHlth, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(GenHlth, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

p18 <- balanced %>% group_by(Age) %>% select(Age, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(Age, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

p19 <- balanced %>% group_by(Education) %>% select(Education, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(Education, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

dd2 <- balanced %>% select(Disease, Diabetes) %>% 
summarize(dd2 = sum(Diabetes == 2 & Disease == 1)/sum(Disease == 1) * 100)

hd2 <- balanced %>% select(Disease, Diabetes) %>% 
summarize(dd2 = sum(Diabetes == 2 & Disease == 0)/sum(Disease == 0) * 100)                                                                    

grid.arrange(p14, p15, p18, p19, ncol = 2)





# The **Diabetes** variable plot shows the group 2 with disease has a 
round(dd2, 2) # percent of cases compared to 
round(hd2, 2) # percent for the healthy in the same group. 

# In the **GenHlth** variable we observe that groups 3, 4 and 5 have in general more disease cases and in groups 1 and 2 the healthy cases are more predominant. 

# We have a right skewed distribution for disease in the groups of the **Age** variable plot, in contrast to a more centered distribution for the healthy cases.

# In the **Education** variable plot we see more disease in groups 2 through 5, but in group the group 6 the trend reverses



p16 <- balanced %>% group_by(MentHlth) %>% select(MentHlth, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(MentHlth, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(Disease ~ .)

p17 <- balanced %>% group_by(PhysHlth) %>% select(PhysHlth, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(PhysHlth, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(Disease ~ .)

p20 <- balanced %>% group_by(Income) %>% select(Income, Disease) %>%
table() %>% data.frame() %>% ggplot(aes(Income, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(. ~ Disease)

p21 <- balanced %>% group_by(BMI) %>% select(BMI, Disease) %>% 
filter(BMI < 50) %>% table() %>% data.frame() %>% 
ggplot(aes(BMI, Freq, fill = Disease)) + 
geom_bar(stat = "identity") + facet_grid(Disease ~ .) + 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

dm <- balanced %>% select(Disease, MentHlth) %>% filter(MentHlth == 0) %>% 
summarize(dm = sum(Disease == 1)) %>% pull(dm)

hm <- balanced %>% select(Disease, MentHlth) %>% filter(MentHlth == 0) %>% 
summarize(hm = sum(Disease == 0)) %>% pull(hm)

print(p16)



#In the mental health plot the data is scattered among the 31 groups, but a big concentration occurs in the group 0 with 
dm # disease cases vs 
hm # healthy

print(p17)

# We also have scattered data in the **PhysHlth** variable with big concentrations in the groups 0 and 30


grid.arrange(p20, p21, nrow = 2)


# A slightly bigger count of disease cases in the **Income** variable groups 1 through 6, and the tendency reverses on the groups 7 and 8.

# The **BMI** variable has a distribution centered around the 27 value with a long right tail, this plot doesn't look very informative from this perspective.


# Heatmap
# In this data set we count with 21 predictors, we start by scaling the dataset to plot the heatmap and perform a Principal Component Analysis.


# Saving predictors of full data set into variable x
x <- as.matrix(hd[, 2:22])
# Subtract the column means of x
x_centered <- sweep(x, 2, colMeans(x))
# Divide by the column standard deviations
x_scaled <- sweep(x_centered, 2, colSds(x), FUN = "/")
# Calculate the distance between all samples
d_features <- dist(t(x_scaled))
# Plot heatmap
heatmap(as.matrix(d_features), scale = "column", col = rainbow(256))


# If we look closely we find a few clusters with relevant correlation between predictors,  but other than that we can't determine the variables that could drive an effective predictive algorithm.


# Dendogram
#Performing hierarchical clustering on the 21 features


# Make dendogram of predictors
h <- hclust(d_features)
plot(h)


# Principal Components
#Performing Principal Component Analysis calculation on the full data set


pca <- prcomp(x_scaled)
extr <- summary(pca)

# Boxplot of PCA
data.frame(type = ifelse(hd[,1] == 0, "Healthy", "Disease"), pca$x) %>% 
gather(key = "PC", value = "value", -type) %>% 
ggplot(aes(PC, value, fill = type)) + geom_boxplot() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))


# We have significant overlap in the interquartile ranges of the principal components, with a considerable amount of outliers, so we don't have a component different enough to differentiate between healthy cases and the ones with disease



# Display Principal Components Values:


extr$importance[,1:6]
extr$importance[,7:11]
extr$importance[,12:16]
extr$importance[,17:21]


# The cumulative variance proportion reaches more than 90% until the component 17, we could use only those 17 predictors for our model but since the data set dimensions aren't cumbersome we are using the 21 predictors anyways


# Models

# Split data set into training and testing. we will split the full data set instead of the balanced version we created because the accuracy of the predictions depends heavily on the amount of samples we use for the training of our models.


#Spit data set into training and testing sets

set.seed(9, sample.kind = "Rounding")

test_index <- createDataPartition(y = hd$HeartDiseaseorAttack, 
times = 1, p = 0.2, list = FALSE)

# Extract training set from full data set
train_set <- hd[-test_index, ]
# Extranct test set from full data set
test_set <- hd[test_index, ]

# Predictors for train set
train_x <- train_set[, 2:22]
# Outcomes for train_set
train_y <- train_set[,1]
# Predictors for test set
test_x <- test_set[, 2:22]
# Outcomes for test_set
test_y <- test_set[,1]



# Random Forest Model:

For a first approach we use the random forest implementation in the Rborist package, because is faster than the one in the randomForest package.

Random forests are frequently used because they generate reasonable predictions across a wide range of data while requiring little configuration.


set.seed(9, sample.kind = "Rounding")
# We will use only 5-fold cross validation
control <- trainControl(method="cv", number = 5, p = 1)

grid <- expand.grid(minNode = c(1) , predFixed = c(10, 13, 16, 21))

train_rf <- train(train_x, as.factor(train_y), method = "Rborist", 
nTree = 50, trControl = control, tuneGrid = grid, nSamp = 10000)

# Calculate the confusion matrix
cm <- confusionMatrix(predict(train_rf, test_x), as.factor(test_y))

# Show accuracy of the model
rf_acc <- cm$overall["Accuracy"]

models <- tibble(Method = "Random Forest", Accuracy = rf_acc)

models %>% knitr::kable()



# Extreme Gradient Boosting Model

# Xgboost (extreme gradient boosting) is an advanced version of the gradient descent boosting technique, which is used for increasing the speed and efficiency of computation of the algorithm.

# xgboost
# Define training and testing sets
xgb_train <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)

# Define final model
model_xgboost <- xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

# Use model to make predictions on test data
pred_y <- predict(model_xgboost, xgb_test)



#Transform the regression in a binary classification:

#The only thing that XGBoost does is a regression. XGBoost is using label vector to build its regression model.

#If we think about the meaning of a regression applied to our data, the numbers we get are probabilities that a value will be classified as 1. Therefore, we will set the rule that if this probability for a specific value is > 0.5 then the observation is classified as 1 or 0 otherwise.



# Transform prediction to binary
prediction <- as.numeric(pred_y > 0.5)
# Calculate accuracy
xgb_acc <- mean(prediction == test_y)

models <- bind_rows(models, tibble(Method = "XGBoost", Accuracy = xgb_acc))

models[2,] %>% knitr::kable()




# Results
models %>% knitr::kable()

# Our Random Forest prediction algorithm from the Rborist pakage delivered an accuracy of 
# rf_acc * 100 # percent, it reached its maximum accuracy by using the 21 predictors available in the data set.
# summary(train_rf)
# train_rf$results
# Meanwhile the extreme gradient boosting algorithm delivered an accuracy of 
# r xgb_acc * 100  # percent, a slight improvement over the random forest prediction, but it finishes the computations in a very short amount of time.
# summary(model_xgboost)


# Conclusion

# We obtained an accuracy above 90% with both algorithms which according to the machine learning literature is within an acceptable range and with very little tuning of parameters, we consider that the goal of the report was achieved for now, but keeping in mind that there is always room for improvement, each algorithm library has many parameters that can be tweaked to improve performance, as we learn more about them the more we will try to implement these improvements in the future and to explore alternative methods of predictions.
