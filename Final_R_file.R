library(caret)
library(rsample)
library(ROSE)
library(FSelector)
library(Boruta)
library(randomForest)
library(RWeka)
library(e1071)
library(ggplot2) 
library(pROC)
# Read the CSV file
data <- read.csv("C:/Users/Saurav N. Thakor/Downloads/preprocessed_data (1).csv")

# Now you can work with the 'data' object

setwd("C:/Users/Saurav N. Thakor/Downloads/")


###########################################################################################################################################

########################################## MAIN METHOD FOR ANALYSIS #######################################################################

########### STEP 1 - Split the dataset into the testing and training data sets################

# Read the CSV file "preprocessed_data.csv" into a data frame called 'dataset'
dataset <- read.csv("preprocessed_data.csv", header = TRUE)

# Convert the 'Class' column to numeric, where 'Y' is converted to 1 and other values to 0
dataset$Class <- as.numeric(dataset$Class == "Y")

# Set the seed for reproducibility
set.seed(123)

# Split the dataset into training and testing sets with 80% of the data in the training set
# and 20% in the testing set, stratified by the 'Class' variable
split <- initial_split(dataset, prop = 0.80, strata = "Class") 

# Extract the training set from the split
train <- training(split)

# Extract the testing set from the split
test <- testing(split)              
split <- initial_split(dataset, prop = 0.80, strata = "Class") 
# Create data_vals_train from the split
data_vals_train <- training(split)
data_vals_test <- testing(split)

################################ STEP 2 - Create the Balanced Datasets ###############################################

# 1st method - ROSE function for undersampling 
data_balanced_train_under <- ovun.sample(Class ~ ., data = train, method = "under")$data
table(data_balanced_train_under$Class)

# 2nd method - ROSE function for oversampling 
data_balanced_train_over <- ovun.sample(Class ~ ., data = train, method = "over")$data
table(data_balanced_train_over$Class)

################################# STEP 3 - Apply Attribute Selection on the Balanced Datasets #################################################

# 1st method - Remove co-linear features
# Calculate the correlation matrix for the features in the balanced training dataset (under-sampling)
corr_1 <- cor(data_balanced_train_under)

# Find highly correlated features based on a cutoff of 0.7
highCorr_1 <- findCorrelation(corr_1, cutoff = 0.7)

# Remove highly correlated features from the balanced training dataset (under-sampling)
reduced_balanced_train_under <- data_balanced_train_under[,-highCorr_1]

# Remove the same highly correlated features from the test dataset (under-sampling)
reduced_balanced_test_under <- test[,-highCorr_1]

# Calculate the correlation matrix for the features in the balanced training dataset (over-sampling)
corr_2 <- cor(data_balanced_train_over)

# Find highly correlated features based on a cutoff of 0.7
highCorr_2 <- findCorrelation(corr_2, cutoff = 0.7)

# Remove highly correlated features from the balanced training dataset (over-sampling)
reduced_balanced_train_over <- data_balanced_train_over[,-highCorr_2]

# Remove the same highly correlated features from the test dataset (over-sampling)
reduced_balanced_test_over <- test[,-highCorr_2]

