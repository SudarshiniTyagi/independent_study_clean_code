library(here)
library(rpart)


# Read CSV into R
MyData <- read.csv(file=here("PycharmProjects/independentStudy/data", "breast_cancer_data.csv"), header=TRUE, sep=",")
irrelevant_features <- c("id", "X")
MyData <- MyData[, ! names(MyData) %in% irrelevant_features, drop = F]

#Removing features not required for heatmap(i.e the labels in column diagnosis)
to_remove <- c("diagnosis")
X <- MyData[, ! names(MyData) %in% to_remove, drop = F]

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_heatmap <- function(my_data){
  cormat <- round(cor(my_data),2)
  lower_triangle <- get_lower_tri(cormat)
  library(reshape2)
  melted_cormat <- melt(lower_triangle, na.rm = TRUE)
  library(ggplot2)
  ggplot(data = melted_cormat, aes(Var1, Var2, fill=value)) +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1))+
    coord_fixed()
}


get_heatmap(X)

#REMOVING THE REDUNDANT FEATURES i.e. Features with high correlation, as seen from the heatmap

columns_to_remove <- c("perimeter_mean", "area_mean",
                       "perimeter_se", "area_se", "radius_worst", 
                       "texture_worst", 
                       "perimeter_worst", 
                       "area_worst", 
                       "smoothness_worst", 
                       "compactness_worst", 
                       "concavity_worst",
                       "concave.points_mean", 
                       "symmetry_worst", 
                       "fractal_dimension_worst",
                       "concavity_mean",
                       "concavity_se",
                       "concave.points_worst",
                       "concave.points_se")

MyData <-  MyData[, ! names(MyData) %in% columns_to_remove, drop = F]

# We now have 2 types of features: One with mean and one with SE(Standard error). 
# We use SVM as our classifier.
library(e1071) #Library to get SVM classifier

# Shuffling the dataset
set.seed(42)
rows <- sample(nrow(MyData))
MyData <- MyData[rows, ]



## ******USING ALL FEATURES*******
cat("\n******USING ALL FEATURES*******\n")
MyDataAllFeatures <- MyData

#Split into train and test
n <- nrow(MyDataAllFeatures)
train_indices <- 1:round(0.7*n)
train_data_af <- MyDataAllFeatures[train_indices, ]

test_indices <- (round(0.7*n)+1):n
test_data_af <- MyDataAllFeatures[test_indices, ]

#Build Classifier
svm_clf_af <- svm(diagnosis ~ . - diagnosis, data = train_data_af, 
               kernel = "linear", cost = 10, scale = FALSE)
#Predict on train
predictions_svm_af_train <- predict(svm_clf_af, train_data_af[,-1], type='class')
conf_mat_af_train <- table(train_data_af$diagnosis,predictions_svm_af_train)
accuracy_af_svm_train <- sum(diag(conf_mat_af_train))/sum(conf_mat_af_train)
cat("SVM Train Accuracy: ", accuracy_af_svm_train*100, "\n")
#Predict on Test
predictions_svm_af_test <- predict(svm_clf_af, test_data_af[,-1], type='class')
conf_mat_af_test <- table(test_data_af$diagnosis,predictions_svm_af_test)
accuracy_af_svm_test <- sum(diag(conf_mat_af_test))/sum(conf_mat_af_test)
cat("SVM Test Accuracy: ", accuracy_af_svm_test*100, "\n")



## ******USING MEAN FEATURES (Dropped SE features)******
cat("\n******USING MEAN FEATURES (Dropped SE features)******\n")
cols_to_drop_for_mean <- c('radius_se',
                'texture_se',
                'smoothness_se',
                'compactness_se',
                'symmetry_se',
                'fractal_dimension_se')
MyDataJustMean <- MyData[, ! names(MyData) %in% cols_to_drop_for_mean, drop = F]


#Split into train and test
n <- nrow(MyDataJustMean)
train_indices <- 1:round(0.7*n)
train_data_mean <- MyDataJustMean[train_indices, ]

test_indices <- (round(0.7*n)+1):n
test_data_mean <- MyDataJustMean[test_indices, ]

#Build Classifier
svm_clf_mean <- svm(diagnosis ~ . - diagnosis, data = train_data_mean, 
               kernel = "linear", cost = 10, scale = FALSE)
#Predict on train
predictions_svm_mean_train <- predict(svm_clf_mean, train_data_mean[,-1], type='class')
conf_mat_mean_train <- table(train_data_mean$diagnosis,predictions_svm_mean_train)
accuracy_mean_train <- sum(diag(conf_mat_mean_train))/sum(conf_mat_mean_train)
cat("SVM Train Accuracy: ", accuracy_mean_train*100, "\n")
#Predict on Test
predictions_svm_mean_test <- predict(svm_clf_mean, test_data_mean[,-1], type='class')
conf_mat_mean_test <- table(test_data_mean$diagnosis,predictions_svm_mean_test)
accuracy_mean_test <- sum(diag(conf_mat_mean_test))/sum(conf_mat_mean_test)
cat("SVM Test Accuracy: ", accuracy_mean_test*100, "\n")




## ******USING SE FEATURES (Dropped Mean Features)******
cat("\n******USING SE FEATURES (Dropped Mean Features)******\n")
cols_to_drop_for_se <- c('radius_mean',
                          'texture_mean',
                          'smoothness_mean',
                          'compactness_mean',
                          'symmetry_mean',
                          'fractal_dimension_mean')
MyDataJustSE <- MyData[, ! names(MyData) %in% cols_to_drop_for_se, drop = F]


#Split into train and test
n <- nrow(MyDataJustSE)
train_indices <- 1:round(0.7*n)
train_data_se <- MyDataJustSE[train_indices, ]

test_indices <- (round(0.7*n)+1):n
test_data_se <- MyDataJustSE[test_indices, ]

#Build Classifier
svm_clf_se <- svm(diagnosis ~ . - diagnosis, data = train_data_se, 
               kernel = "linear", cost = 10, scale = FALSE)
#Predict on train
predictions_svm_se_train <- predict(svm_clf_se, train_data_se[,-1], type='class')
conf_mat_se_train <- table(train_data_se$diagnosis,predictions_svm_se_train)
accuracy_se_train <- sum(diag(conf_mat_se_train))/sum(conf_mat_se_train)
cat("SVM Train Accuracy: ", accuracy_se_train*100, "\n")
#Predict on Test
predictions_svm_se_test <- predict(svm_clf_se, test_data_se[,-1], type='class')
conf_mat_se_test <- table(test_data_se$diagnosis,predictions_svm_se_test)
accuracy_se_test <- sum(diag(conf_mat_se_test))/sum(conf_mat_se_test)
cat("SVM Test Accuracy: ", accuracy_se_test*100, "\n")

