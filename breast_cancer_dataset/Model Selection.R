library(gsubfn)

shuffle_data <- function(grp1, grp2){
  l1 <- length(grp1)
  l2 <- length(grp2)
  pool <- c(grp1, grp2)
  
  # mix them up
  pool <- sample(pool)
  new_grp1 <- head(pool, l1)
  new_grp2 <- tail(pool, l2)
  
  return(list(new_grp1, new_grp2))
}


# subtracts group A mean from group B mean and returns result
meandiff <- function(grpA, grpB){
  return ((mean(grpA) - mean(grpB))*100 )
}

p_test <- function(sample_0, sample_1){

  observed_mean_diff <- meandiff(sample_0, sample_1)
  
  count <- 0
  num_shuffles = 10000
  
  for (i in c(1: num_shuffles)){
    list[new_data_0, new_data_1] <- shuffle_data(sample_0, sample_1)
    mean_diff <- meandiff(new_data_0, new_data_1)
    # if the observed difference is negative, look for differences that are smaller
    # if the observed difference is positive, look for differences that are greater
    if (observed_mean_diff < 0 && mean_diff <= observed_mean_diff){
      count <- count + 1
    }
    else if(observed_mean_diff >= 0 && mean_diff >= observed_mean_diff){
      count <- count + 1
    }
  }
  
  ######################################
  #
  # Output
  #
  ######################################
  
  cat ("\nObserved difference of two means: ",observed_mean_diff)
  
  if(observed_mean_diff < 0){
    cat ("\n", count, "out of", num_shuffles, "experiments had a difference of two means less than or equal to ", observed_mean_diff)
  } else{
    cat("\n", count, "out of", num_shuffles, "experiments had a difference of two means greater than or equal to ", observed_mean_diff)
  }
  
  
  if(observed_mean_diff < 0){
    cat ("\nThe chance of getting a difference of two means less than or equal to ", observed_mean_diff, "is", (count /
                                                                                                                num_shuffles), ".")
  }else{
    cat ("\nThe chance of getting a difference of two means greater than or equal to ", observed_mean_diff, "is", (count /
                                                                                                                   num_shuffles), ".")
  }
}


MyData <- read.csv(file=here("PycharmProjects/independentStudy/data", "breast_cancer_data.csv"), header=TRUE, sep=",")

columns_to_remove <- c("Unnamed: 32","id", "X", "perimeter_mean", "area_mean",
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
                       "concave.points_se",
                       "radius_se",
                       "texture_se",
                       "smoothness_se",
                       "compactness_se",
                       "symmetry_se",
                       "fractal_dimension_se")
MyData <-  MyData[, ! names(MyData) %in% columns_to_remove, drop = F]
MyData$diagnosis <- ifelse(MyData$diagnosis == "M", 1, 0)
MyData$diagnosis <- factor(MyData$diagnosis, levels = c(0,1))

#Split into train and test
n <- nrow(MyData)
train_indices <- 1:round(0.7*n)
train_data <- MyData[train_indices, ]

test_indices <- (round(0.7*n)+1):n
test_data <- MyData[test_indices, ]

cat("___________________________________________________________________________\n")
#LOGISTIC REGRESSION
logit_model <- glm(diagnosis ~ . - diagnosis, data = train_data, family=binomial(link="logit"))

predictions_train_lr <- predict(logit_model, train_data[,-1], type='response')
y_pred_num_train <- ifelse(predictions_train_lr > 0.5, 1, 0)
y_pred_train <- factor(y_pred_num_train, levels=c(0, 1))
y_act_train <- train_data$diagnosis

conf_mat <- table(y_act_train,y_pred_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("LR Train Accuracy: ", accuracy*100, "\n")

predictions_lr <- predict(logit_model, test_data[,-1], type='response')
y_pred_num_test <- ifelse(predictions_lr > 0.5, 1, 0)
y_pred_test <- factor(y_pred_num_test, levels=c(0, 1))
y_act_test <- test_data$diagnosis
conf_mat <- table(y_act_test,y_pred_test)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("LR Test Accuracy: ", accuracy*100, "\n")

cat("___________________________________________________________________________\n")
#DECISION TREE
tree_clf <- rpart(diagnosis ~ . - diagnosis, data = train_data, method = 'class')

predictions_train <- predict(tree_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("DT Train Accuracy: ", accuracy*100, "\n")

predictions_dt <- predict(tree_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions_dt)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("DT Test Accuracy: ", accuracy*100, "\n")

cat("___________________________________________________________________________\n")
#RANDOM FOREST
library(randomForest)
rf_clf <- randomForest(diagnosis ~ . - diagnosis, data = train_data, importance=TRUE,
                       proximity=TRUE)

predictions_train <- predict(rf_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("RF Train Accuracy: ", accuracy*100, "\n")

predictions_rf <- predict(rf_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions_rf)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("RF Test Accuracy: ", accuracy*100, "\n")

cat("___________________________________________________________________________\n")

#SVM
library(e1071)
svm_clf <- svm(diagnosis ~ . - diagnosis, data = train_data, 
              kernel = "linear", cost = 10, scale = FALSE)

predictions_train <- predict(svm_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("SVM Train Accuracy: ", accuracy*100, "\n")

predictions_svm <- predict(svm_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions_svm)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("SVM Test Accuracy: ", accuracy*100, "\n")

cat("___________________________________________________________________________\n")

cat("\np_test: Logistic Regression vs Decision Trees")
sample_0 <- y_act_test == y_pred_test
sample_1 <- test_data$diagnosis == predictions_dt
p_test(sample_0, sample_1)

cat ("\n\n\np_test: Random Forest vs SVM")
sample_0 <- test_data$diagnosis == predictions_rf
sample_1 <- test_data$diagnosis == predictions_svm
p_test(sample_0, sample_1)

cat("\n\n\np_test: Logistic Regression vs Random Forest")
sample_0 <- test_data$diagnosis == y_pred_test
sample_1 <- test_data$diagnosis == predictions_rf
p_test(sample_0, sample_1)

cat("\n\n\np_test: Logistic Regression vs SVM")
sample_0 <- test_data$diagnosis == y_pred_test
sample_1 <- test_data$diagnosis == predictions_svm
p_test(sample_0, sample_1)

cat("\n\n\np_test: Decision Trees vs SVM")
sample_0 <- test_data$diagnosis == predictions_dt
sample_1 <- test_data$diagnosis == predictions_svm
p_test(sample_0, sample_1)

cat("\n\n\np_test: Decision Trees vs Random Forests")
sample_0 <- test_data$diagnosis == predictions_dt
sample_1 <- test_data$diagnosis == predictions_rf
p_test(sample_0, sample_1)

cat("\n___________________________________________________________________________\n")
