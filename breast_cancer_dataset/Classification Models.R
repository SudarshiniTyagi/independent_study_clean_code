# Read CSV into R
MyData <- read.csv(file=here("PycharmProjects/independentStudy/data", "breast_cancer_data.csv"), header=TRUE, sep=",")

cols_to_remove <- c("Unnamed: 32", "id", "X")

MyData <-  MyData[, ! names(MyData) %in% cols_to_remove, drop = F]

#Split into train and test
n <- nrow(MyData)
train_indices <- 1:round(0.7*n)
train_data <- MyData[train_indices, ]

test_indices <- (round(0.7*n)+1):n
test_data <- MyData[test_indices, ]

cat("___________________________________________________________________________\n")
#LOGISTIC REGRESSION
logit_model <- glm(diagnosis ~ . - diagnosis, data = train_data, family=binomial(link="logit"))

predictions_train <- predict(logit_model, train_data[,-1], type='response')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("LR Train Accuracy: ", accuracy, "\n")

predictions <- predict(logit_model, test_data[,-1], type='response')
conf_mat <- table(test_data$diagnosis,predictions)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("LR Test Accuracy: ", accuracy, "\n")

cat("___________________________________________________________________________\n")
#DECISION TREE
tree_clf <- rpart(diagnosis ~ . - diagnosis, data = train_data, method = 'class')

predictions_train <- predict(tree_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("DT Train Accuracy: ", accuracy, "\n")

predictions <- predict(tree_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("DT Test Accuracy: ", accuracy, "\n")

cat("___________________________________________________________________________\n")
#RANDOM FOREST
rf_clf <- randomForest(diagnosis ~ . - diagnosis, data = train_data, importance=TRUE,
                       proximity=TRUE)

predictions_train <- predict(rf_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("RF Train Accuracy: ", accuracy, "\n")

predictions <- predict(rf_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("RF Test Accuracy: ", accuracy, "\n")

cat("___________________________________________________________________________\n")

#SVM
rf_clf <- svm(diagnosis ~ . - diagnosis, data = train_data, 
              kernel = "linear", cost = 10, scale = FALSE)

predictions_train <- predict(rf_clf, train_data[,-1], type='class')
conf_mat <- table(train_data$diagnosis,predictions_train)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("SVM Train Accuracy: ", accuracy, "\n")

predictions <- predict(rf_clf, test_data[,-1], type='class')
conf_mat <- table(test_data$diagnosis,predictions)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
cat("SVM Test Accuracy: ", accuracy, "\n")

cat("___________________________________________________________________________\n")
