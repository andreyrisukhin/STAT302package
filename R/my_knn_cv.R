library(class)

# my_knn_cv: a custom function to return knn predictions and evaluate its quality through cross validation.
# Input: the dataframe, true class of data, integer number of neighbors, integer number of folds.
# Output: a list of the predicted class for all observations and the cv classification error.
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  folds <- sample(rep(1 : k_cv, length = nrow(train)))
  misclass <- c()

  for (i in 1 : k_cv) {
    # Split data by folds
    train_trainfold <- train[folds != i, ]
    test_valfold  <-  train[folds == i, ]
    cl_trainfold <- cl[folds != i]
    cl_valfold <-  cl[folds == i]
    # Predict the classes for this fold
    predictions <- class::knn(train = train_trainfold, cl = cl_trainfold, test  = test_valfold , k = k_nn)
    # Store the errors for each fold
    misclass[i] <- sum(predictions != cl_valfold) / length(cl_valfold)
  }
  # Minimal loading; I have referenced loading class, now I invoke this
  my_class <- class::knn(train = train, cl = cl, test  = train, k = k_nn)
  cv_misclass <- mean(misclass) # Cross validation estimates the error, does not "improve" the final model
  output <- list("class" = my_class, "cv_misclass" = cv_misclass)
  return(output)
}
