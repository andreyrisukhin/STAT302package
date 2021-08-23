#' my_knn_cv
#'
#' This function performs a k-Nearest Neighbors Cross-Validation in R
#' @param train Input data frame to be validated
#' @param cl true class value of the training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords K-Nearest Neighbors Cross-Validation Function
#'
#' @return a list with objects \code{output} .
#'
#' @examples
#' my_penguins<- na.omit(palmerpenguins::penguins) # bring the data in.
#'  my_knn_cv(train = my_penguins[, 3:6]
#'                         , cl = my_penguins$species,  k_nn = 5, k_cv = 5)
#'
#' @export

# my_knn_cv: a custom function to predict output class using covariate
#my_knn_cv <- function(train, cl, k_nn, k_cv) {

#  set.seed(123) # make sure the sampling can be repeat within a same parameter
#  fold <- sample(rep(1 : k_cv, length = nrow(train)))
#  errors <- c()

  # split the data
#  for (i in 1 : k_cv) {

#    train_df <- train[fold != i, ]
#    validation_df  <-  train[fold == i, ]
#    cl_train <- cl[fold != i]
#    cl_validation <-  cl[fold == i]

    # Make a prediction for this data-set
#    predictions <- class::knn(train = train_df, cl = cl_train, test  = validation_df , k = k_nn)

    # Assigned mis-classifications
#    errors[i] <- sum(predictions != cl_validation) / length(cl_validation)
#  }

#  knnClass <- class::knn(train = train, cl = cl, test  = train, k = k_nn)
#  cv_error <- mean(errors)

  # store output as a list
#  output <- list("class" = knnClass, "cv_err" = cv_error)

  # return the output
#  return(output)
#}

my_knn_cv <- function(train, cl, k_nn, k_cv) {

  set.seed(123) # make sure the sampling can be repeat within a same parameter
  fold <- sample(rep(1 : k_cv, length = nrow(train)))
  errors <- c()

  # split the data
  for (i in 1 : k_cv) {

    train_df <- train[fold != i, ]
    validation_df  <-  train[fold == i, ]
    cl_train <- cl[fold != i]
    cl_validation <-  cl[fold == i]

    # Make a prediction for this data-set
    predictions <- class::knn(train = train_df, cl = cl_train, test  = validation_df , k = k_nn)

    # Assigned mis-classifications
    errors[i] <- sum(predictions != cl_validation) / length(cl_validation)
  }

  knnClass <- class::knn(train = train, cl = cl, test  = train, k = k_nn)
  cv_error <- mean(errors)

  # store output as a list
  output <- list("class" = knnClass, "cv_err" = cv_error)

  # return the output
  return(output)
}

#library(class)

# my_knn_cv: a custom function to return knn predictions and evaluate its quality through cross validation.
# Input: the dataframe, true class of data, integer number of neighbors, integer number of folds.
# Output: a list of the predicted class for all observations and the cv classification error.
#my_knn_cv <- function(train, cl, k_nn, k_cv) {

#  folds <- sample(rep(1 : k_cv, length = nrow(train)))
#  misclass <- c()

#  for (i in 1 : k_cv) {
    # Split data by folds
#    train_trainfold <- train[folds != i, ]
#    test_valfold  <-  train[folds == i, ]
#    cl_trainfold <- cl[folds != i]
#    cl_valfold <-  cl[folds == i]
    # Predict the classes for this fold
#    predictions <- class::knn(train = train_trainfold, cl = cl_trainfold, test  = test_valfold , k = k_nn)
    # Store the errors for each fold
#    misclass[i] <- sum(predictions != cl_valfold) / length(cl_valfold)
#  }
  # Minimal loading; I have referenced loading class, now I invoke this
#  my_class <- class::knn(train = train, cl = cl, test  = train, k = k_nn)
#  cv_misclass <- mean(misclass) # Cross validation estimates the error, does not "improve" the final model
#  output <- list("class" = my_class, "cv_misclass" = cv_misclass)
#  return(output)
#}
