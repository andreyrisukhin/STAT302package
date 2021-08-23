# Import penguins with STAT302package::my_penguins

#' my_rf_cv
#'
#' Description
#' This function uses \code{bill_length_mm},
#'   \code{bill_depth_mm}, and \code{flipper_length_mm} to predict
#'   \code{body_mass_g} from the data \code{my_penguins} with k-fold Cross-validation
#'   & random forest algorithms.
#'
#' @param k number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error;
#'
#' @examples
#' my_rf_cv(k = 5)
#' my_rf_cv(k = 10)
#'
#' @import randomForest stats
#'
#' @export

my_rf_cv <- function(k) {

  library(randomForest)
  set.seed(123)

  # Randomly assign observations to fold 1 to k_cv
  mse <- c()
  predictions <- c()

  # Remove NAs from penguins data
  penguins_clean<- na.omit(palmerpenguins::penguins)

  # Create data-set with selected columns
  penguin_data <-  penguins_clean[, c("bill_length_mm",
                                      "bill_depth_mm",
                                      "flipper_length_mm",
                                      "body_mass_g")]


  fold <- sample(rep(1:k, length = nrow(penguin_data)))

  # Iterate through the fold
  for (i in 1:k) {
    # Split training data by fold
    train_fold <- penguin_data[fold != i, ]

    # Split testing data by fold
    test_fold <- penguin_data[fold == i, ]

    # Train rf, 100 trees
    model100 <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                               flipper_length_mm, data = train_fold, ntree = 100)

    # Predict body_mass_g for each fold with rf, 100 trees
    predictions <- predict(model100, test_fold[, -4]) # To get rid of body mass g

    # Evaluate the MSE, the average squared difference between predicted body_mass_g and true body_mass_g.
    mse[i] <- (sum(predictions - test_fold[, 4])^2) / length(predictions)
  }

  # Add the predicted class and cross validation errors to a list
  output <- list("cv_error" = mean(mse))
  return(output)
}





# my_rf_cv: A custom random forest prediction function that also returns cross validation error.
# Input: integer number of folds.
# Output: numeric vector of classification error.
my_rf_cv <- function(k) {

  # Do not have library() in a function
  #library(randomForest)

  # Randomly assign observations to folds 1 to k_cv, with equal probability
  mse <- c()
  predictions <- c()

  # Create dataset with columns we want
  penguin_data <- STAT302package::my_penguins[, c("bill_length_mm",
                               "bill_depth_mm",
                               "flipper_length_mm",
                               "body_mass_g")]

  # Remove NAs from penguins data
  penguin_data <- stats::na.omit(penguin_data)

  folds <- sample(rep(1:k, length = nrow(penguin_data)))

  # Iterate through the folds
  for (i in 1:k) {
    # Split training data by folds
    train_fold <- penguin_data[folds != i, ]
    # Split testing data by folds
    test_fold <- penguin_data[folds == i, ]
    # Train rf, 100 trees
    rf100_i <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                              flipper_length_mm, data = train_fold, ntree = 100)

    # Predict body_mass_g for each fold with rf, 100 trees
    predictions <- stats::predict(rf100_i, test_fold[, -4]) # To get rid of body mass g

    # Evaluate the MSE, the average squared difference between predicted body_mass_g and true body_mass_g.
    mse[i] <- (sum(predictions - test_fold[, 4])^2) / length(predictions) # Compare predictions to test data, not to the full data
  }

  # Add the predicted class and cross validation errors to a list
  output <- list("cv_err" = mean(mse))
  return(output)
}
