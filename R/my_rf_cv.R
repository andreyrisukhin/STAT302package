# Import penguins with STAT302package::my_penguins

# Part 2

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
