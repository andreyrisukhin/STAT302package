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
#' @export
#'
my_rf_cv <- function(k) {

  if(!is.numeric(k)) {
    # Return error for non-numeric input
    stop("k must be numeric!")
  }

  #penguins_clean<- na.omit(palmerpenguins::penguins)
  my_penguins <- na.omit(STAT302package::my_penguins)

  data_lenght <- nrow(my_penguins)
  # split data in k parts, randomly
  fold <- sample(rep(1:k, length = data_lenght))
  data_input <- my_penguins %>% dplyr::select("bill_length_mm","bill_depth_mm", "flipper_length_mm",
                                       "body_mass_g")
  data_input[, "split"] <- fold

  mse <- c()
  for (i in 1:k) {
    # define the training data as all the data not in the i fold
    train_df <- data_input %>% dplyr::filter(split != i)
    test_df  <- data_input %>% dplyr::filter(split == i)

    # train the model with 50 trees
    my_model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                               flipper_length_mm, data = train_df, ntree = 50)

    # predict the body_mass_g
    my_pred <- predict(my_model, dplyr::select(test_df, -split))


    # evaluate the MSE, the average squared difference between predicted body_mass vs the true body mass
    mse <- c(mse, mean((my_pred - test_df$body_mass_g)^2))
  }
  output <- mean(mse)
  type <- typeof(output)
  return(output)
}
