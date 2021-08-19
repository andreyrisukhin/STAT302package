# my_lm: calculates estimate, standard error, t and P values for each coefficient
# given a formula and data.
# Input: a formula object, a data vector.
# Output: a table.
# This function works with the ~. notation
my_lm <- function(formula, data) {

  # Ensure that data is passed as a dataframe
  if(!is.data.frame(data)) {
    # Return error for non-dataframe input
    stop("data must be a data.frame!")
  }

  # Use model.matrix() to get model matrix X. It takes as input a formula and data.
  X <- stats::model.matrix(formula, data)
  # Use model.frame() to extract
  mf <- stats::model.frame(formula, data)
  # Use model.response() to get model response Y. It takes as input a model frame.
  Y <- stats::model.response(mf)
  # Solve for linear regression coefficients with formula B_hat = (Xt X)^-1 Xt Y
  # My MLE
  B_hat <- solve((t(X) %*% X)) %*% t(X) %*% Y
  # Calculate degrees of freedom: sample size - number of covariates (including intercept)
  # Always one row for observation, one col for each covariate, intercept
  df <- nrow(X) - ncol(X)
  # Estimate sigma squared using Sum of (Yi - XiB)^2 / df
  sigma_sq <- sum((Y - (X %*% B_hat))^2 / df)
  # Estimate standard error
  # Be careful to use the diagonal elements!
  # Use sqrt(diag()) at some point, not the other way around
  se <- sqrt(diag(sigma_sq * solve((t(X) %*% X))))
  # Compute the test statistic
  # I am testing that the parameter is actually 0
  # Always looks like (theta_hat - theta_0) / se
  test_stat <- (B_hat - 0) / se
  # Use pt() to get the area under the curve for a t-distribution (symmetric, multiply by 2)
  area <- 2 * stats::pt(abs(test_stat), df, lower.tail = FALSE)
  # Return a table similar to coefficient table from summary()
  # cbind the four things, as.table
  # Using model.<thing>, the rownames will automatically be there
  # Need to customize column names
  columns <- cbind(B_hat, se, test_stat, area)
  output <- as.table(columns)
  # colnames after as.table, because cbind is encoded as a numeric
  colnames(output) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # Return the calculated output
  return(output)
}
