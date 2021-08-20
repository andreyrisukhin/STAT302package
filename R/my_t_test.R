#' T-test
#'
#' This function calculates P-value using the t-distribution hypothesis test.
#'
#' @param x The numeric vector of data given to conduct the t-test. No default
#'   value.
#' @param alternative The alternative hypothesis, a string of either "two.sided",
#'   "less", or "greater". Used to determine which t-test to conduct. No default
#'   value.
#' @param mu The predicted mean, a numeric value to calculate the test statistic.
#'   No default value.
#'
#' @keywords test hypothesis inference
#'
#' @return A list of the t-value, the degrees of freedom, the alternative used,
#'   and the P-value.
#'
#' @examples
#' my_t.test(c(1, 2, 2), alternative = "less", 3)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # Check that the input parameters are valid
  if(!is.numeric(x)) {
    # Return error for non-numeric input
    stop("x must be numeric!")
  }
  if(!(identical(alternative, "two.sided") || identical(alternative, "less") || identical(alternative, "greater"))) {
    # Return error for appropriate alternative input
    stop("alternative must be either \"two.sided\", \"less\", or \"greater\".")
  }
  if(!is.numeric(mu)) {
    stop("mu must be a number! This indicates the null hypothesized value of the mean.")
  }

  # Compute standard error
  se <- stats::sd(x) / sqrt(length(x))
  # Assign the test statistic, using standard statistic
  test_stat <- (mean(x) - mu) / se
  # Assign the degrees of freedom
  df <- length(x) - 1

  # Compute p-value (area with pt)
  if(identical(alternative, "less")) {
    # No matter the test_stat sign, use P(upper tail) = 1 - P(lower tail)
    p_val <- stats::pt(test_stat, df, lower.tail = TRUE)
  }
  if(identical(alternative, "greater")) {
    # No matter the test_stat sign, use P(upper tail) = 1 - P(lower tail)
    p_val <- 1 - stats::pt(test_stat, df, lower.tail = TRUE)

    #p_val <- pt(test_stat_pos, df, lower.tail = TRUE)
    # Use fact that sum of p_vals for less and for greater = 1
    #p_val <- 1 - p_val
  }
  if(identical(alternative, "two.sided")) {
    # Take absolute value of test statistic
    test_stat_pos <- abs(test_stat)
    # Take upper tail of absolute value
    p_val_half <- stats::pt(test_stat_pos, df, lower.tail = FALSE)
    # Multiply by two (symmetric tails)
    p_val <- p_val_half * 2
  }

  # Return a list with test_stat, df, alternative, and p_val
  return(list("test_stat" = test_stat,
              "df" = df,
              "alternative" = alternative,
              "p_val" = p_val))

}
