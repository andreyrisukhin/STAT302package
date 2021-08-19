# Test
# Check the output of the function for multiple inputs

# Set seed, and create test data, then use that inside my tests
#set.seed(123)
#testing_data <-

test_that("Passing in non-dataframe data results in an error", {
  expect_error(my_lm(mpg ~ hp + wt, data = "data"))
  expect_error(my_lm(mpg ~ hp + wt, data = 7))
  expect_error(my_lm(mpg ~ hp + wt, data = NA))
})

test_that("my_lm() and lm() return identical values for the same inputs", {
  expect_equal(as.numeric(my_lm(mpg ~ hp + wt, data = mtcars)[1, 1]),
               as.numeric(lm(mpg ~ hp + wt, data = mtcars)$coefficients[1]))
})


