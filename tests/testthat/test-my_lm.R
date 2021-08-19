# Test
# Check the output of the function for multiple inputs

# Set seed, and create test data, then use that inside my tests
#set.seed(123)
#testing_data <-


test_that("my_lm() and lm() return identical values for the same inputs", {
  expect_equal(as.numeric(my_lm(mpg ~ hp + wt, data = mtcars, pp = 5)[1, 1]),
               as.numeric(lm(mpg ~ hp + wt, data = mtcars)$coefficients[1]))
})


