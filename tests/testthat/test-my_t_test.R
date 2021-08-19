
testing_x <- c(1,2,2)
testing_mu <- 2

# Test that my_t_test errors when improper values passed in
test_that("Passing in non-numeric x results in an error", {
  expect_error(my_t.test(x = "a", alternative = "two.sided", mu = testing_mu))
})
test_that("Passing in non-numeric mu results in an error", {
  expect_error(my_t.test(x = testing_x, alternative = "two.sided", mu = "a"))
})
test_that("Passing in non-legal alternative results in an error", {
  expect_error(my_t.test(x = testing_x, alternative = "two.sidedd", mu = testing_mu))
})

# Test that expected output matches for each alternative
test_that("my_t.test() and t.test() return identical P-values for the same inputs", {
  expect_equal(as.numeric(my_t.test(x = testing_x, alternative = "greater", mu = testing_mu)$p_val),
               as.numeric(t.test(x = testing_x, alternative = "greater", mu = testing_mu)$p.value))
  expect_equal(as.numeric(my_t.test(x = testing_x, alternative = "less", mu = testing_mu)$p_val),
               as.numeric(t.test(x = testing_x, alternative = "less", mu = testing_mu)$p.value))
  expect_equal(as.numeric(my_t.test(x = testing_x, alternative = "two.sided", mu = testing_mu)$p_val),
               as.numeric(t.test(x = testing_x, alternative = "two.sided", mu = testing_mu)$p.value))
})
