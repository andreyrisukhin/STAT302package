test_that("my_rf_cv returns a double as output mathematically", {
  expect_type(my_rf_cv(5), "double")
})

test_that("my_rf_cv throws an error if input is non-numeric", {
  expect_error(my_rf_cv("c"))
})
