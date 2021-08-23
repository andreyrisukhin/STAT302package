#data("my_penguins")

STAT302package::my_penguins

test_that("my_knn_cv works mathematically", {
  penguins_data <- na.omit(my_penguins)
  train <- penguins_data[, 3:6]
  cl <- penguins_data$species
  knn_vadidate <- my_knn_cv(train, cl, 5, 5)

  expect_type(knn_vadidate, "list")
  expect_type(knn_vadidate[[1]], "integer")
})

test_that("k_nn parameter is less than 1 or non-numeric then shows error", {
  expect_error(my_knn_cv(train, cl, 0, 5))
  expect_error(my_knn_cv(train, cl, "a string", 5))
})

test_that("k_nn parameter is less than 1 or non-numeric then shows error", {
  expect_error(my_knn_cv(train, cl, 5, 1))
  expect_error(my_knn_cv(train, cl, 5, "a string"))
})

#test_that("my_knn_cv works properly", {
#  expect_type(my_knn_cv(train = na.omit(my_penguins[, 3:6]), cl =  na.omit(my_penguins[, 1]), k_nn = 5, k_cv = 5), "list")
  #expect_type(my_knn_cv(train = na.omit(my_penguins[, 3:6]), cl =  na.omit(my_penguins[, 1]), k_nn = 1, k_cv = 5), "list")
#})
#test_that("my_knn_cv breaks", {
#  expect_error(my_knn_cv(train = na.omit(my_penguins[, 8]), k_nn = 5, k_cv = 5))
#  expect_error(my_knn_cv(na.omit(my_penguins)))
#})
