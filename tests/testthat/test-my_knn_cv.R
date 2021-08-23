data("my_penguins")

test_that("my_knn_cv works properly", {
  expect_type(my_knn_cv(train = na.omit(my_penguins[, 3:6]), cl =  na.omit(my_penguins[, 1]), k_nn = 5, k_cv = 5), "list")
  #expect_type(my_knn_cv(train = na.omit(my_penguins[, 3:6]), cl =  na.omit(my_penguins[, 1]), k_nn = 1, k_cv = 5), "list")
})
test_that("my_knn_cv breaks", {
  expect_error(my_knn_cv(train = na.omit(my_penguins[, 8]), k_nn = 5, k_cv = 5))
  expect_error(my_knn_cv(na.omit(my_penguins)))
})
