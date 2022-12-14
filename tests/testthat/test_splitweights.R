## Tests for split select weights

library(rangerts)
context("rangerts_splitweights")

## Tests
test_that("split select weights work", {
  expect_silent(rangerts(Species ~ ., iris, num.trees = 5, split.select.weights = c(0.1, 0.2, 0.3, 0.4)))
  expect_error(rangerts(Species ~ ., iris, num.trees = 5, split.select.weights = c(0.1, 0.2, 0.3)))
})

test_that("Tree-wise split select weights work", {
  num.trees <- 5
  weights <- replicate(num.trees, runif(ncol(iris)-1), simplify = FALSE)
  expect_silent(rangerts(Species ~ ., iris, num.trees = num.trees, split.select.weights = weights))

  weights <- replicate(num.trees+1, runif(ncol(iris)-1), simplify = FALSE)
  expect_error(rangerts(Species ~ ., iris, num.trees = num.trees, split.select.weights = weights))
})

test_that("always split variables work", {
  expect_silent(rangerts(Species ~ ., iris, num.trees = 10,
                       always.split.variables = c("Petal.Length", "Petal.Width"), mtry = 2))
  expect_silent(rangerts(dependent.variable.name = "Species", data = iris, num.trees = 10,
                       always.split.variables = c("Petal.Length", "Petal.Width"), mtry = 2))
})

test_that("Tree-wise split select weights work with 0s", {
  num.trees <- 5
  weights <- replicate(num.trees, sample(c(0, 0, 0.5, 0.5)), simplify = FALSE)
  rf <- rangerts(Species ~ ., iris, mtry = 2, num.trees = num.trees,
               split.select.weights = weights)
  selected_correctly <- sapply(1:num.trees, function(i) {
    all(treeInfo(rf, i)[,"splitvarID"] %in% c(which(weights[[i]] > 0) - 1, NA))
  })
  expect_true(all(selected_correctly))
})
