
dataset = fetchData()

test_that("Data is a dataframe", {
  expect_true(is.data.frame(dataset))
})


test_that("Data is a dataframe", {
  expect_equal(length(dataset), 8)
})
