test_that("endDate before startDate raise error", {
  expect_error(plotWorldMap(c("2020-10-01", "2020-09-01"), "Death"))
})

test_that("invalid type raise error", {
  expect_error(plotWorldMap(c("2020-05-01", "2020-09-01"), "Recovered"))
})

test_that("invalid date range raise error", {
  expect_error(plotWorldMap(c("2019-01-01", "2019-02-01"), "Confirmed"))
})

test_that("invalid number of dates raise error", {
  expect_error(plotWorldMap(c("2020-01-01", "2020-02-01", "2020-03-01"), "Confirmed"))
})
