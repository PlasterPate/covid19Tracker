test_that("endDate before startDate raise error", {
  expect_error(plotCountryTimeline(c("2020-10-01", "2020-09-01"), "France"))
})

test_that("invalid country raise error", {
  expect_error(plotCountryTimeline(c("2020-05-01", "2020-09-01"), "Isfahan"))
})

test_that("invalid date range raise error", {
  expect_error(plotCountryTimeline(c("2019-01-01", "2019-02-01"), "Italy"))
})

test_that("invalid number of dates raise error", {
  expect_error(plotCountryTimeline(c("2020-01-01", "2020-02-01", "2020-03-01"), "China"))
})
