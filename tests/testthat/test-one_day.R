context("Simulate disease progression for a single day increment")


od_t1 <- one_day(day = as.POSIXct("2020-12-18", tz = "UTC"),
                 )

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
