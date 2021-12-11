tidy_test <- tidy_trace(
  trace_asco(
    weather = newM_weather,
    paddock_length = 100,
    paddock_width = 100,
    initial_infection = "1998-05-10",
    sowing_date = as.POSIXct("1998-05-09"),
    harvest_date = as.POSIXct("1998-05-12"),
    time_zone = "Australia/Perth",
    primary_infection_foci = "centre"
  )
)

test_that("tidy_trace() produces expected output", {
  expect_s3_class(tidy_test, c("data.table", "data.frame"))
  expect_equal(length(tidy_test), 14)
  expect_equal(nrow(tidy_test), 50000)
  expect_named(
    tidy_test,
    c(
      "i_day",
      "i_date",
      "day",
      "x",
      "y",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp",
      "stubble_lesions",
      "cdd",
      "cwh",
      "cr",
      "gp_standard"
    )
  )
})
