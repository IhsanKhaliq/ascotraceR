summarised_test <- summarise_trace(
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

test_that("summarise_trace() produces expected output", {
  expect_s3_class(summarised_test, c("data.table", "data.frame"))
  expect_equal(length(summarised_test), 12)
  expect_equal(nrow(summarised_test), 5)
  expect_named(
    summarised_test,
    c(
      "i_day",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp",
      "i_date",
      "day",
      "cdd",
      "cwh",
      "cr",
      "gp_standard",
      "AUDPC"
    )
  )
})
