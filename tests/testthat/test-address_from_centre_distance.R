test1 <- address_from_centre_distance(offset_distance = c(3, 6),
                                      start_address = c(50, 50))

test_that("test1 returns correct coordinates", {
  expect_equal(test1, data.table(x = 53,
                                 y = 56))
  expect_length(test1, 2)
  expect_true(any(is.na(test1)) == FALSE)
  expect_type(test1[, x], "integer")
  expect_type(test1[, y], "integer")
  expect_s3_class(test1, "data.table")
})


test2 <- address_from_centre_distance(offset_distance = c(-13, 20),
                                      start_address = c(50, 50))

test_that("test4 returns correct coordinates with negatives", {
  expect_equal(test2, data.table(x = 37,
                                 y = 70))
  expect_length(test2, 2)
  expect_true(any(is.na(test2)) == FALSE)
  expect_type(test2[, x], "integer")
  expect_type(test2[, y], "integer")
  expect_s3_class(test2, "data.table")
})

test3 <-
  address_from_centre_distance(offset_distance = c(-45.93146, -4.90950),
                               start_address = c(50, 50))
xy <- data.table(x = round(50 - 45.93146),
                 y = round(50 - 4.90950))

test_that("test3 returns correct coordinates with negatives and floating
          points",
          {
            expect_equal(test3, xy)
            expect_length(test3, 2)
            expect_true(any(is.na(test3)) == FALSE)
            expect_type(test3[, x], "integer")
            expect_type(test3[, y], "integer")
            expect_s3_class(test3, "data.table")
          })

test_that("test4 returns an error", {
  expect_error(address_from_centre_distance(
    offset_distance = c(NA, 5),
    start_address = c(50, 50)
  ))
})
