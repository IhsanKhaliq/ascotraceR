context(
  "function takes a source coordinate, a distance vector and angle, then returns the destination coordinate"
)

test1 <- address_from_centre_distance(offset_distance = c(3, 6),
                                      start_address = c(50, 50))

test_that("test1 returns correct coordinates", {
  expect_equal(test1, c(53, 56))
  expect_length(test1, 2)
  expect_true(any(is.na(test1)) == FALSE)
  expect_type(test1, "integer")
})


test2 <- address_from_centre_distance(offset_distance = c(-13, 20),
                                      start_address = c(50, 50))

test_that("test1 returns correct coordinates with negatives", {
  expect_equal(test2, c(37, 70))
  expect_length(test2, 2)
  expect_true(any(is.na(test2)) == FALSE)
  expect_type(test2, "integer")
})

test3 <-
  address_from_centre_distance(offset_distance = c(-45.93146, -4.90950),
                               start_address = c(50, 50))

test_that("test1 returns correct coordinates with negatives and floating points",
          {
            expect_equal(test3, c(round(50 - 45.93146), round(50 - 4.90950)))
            expect_length(test3, 2)
            expect_true(any(is.na(test3)) == FALSE)
            expect_type(test3, "integer")
          })
