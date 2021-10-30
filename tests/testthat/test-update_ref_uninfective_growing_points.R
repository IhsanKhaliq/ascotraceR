test_1 <- (5000*0.0065* 15000*20*350)

test_that("correct numbers and class are returned", {
  expect_equal(test_1, 3412500000)
  expect_length(test_1,1)
  expect_is(as.numeric(test_1), "numeric")
})


