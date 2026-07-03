pdk <- CJ(x = 1:100, y = 1:100)
qry <- pdk[sample(seq_len(nrow(pdk)), 5), ]
which_paddock_row(pdk, qry)

test1 <- which_paddock_row(pdk, qry)

test_that("test1 gives correct output", {
  expect_false(anyNA(test1))
  expect_length(test1, 5)
  expect_true(is.vector(test1))
  expect_gt(max(test1), 125)
  expect_lt(min(test1), 4797)
  expect_is(test1, "integer")
})
