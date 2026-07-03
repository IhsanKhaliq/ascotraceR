# `which_paddock_row()` itself is deterministic -- it only depends on the
# `paddock` and `query` inputs, not on any random draws. The randomness in
# the *old* version of this test came entirely from using `sample()` to
# choose which rows to query, with no `set.seed()`, so the exact row numbers
# checked (and therefore what the test actually verified) changed on every
# run. That was combined with loose `expect_gt()`/`expect_lt()` bounds rather
# than exact expected values, so the test could pass even if the row-number
# formula were wrong.
#
# Fixed here by: (1) seeding `sample()` so the queried rows are reproducible,
# and (2) checking correctness via a round-trip against `paddock` itself --
# i.e. the coordinates at the row numbers `which_paddock_row()` returns must
# be exactly the coordinates that were queried. This is stronger than
# checking loose numeric bounds, and it works without needing to assume or
# hard-code how `data.table::CJ()` orders its output internally.

pdk <- CJ(x = 1:100, y = 1:100)

test_that("which_paddock_row identifies the correct paddock rows", {
  set.seed(123)
  qry <- pdk[sample(1:nrow(pdk), 5), ]
  test1 <- which_paddock_row(pdk, qry)

  expect_type(test1, "integer")
  expect_true(is.vector(test1))
  expect_length(test1, 5)
  expect_false(anyNA(test1))
  expect_true(all(test1 >= 1 & test1 <= nrow(pdk)))

  # round-trip: the rows returned must map back to the exact coordinates
  # that were queried
  expect_equal(pdk[test1, x], qry[, x])
  expect_equal(pdk[test1, y], qry[, y])
})

test_that("which_paddock_row works for a single query row", {
  qry_single <- pdk[42, ]
  row_single <- which_paddock_row(pdk, qry_single)

  expect_length(row_single, 1)
  expect_equal(row_single, 42L)
})

test_that("which_paddock_row works for the first and last paddock rows", {
  corners <- pdk[c(1, .N), ]
  rows_corners <- which_paddock_row(pdk, corners)

  expect_equal(pdk[rows_corners, x], corners[, x])
  expect_equal(pdk[rows_corners, y], corners[, y])
})
