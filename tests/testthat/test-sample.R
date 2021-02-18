context("sample test")

test_that("a simple test", {
  t <- data.table(a=c(1,2,3), b=c(4,5,6))
  t[,c:=a+b]
  expect_equal(1,1)
})

test_that("The data.table assignment works",{
  dt <- DT_test()
  expect_equal(1,1)
  #test stuff
})
