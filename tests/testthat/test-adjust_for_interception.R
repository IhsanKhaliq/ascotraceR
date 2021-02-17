context("adjust coordinates for interception")

library(data.table)

# for manual testing
spore_dat <- fread("tests/testthat/test-data_adjust_for_interception.csv")

# for automatic check() testing
#spore_dat <- fread("test-data_adjust_for_interception.csv")

test1 <- adjust_for
